open! Core
open Async
open Binance_protocol
module Ws_client = Venue_net.Ws_client

let decode frame =
  Or_error.try_with (fun () -> User_event.t_of_yojson (Yojson.Safe.from_string frame))
;;

type options =
  { ping : Ws_client.Ping.t
  ; max_silent_intervals : int
  ; initial_backoff : Time_ns.Span.t option
  ; max_backoff : Time_ns.Span.t option
  ; on_reconnect : unit -> unit
  }

type t =
  { config : Config.t
  ; options : options
  ; mutable listen_key : string
  ; mutable client : Ws_client.t
  ; mutable renewing : unit Or_error.t Deferred.t option
  ; mutable closed : bool
  ; events_r : User_event.t Pipe.Reader.t
  ; events_w : User_event.t Pipe.Writer.t
  }

let events t = t.events_r
let listen_key t = t.listen_key
let is_connected t = Ws_client.is_connected t.client

let open_socket config options ~listen_key =
  Ws_client.connect
    ~ping:options.ping
    ~max_silent_intervals:options.max_silent_intervals
    ?initial_backoff:options.initial_backoff
    ?max_backoff:options.max_backoff
    ~on_connect:(fun client ->
      if Ws_client.reconnects client > 0 then options.on_reconnect ();
      return ())
    ~log_name:"binance user ws"
    ~url:(Config.user_stream_uri config ~listen_key)
    ()
;;

let rec pump t client =
  don't_wait_for
    (Pipe.iter_without_pushback (Ws_client.messages client) ~f:(fun frame ->
       match decode frame with
       | Ok event ->
         Pipe.write_without_pushback_if_open t.events_w event;
         (match event with
          | Listen_key_expired { listen_key; _ } when String.equal listen_key t.listen_key
            ->
            Log.Global.info "binance user ws: listen key expired, renewing";
            don't_wait_for (Deferred.ignore_m (renew t))
          | _ -> ())
       | Error error ->
         Log.Global.debug_s
           [%message
             "binance user ws: undecodable frame" (frame : string) (error : Error.t)]))

and renew t =
  match t.renewing with
  | Some renewing -> renewing
  | None ->
    let renewing =
      let open Deferred.Or_error.Let_syntax in
      let%bind listen_key = Rest.create_listen_key t.config in
      let%bind client = open_socket t.config t.options ~listen_key in
      let old = t.client in
      if t.closed
      then Deferred.ok (Ws_client.close client)
      else (
        t.listen_key <- listen_key;
        t.client <- client;
        pump t client;
        let%map () = Deferred.ok (Ws_client.close old) in
        t.options.on_reconnect ())
    in
    t.renewing <- Some renewing;
    let%map result = renewing in
    t.renewing <- None;
    result
;;

let keepalive t =
  match%bind Rest.keepalive_listen_key t.config with
  | Ok () -> return ()
  | Error error ->
    (match Rest.api_error error with
     | Some { code = Invalid_listen_key; _ } ->
       Log.Global.info "binance user ws: listen key unknown to the server, renewing";
       (match%map renew t with
        | Ok () -> ()
        | Error error ->
          Log.Global.error_s
            [%message "binance user ws: listen key renewal failed" (error : Error.t)])
     | _ ->
       Log.Global.error_s [%message "binance user ws: keepalive failed" (error : Error.t)];
       return ())
;;

let connect
  ?(keepalive_interval = Time_ns.Span.of_int_min 30)
  ?(ping = `Frame (Time_ns.Span.of_int_sec 30))
  ?(max_silent_intervals = 2)
  ?initial_backoff
  ?max_backoff
  ?(on_reconnect = fun () -> ())
  config
  =
  let options =
    { ping; max_silent_intervals; initial_backoff; max_backoff; on_reconnect }
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind listen_key = Rest.create_listen_key config in
  let%map client = open_socket config options ~listen_key in
  let events_r, events_w = Pipe.create () in
  let t =
    { config
    ; options
    ; listen_key
    ; client
    ; renewing = None
    ; closed = false
    ; events_r
    ; events_w
    }
  in
  pump t client;
  Clock_ns.every'
    ~start:(Clock_ns.after keepalive_interval)
    ~stop:(Pipe.closed events_w)
    keepalive_interval
    (fun () -> keepalive t);
  t
;;

let close ?(close_key = false) t =
  if t.closed
  then return ()
  else (
    t.closed <- true;
    Pipe.close t.events_w;
    let%bind () = Ws_client.close t.client in
    if close_key
    then (
      match%map Rest.close_listen_key t.config with
      | Ok () -> ()
      | Error error ->
        Log.Global.debug_s
          [%message "binance user ws: closing the listen key failed" (error : Error.t)])
    else return ())
;;

module For_testing = struct
  let decode = decode
end
