open! Core
open Async
open Binance_protocol
module Ws_client = Venue_net.Ws_client

type message =
  | Event of Market_event.t Combined.t
  | Response of Subscribe.Response.t
[@@deriving sexp_of]

let decode frame =
  Or_error.try_with (fun () ->
    let json = Yojson.Safe.from_string frame in
    if Subscribe.Response.is_response json
    then Response (Subscribe.Response.t_of_yojson json)
    else Event (Combined.t_of_yojson Market_event.t_of_yojson json))
;;

let encode request = Yojson.Safe.to_string (Subscribe.Request.yojson_of_t request)

module Stream_set = struct
  (* Keyed by wire name, insertion-independent. *)
  type t = Stream_name.t String.Map.t

  let add t s = Map.set t ~key:(Stream_name.to_string s) ~data:s
  let remove t s = Map.remove t (Stream_name.to_string s)
  let mem t s = Map.mem t (Stream_name.to_string s)
end

type t =
  { client : Ws_client.t
  ; mutable streams : Stream_set.t
  ; mutable next_id : int
  ; pending : unit Or_error.t Ivar.t Int.Table.t
  ; events_r : Market_event.t Combined.t Pipe.Reader.t
  ; events_w : Market_event.t Combined.t Pipe.Writer.t
  }

let events t = t.events_r
let subscriptions t = Map.data t.streams
let is_connected t = Ws_client.is_connected t.client

let on_frame t frame =
  match decode frame with
  | Ok (Event event) -> Pipe.write_without_pushback_if_open t.events_w event
  | Ok (Response response) ->
    let id, result =
      match response with
      | Result { id; result = _ } -> Some id, Ok ()
      | Error { code; msg; id } ->
        Log.Global.error_s [%message "binance market ws: request failed" (code : int) msg];
        id, Or_error.error_s [%message "subscription request failed" (code : int) msg]
    in
    Option.iter id ~f:(fun id ->
      Option.iter (Hashtbl.find_and_remove t.pending id) ~f:(fun ivar ->
        Ivar.fill_if_empty ivar result))
  | Error error ->
    Log.Global.debug_s
      [%message "binance market ws: undecodable frame" (frame : string) (error : Error.t)]
;;

let fresh_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

(* Binance accepts many streams per request; keep requests small anyway. *)
let chunks streams = List.chunks_of streams ~length:50

let request t ~timeout method_ streams =
  Deferred.Or_error.List.iter (chunks streams) ~how:`Sequential ~f:(fun params ->
    if not (Ws_client.is_connected t.client)
    then Deferred.Or_error.ok_unit
    else (
      let id = fresh_id t in
      let ivar = Ivar.create () in
      Hashtbl.set t.pending ~key:id ~data:ivar;
      let%bind () = Ws_client.send t.client (encode { method_; params; id }) in
      match%map Clock_ns.with_timeout timeout (Ivar.read ivar) with
      | `Result result -> result
      | `Timeout ->
        Hashtbl.remove t.pending id;
        Or_error.error_s
          [%message
            "no response to subscription request"
              (method_ : Subscribe.Method.t)
              (params : Stream_name.t list)]))
;;

let default_timeout = Time_ns.Span.of_int_sec 10

let subscribe ?(timeout = default_timeout) t streams =
  let fresh =
    List.dedup_and_sort streams ~compare:Stream_name.compare
    |> List.filter ~f:(fun s -> not (Stream_set.mem t.streams s))
  in
  t.streams <- List.fold fresh ~init:t.streams ~f:Stream_set.add;
  if List.is_empty fresh
  then Deferred.Or_error.ok_unit
  else request t ~timeout Subscribe fresh
;;

let unsubscribe ?(timeout = default_timeout) t streams =
  let active = List.filter streams ~f:(Stream_set.mem t.streams) in
  t.streams <- List.fold active ~init:t.streams ~f:Stream_set.remove;
  if List.is_empty active
  then Deferred.Or_error.ok_unit
  else request t ~timeout Unsubscribe active
;;

let connect
  ?(route = Config.Route.Public)
  ?(ping = `Frame (Time_ns.Span.of_int_sec 30))
  ?(max_silent_intervals = 2)
  ?initial_backoff
  ?max_backoff
  ?(on_reconnect = fun () -> ())
  config
  =
  let t_ref = ref None in
  let on_connect client =
    match !t_ref with
    | None -> return ()
    | Some t ->
      (* Resubscriptions go out before anything [on_reconnect] sends. The ids are not
         awaited: responses for unknown ids are ignored. *)
      let%map () =
        Deferred.List.iter
          (chunks (subscriptions t))
          ~how:`Sequential
          ~f:(fun params ->
            Ws_client.send
              client
              (encode { method_ = Subscribe; params; id = fresh_id t }))
      in
      if Ws_client.reconnects client > 0 then on_reconnect ()
  in
  let%map.Deferred.Or_error client =
    Ws_client.connect
      ~ping
      ~max_silent_intervals
      ?initial_backoff
      ?max_backoff
      ~on_connect
      ~log_name:"binance market ws"
      ~url:(Config.combined_stream_uri config ~route)
      ()
  in
  let events_r, events_w = Pipe.create () in
  let t =
    { client
    ; streams = String.Map.empty
    ; next_id = 1
    ; pending = Int.Table.create ()
    ; events_r
    ; events_w
    }
  in
  t_ref := Some t;
  don't_wait_for
    (let%map () =
       Pipe.iter_without_pushback (Ws_client.messages client) ~f:(on_frame t)
     in
     Pipe.close t.events_w);
  t
;;

let close t =
  let%map () = Ws_client.close t.client in
  Hashtbl.iter t.pending ~f:(fun ivar ->
    Ivar.fill_if_empty ivar (Or_error.error_string "market websocket closed"));
  Hashtbl.clear t.pending;
  Pipe.close t.events_w
;;

module For_testing = struct
  type nonrec message = message =
    | Event of Market_event.t Combined.t
    | Response of Subscribe.Response.t
  [@@deriving sexp_of]

  let decode = decode
  let encode = encode
end
