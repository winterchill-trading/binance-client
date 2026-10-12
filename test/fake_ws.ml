open! Core
open Async

type t =
  { tcp : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; mutable connections : int
  ; received : string Queue.t
  ; sockets : string Pipe.Writer.t Stack.t
  ; mutable websockets : string Websocket.t list
  ; mutable depth : string option
  ; mutable depth_requests : int
  }

let rec read_headers reader acc =
  match%bind Reader.read_line reader with
  | `Eof | `Ok "" -> return (List.rev acc)
  | `Ok line ->
    read_headers
      reader
      (match String.lsplit2 line ~on:':' with
       | Some (k, v) -> (String.lowercase k, String.strip v) :: acc
       | None -> acc)
;;

let respond_depth t writer =
  t.depth_requests <- t.depth_requests + 1;
  let status, body =
    match t.depth with
    | Some body -> "200 OK", body
    | None -> "404 Not Found", {|{"code":-1,"msg":"no depth"}|}
  in
  Writer.write
    writer
    (sprintf
       "HTTP/1.1 %s\r\n\
        Content-Type: application/json\r\n\
        Content-Length: %d\r\n\
        Connection: close\r\n\
        \r\n\
        %s"
       status
       (String.length body)
       body);
  Writer.close writer
;;

let handle t reader writer =
  let%bind request_line = Reader.read_line reader in
  let%bind headers = read_headers reader [] in
  match request_line with
  | `Ok line when String.is_prefix line ~prefix:"GET /fapi/v1/depth" ->
    respond_depth t writer
  | `Ok _ | `Eof ->
    let key = List.Assoc.find_exn headers ~equal:String.equal "sec-websocket-key" in
    Writer.write
      writer
      (sprintf
         "HTTP/1.1 101 Switching Protocols\r\n\
          Upgrade: websocket\r\n\
          Connection: Upgrade\r\n\
          Sec-WebSocket-Accept: %s\r\n\
          \r\n"
         (Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key));
    let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
    t.connections <- t.connections + 1;
    t.websockets <- ws :: t.websockets;
    let from_client, to_client = Websocket.pipes ws in
    Stack.push t.sockets to_client;
    Pipe.iter_without_pushback from_client ~f:(Queue.enqueue t.received)
;;

let start () =
  let t_ref = ref None in
  let%map tcp =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
      (fun _ reader writer -> handle (Option.value_exn !t_ref) reader writer)
  in
  let t =
    { tcp
    ; connections = 0
    ; received = Queue.create ()
    ; sockets = Stack.create ()
    ; websockets = []
    ; depth = None
    ; depth_requests = 0
    }
  in
  t_ref := Some t;
  t
;;

let config t =
  let port = Tcp.Server.listening_on t.tcp in
  Binance_client.Config.create
    ~base_url:(Uri.of_string (sprintf "http://127.0.0.1:%d" port))
    ~api_key:"test"
    ()
;;

let connections t = t.connections
let set_depth t body = t.depth <- Some body
let depth_requests t = t.depth_requests
let received t = Queue.to_list t.received

let send t frame =
  match Stack.top t.sockets with
  | Some w -> Pipe.write_without_pushback_if_open w frame
  | None -> raise_s [%message "Fake_ws.send: no connection"]
;;

let drop t =
  match t.websockets with
  | ws :: _ -> Websocket.close ws ~code:Endpoint_going_away ~reason:"bye"
  | [] -> return ()
;;

let wait_until ?(timeout = Time_ns.Span.of_int_sec 5) f =
  let deadline = Time_ns.add (Time_ns.now ()) timeout in
  Deferred.repeat_until_finished () (fun () ->
    if f ()
    then return (`Finished true)
    else if Time_ns.( > ) (Time_ns.now ()) deadline
    then return (`Finished false)
    else (
      let%map () = Clock_ns.after (Time_ns.Span.of_int_ms 1) in
      `Repeat ()))
;;

let close t = Tcp.Server.close ~close_existing_connections:true t.tcp
