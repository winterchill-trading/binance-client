(* [Market_ws] takes frames through [on_message]. Frames that arrive with the end of the
   upgrade handshake can be read before [connect] has built its state; they are queued and
   must still come out of [events] first, in order. *)

open! Core
open Async
open Binance_client
open Binance_client_blast

let agg_trade id =
  sprintf
    {|{"stream":"btcusdt@aggTrade","data":{"e":"aggTrade","E":1,"a":%d,"s":"BTCUSDT","p":"1.0","q":"1","f":1,"l":1,"T":1,"m":false}}|}
    id
;;

let%expect_test "frames sent with the upgrade response come out first and in order" =
  Log.Global.set_output [];
  let%bind server =
    Blast_server.start
      ~greeting:(List.map [ 1; 2; 3 ] ~f:agg_trade)
      ~frame:(agg_trade 4)
      ~per_write:1
      ()
  in
  let config =
    Config.Read_only.create
      ~base_url:(Uri.of_string "http://127.0.0.1:1")
      ~ws_url:(Uri.of_string (sprintf "ws://127.0.0.1:%d" (Blast_server.port server)))
      ()
  in
  let%bind ws = Market_ws.connect config >>| ok_exn in
  (* Any frame from the client makes the server ready; the request itself times out. *)
  don't_wait_for
    (Market_ws.subscribe
       ~timeout:(Time_ns.Span.of_int_ms 1)
       ws
       [ Binance_protocol.Stream_name.of_string "btcusdt@aggTrade" ]
     |> Deferred.ignore_m);
  let%bind () = Blast_server.ready server in
  let%bind (_ : int) = Blast_server.blast server ~n:1 in
  let%bind events = Pipe.read_exactly (Market_ws.events ws) ~num_values:4 in
  (match events with
   | `Exactly events ->
     Queue.iter events ~f:(fun (event : _ Binance_protocol.Combined.t) ->
       match event.data with
       | Agg_trade trade -> printf "%d " trade.agg_trade_id
       | _ -> print_string "other ")
   | `Eof | `Fewer _ -> print_endline "missing events");
  [%expect {| 1 2 3 4 |}];
  let%bind () = Market_ws.close ws in
  Blast_server.close server
;;
