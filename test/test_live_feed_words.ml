(* Minor words per frame that [Live_feed] and [Market_ws] add on top of the websocket
   transport: frames read through each, minus the same frames read by a bare [Ws_client]
   that only counts them. Both sides pay the socket reads and async_websocket's framing,
   which differ a little between macOS arm64 and Linux amd64; the difference is the
   client's own work (the frame hand-off, decoding and the output pipe). *)

open! Core
open Async
open Binance_client_blast

let n = 20_000

let mean results =
  List.sum (module Float) results ~f:(fun (r : Feed_cost.result) -> r.words)
  /. Float.of_int (List.length results)
;;

let%expect_test "words per frame added by the live feed and the market websocket" =
  Log.Global.set_output [];
  let%bind server = Blast_server.start ~frame:Feed_cost.frame ~per_write:1_000 () in
  let%bind floor = Feed_cost.floor server ~n ~runs:3 in
  let%bind feed = Feed_cost.live_feed server ~n ~runs:3 in
  let%bind market_ws = Feed_cost.market_ws server ~n ~runs:3 in
  let over_floor results = Float.iround_nearest_exn (mean results -. mean floor) in
  print_s
    [%message
      "words per frame over the floor"
        ~live_feed:(over_floor feed : int)
        ~market_ws:(over_floor market_ws : int)];
  [%expect {| ("words per frame over the floor" (live_feed 38) (market_ws 429)) |}];
  Blast_server.close server
;;
