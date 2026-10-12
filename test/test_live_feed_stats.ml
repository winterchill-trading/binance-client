(* [Live_feed.stats] with frames pushed by a scripted local websocket server. *)

open! Core
open Async
open Binance_client

let agg_trade =
  {|{"stream":"btcusdt@aggTrade","data":{"e":"aggTrade","E":1791817544988,"a":3482328964,"s":"BTCUSDT","p":"82613.60","q":"0.005","f":8165283112,"l":8165283113,"T":1791817544843,"m":false}}|}
;;

let%expect_test "a malformed frame is counted and skipped" =
  Log.Global.set_output [];
  let%bind server = Fake_ws.start () in
  let%bind live =
    Live_feed.start (Fake_ws.config server) ~symbols:[ "BTCUSDT" ] ~streams:[ Trades ]
  in
  let module F = (val Live_feed.feed live : Market_data.Feed.S) in
  let%bind (_ : bool) =
    Fake_ws.wait_until (fun () -> not (List.is_empty (Fake_ws.received server)))
  in
  print_s [%sexp (Fake_ws.received server : string list)];
  [%expect
    {| ("{\"method\":\"SUBSCRIBE\",\"params\":[\"btcusdt@aggTrade\"],\"id\":1}") |}];
  Fake_ws.send server {|{"result":null,"id":1}|};
  Fake_ws.send server {|{"stream":"btcusdt@aggTrade","data":{"e":"aggTrade",|};
  Fake_ws.send server agg_trade;
  let%bind trade = Pipe.read F.trades in
  (match trade with
   | `Ok trade -> print_s [%sexp (trade.trade_id : string)]
   | `Eof -> print_endline "eof");
  print_s [%sexp (Live_feed.stats live : Live_feed.Stats.t)];
  [%expect {|
    3482328964
    ((frames 3) (parse_errors 1) (reconnects 0))
    |}];
  let%bind () = F.close () in
  Fake_ws.close server
;;
