(* A key-less config on the public endpoints of binance-sim. Signed endpoints do not
   accept it at all: see must_fail/. *)

open! Core
open Async
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"

let%expect_test "a read-only config reaches market data" =
  Log.Global.set_output [];
  print_s [%sexp (Config.Read_only.mainnet () : Config.Read_only.t)];
  [%expect
    {|
    ((base_url https://fapi.binance.com) (ws_url wss://fstream.binance.com)
     (routed_ws true) (keys ()))
    |}];
  let server = Binance_sim.Server.create (Binance_sim.Adapt.create ()) in
  let%bind http = Binance_sim.Server.listen server ~port:0 in
  let config = Config.Read_only.local ~port:(Binance_sim.Server.port http) () in
  let ok name result = printf "%s: %b\n" name (Or_error.is_ok result) in
  let%bind () = Rest.exchange_info config >>| ok "exchange_info" in
  let%bind () = Rest.depth ~limit:5 config btc >>| ok "depth" in
  let%bind () = Rest.book_ticker config btc >>| ok "book_ticker" in
  let%bind () = Rest.premium_index config btc >>| ok "premium_index" in
  let%bind market = Market_ws.connect config >>| ok_exn in
  let%bind () = Market_ws.subscribe market [ Book_ticker btc ] >>| ok "subscribe" in
  let%bind () = Market_ws.close market in
  let%bind feed = Live_feed.create config ~symbols:[ "BTCUSDT" ] ~streams:[ Trades ] in
  let module F = (val feed : Market_data.Feed.S) in
  let%bind () = F.close () in
  [%expect
    {|
    exchange_info: true
    depth: true
    book_ticker: true
    premium_index: true
    subscribe: true
    |}];
  Cohttp_async.Server.close http
;;
