(* The mm-core live loop quoting on binance-sim, with noise traders, through Venue. *)

open! Core
open Async
open Binance_protocol
open Binance_client
module Live = Mm_core.Live.Make (Venue)

let%expect_test "the market maker runs on the simulator" =
  Log.Global.set_output [];
  let adapt = Binance_sim.Adapt.create () in
  let server = Binance_sim.Server.create adapt in
  let%bind http = Binance_sim.Server.listen server ~port:0 in
  Binance_sim.Server.start_periodic server;
  Binance_sim.Server.run_market_sim
    server
    (Exchange_core_engine.Market_sim.create
       { Exchange_core_engine.Market_sim.Config.default with
         symbols = [ Symbol.of_string "BTCUSDT" ]
       }
       (Binance_sim.Adapt.exchange adapt));
  let config = Config.local ~port:(Binance_sim.Server.port http) ~api_key:"mm" () in
  let%bind report =
    Live.connect_and_run
      config
      ~symbol:(Symbol.of_string "BTCUSDT")
      ~params:(fun instrument ~mid ->
        (* Binance's BTCUSDT minimum notional is 100 USD, which the default ~100 USD order
           size misses on bids below 100k. *)
        Mm_core.Params.validate
          (Mm_core.Params.for_instrument
             instrument
             ~mid
             ~order_size:(Size.of_string "0.002")))
      ~summary_interval:(Time_ns.Span.of_int_sec 60)
      ~stop:(Clock_ns.after (Time_ns.Span.of_int_sec 4))
  in
  let report = ok_exn report in
  (* Runs are not repeatable (request timing changes matching); check ranges. *)
  print_s
    [%message
      ""
        ~planned:(report.plans > 0 : bool)
        ~placed:(report.orders_placed > 0 : bool)
        ~rejections:(report.rejections : (string * int) list)
        ~mode:(report.mode : Mm_core.Strategy.Mode.t)
        ~has_mid:(Option.is_some report.mid : bool)
        ~has_account_value:(Option.is_some report.account_value : bool)];
  let%bind () = Cohttp_async.Server.close http in
  [%expect
    {|
    ((planned true) (placed true) (rejections ()) (mode Running) (has_mid true)
     (has_account_value true))
    |}];
  return ()
;;
