(* The mm-core live loop quoting on binance-sim, with noise traders, through Venue. *)

open! Core
open Async
open Binance_protocol
open Binance_client

let symbol = Symbol.of_string "BTCUSDT"

(* The race this avoids: with the market simulator ticking on its own 100ms timer, noise
   flow could move the touch after the strategy priced a post-only quote from its last
   book (depth is flushed every 100ms) and before the order reached the simulator. The
   quote then crossed and came back as "post-only order would cross, bbo none@none", more
   often the slower the machine. Here the market only moves inside the loop's account
   snapshot, which runs on the loop's sequencer between plans, and the snapshot returns
   only once the loop has consumed a book whose touch matches the simulator's. *)
module Lockstep : sig
  include Mm_core.Venue.S with type t = Venue.t and type config = Config.t

  (** Moves the market one tick per account snapshot from now on. *)
  val drive
    :  Binance_sim.Server.t
    -> Exchange_core_engine.Market_sim.t
    -> Exchange_core_engine.Exchange.t
    -> unit
end = struct
  include Venue

  type driver =
    { server : Binance_sim.Server.t
    ; sim : Exchange_core_engine.Market_sim.t
    ; exchange : Exchange_core_engine.Exchange.t
    }

  let driver = ref None
  let drive server sim exchange = driver := Some { server; sim; exchange }

  (* The reader handed to the loop by the latest [books] call, and the last book it took. *)
  let reader = ref None
  let last_book = ref None

  let books t symbol =
    let%map books = Venue.books t symbol in
    let books =
      Pipe.map books ~f:(fun l2 ->
        last_book := Some l2;
        l2)
    in
    reader := Some books;
    books
  ;;

  let touch (l2 : Exchange_core.L2.t option) =
    let level (l : Exchange_core.L2.Level.t option) =
      Option.map l ~f:(fun (l : Exchange_core.L2.Level.t) -> l.px, l.sz)
    in
    Option.map l2 ~f:(fun l2 ->
      level (Exchange_core.L2.best_bid l2), level (Exchange_core.L2.best_ask l2))
  ;;

  let caught_up { exchange; _ } books =
    Pipe.is_empty books
    && [%equal:
         ((Exchange_core.Price.t * Exchange_core.Size.t) option
         * (Exchange_core.Price.t * Exchange_core.Size.t) option)
           option]
         (touch !last_book)
         (touch (Exchange_core_engine.Exchange.l2 exchange symbol))
  ;;

  let wait_until_caught_up driver books =
    let deadline = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec 10) in
    let rec poll () =
      if caught_up driver books
      then return ()
      else if Time_ns.( > ) (Time_ns.now ()) deadline
      then raise_s [%message "the loop's book never caught up with the simulator"]
      else (
        let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 1) in
        poll ())
    in
    poll ()
  ;;

  let account t =
    let%bind result = Venue.account t in
    match !driver, !reader with
    | Some driver, Some books when not (Pipe.is_closed books) ->
      Binance_sim.Server.mutate driver.server ~f:(fun _ ->
        Exchange_core_engine.Market_sim.tick driver.sim);
      let%map () = wait_until_caught_up driver books in
      result
    | _ -> return result
  ;;
end

module Live = Mm_core.Live.Make (Lockstep)

let%expect_test "the market maker runs on the simulator" =
  Log.Global.set_output [];
  let adapt = Binance_sim.Adapt.create () in
  let exchange = Binance_sim.Adapt.exchange adapt in
  let server = Binance_sim.Server.create adapt in
  let%bind http = Binance_sim.Server.listen server ~port:0 in
  Binance_sim.Server.start_periodic server;
  let sim =
    Exchange_core_engine.Market_sim.create
      { Exchange_core_engine.Market_sim.Config.default with symbols = [ symbol ] }
      exchange
  in
  (* Noise flow until the book is two-sided, so the loop can find its first mid. *)
  let rec warm_up n =
    match Exchange_core_engine.Exchange.l2 exchange symbol with
    | Some l2
      when Option.is_some (Exchange_core.L2.best_bid l2)
           && Option.is_some (Exchange_core.L2.best_ask l2) -> ()
    | _ ->
      if n = 0 then raise_s [%message "no two-sided book after warm-up"];
      Binance_sim.Server.mutate server ~f:(fun _ ->
        Exchange_core_engine.Market_sim.tick sim);
      warm_up (n - 1)
  in
  warm_up 100;
  Lockstep.drive server sim exchange;
  let config = Config.local ~port:(Binance_sim.Server.port http) ~api_key:"mm" () in
  let%bind report =
    Live.connect_and_run
      config
      ~symbol
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
