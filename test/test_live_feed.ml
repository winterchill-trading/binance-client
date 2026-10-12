(* Live_feed against binance-sim on a free local port. *)

open! Core
open Async
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"

let with_sim f =
  Log.Global.set_output [];
  let server = Binance_sim.Server.create (Binance_sim.Adapt.create ()) in
  let%bind http = Binance_sim.Server.listen server ~port:0 in
  Binance_sim.Server.start_periodic server;
  let port = Binance_sim.Server.port http in
  Monitor.protect
    (fun () -> f (fun ~api_key -> Config.local ~port ~api_key ()))
    ~finally:(fun () -> Cohttp_async.Server.close http)
;;

let first reader = Pipe_read.next reader

let%expect_test "trades, bbo, l2 and funding from the simulator" =
  with_sim (fun config ->
    let raw = Market_data.Stream.Table.create () in
    let acked = Market_data.Stream.Table.create () in
    let streams = Market_data.Stream.[ Trades; Bbo; L2; Funding ] in
    List.iter streams ~f:(fun s -> Hashtbl.set acked ~key:s ~data:(Ivar.create ()));
    let on_raw ~stream ~recv_ts:_ frame =
      Hashtbl.incr raw stream;
      if String.is_substring frame ~substring:{|"result"|}
      then Ivar.fill_if_empty (Hashtbl.find_exn acked stream) ()
    in
    let%bind feed =
      Live_feed.create ~on_raw (config ~api_key:"feed") ~symbols:[ "btcusdt" ] ~streams
    in
    let module F = (val feed : Market_data.Feed.S) in
    let%bind () = Deferred.all_unit (List.map (Hashtbl.data acked) ~f:Ivar.read) in
    let order api_key side tif =
      Rest.new_order
        (config ~api_key)
        (New_order.Request.limit
           btc
           side
           ~time_in_force:tif
           ~price:(Price.of_string "100000")
           ~quantity:(Size.of_string "0.01"))
      >>| ok_exn
      >>| ignore
    in
    let%bind () = order "alice" Sell Gtc in
    let%bind () = order "bob" Buy Ioc in
    let%bind trade = first F.trades in
    print_s
      [%sexp
        (Option.map trade ~f:(fun t -> t.instrument, t.price, t.size, t.side)
         : (Market_data.Instrument.t * Price.t * Size.t * Side.t) option)];
    [%expect {| ((((venue Binance) (symbol BTCUSDT)) 100000 0.01 Buy)) |}];
    let%bind bbo = first F.bbo in
    print_s [%sexp (Option.is_some bbo : bool)];
    let%bind l2 = first F.l2 in
    print_s
      [%sexp (Option.map l2 ~f:(fun u -> u.kind) : Market_data.L2_update.Kind.t option)];
    let%bind funding = first F.funding in
    print_s [%sexp (Option.map funding ~f:(fun f -> f.mark_price) : Price.t option)];
    [%expect {|
      true
      (Snapshot)
      (100000)
      |}];
    print_s
      [%sexp
        (List.map streams ~f:(fun s -> s, Hashtbl.find raw s |> Option.is_some)
         : (Market_data.Stream.t * bool) list)];
    [%expect {| ((Trades true) (Bbo true) (L2 true) (Funding true)) |}];
    let%bind () = F.close () in
    print_s
      [%sexp
        ([ Pipe.is_closed F.trades
         ; Pipe.is_closed F.bbo
         ; Pipe.is_closed F.l2
         ; Pipe.is_closed F.funding
         ]
         : bool list)];
    [%expect {| (true true true true) |}];
    return ())
;;
