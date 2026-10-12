(* Rest, Market_ws, User_ws and Venue against binance-sim on a free local port. *)

open! Core
open Async
open Binance_protocol
open Binance_client
module Neutral = Exchange_core

let btc = Symbol.of_string "BTCUSDT"
let px = Price.of_string
let sz = Size.of_string

let with_sim f =
  Log.Global.set_output [];
  let server = Binance_sim.Server.create (Binance_sim.Adapt.create ()) in
  let%bind http = Binance_sim.Server.listen server ~port:0 in
  let port = Binance_sim.Server.port http in
  let config ~api_key = Config.local ~port ~api_key () in
  Monitor.protect (fun () -> f config) ~finally:(fun () -> Cohttp_async.Server.close http)
;;

let read_until ?timeout reader ~f = Pipe_read.until ?timeout reader ~f

let limit ?(tif = Neutral.Tif.Gtc) side ~price ~size =
  Neutral.Order_request.limit ~tif btc side ~px:(px price) ~size:(sz size)
;;

let show_statuses statuses =
  List.iter statuses ~f:(fun (status : Neutral.Order_status.t) ->
    match status with
    | Resting _ -> print_endline "resting"
    | Filled { total; avg_px; _ } ->
      printf "filled %s @ %s\n" (Size.to_string total) (Price.to_string avg_px)
    | Rejected reason -> print_s [%message "rejected" (reason : Neutral.Reject_reason.t)])
;;

let show_open_orders venue =
  let%map orders = Venue.open_orders venue btc >>| ok_exn in
  List.iter orders ~f:(fun (o : Neutral.Open_order.t) ->
    printf
      "open %s %s @ %s\n"
      (Side.to_wire o.side)
      (Size.to_string o.size)
      (Price.to_string o.px))
;;

let%expect_test "REST against the simulator" =
  with_sim (fun config ->
    let config = config ~api_key:"maker" in
    let%bind info = Rest.exchange_info config >>| ok_exn in
    print_s [%sexp (List.map info.symbols ~f:(fun s -> s.symbol) : Symbol.t list)];
    let%bind order =
      Rest.new_order
        config
        (New_order.Request.limit btc Sell ~price:(px "101000") ~quantity:(sz "0.01"))
      >>| ok_exn
    in
    print_s
      [%sexp (order.status : Order_status.t), (order.time_in_force : Time_in_force.t)];
    let%bind depth = Rest.depth ~limit:5 config btc >>| ok_exn in
    print_s [%sexp (depth.asks : Book_level.t list)];
    let%bind ticker = Rest.book_ticker config btc >>| ok_exn in
    print_s [%sexp (ticker.ask_price : Price.t)];
    (* A post-only bid at the ask is rejected with Binance's code. *)
    let%bind rejected =
      Rest.new_order
        config
        (New_order.Request.limit
           btc
           Buy
           ~time_in_force:Gtx
           ~price:(px "101000")
           ~quantity:(sz "0.01"))
    in
    print_s
      [%sexp (Option.bind (Result.error rejected) ~f:Rest.api_error : Api_error.t option)];
    let%bind open_orders = Rest.open_orders ~symbol:btc config >>| ok_exn in
    print_s [%sexp (List.length open_orders : int)];
    let%bind cancelled =
      Rest.cancel_order config { symbol = btc; target = Order_id order.order_id }
      >>| ok_exn
    in
    print_s [%sexp (cancelled.status : Order_status.t)];
    let%bind () = Rest.cancel_all config btc >>| ok_exn in
    let%bind account = Rest.account config >>| ok_exn in
    print_s [%sexp (account.total_wallet_balance : Usd.t)];
    let%bind risks = Rest.position_risk ~symbol:btc config >>| ok_exn in
    print_s [%sexp (List.map risks ~f:(fun r -> r.leverage) : int option list)];
    let%bind key = Rest.create_listen_key config >>| ok_exn in
    print_s [%sexp (String.is_empty key : bool)];
    let%bind () = Rest.keepalive_listen_key config >>| ok_exn in
    let%bind () = Rest.close_listen_key config >>| ok_exn in
    let%bind gone = Rest.keepalive_listen_key config in
    print_s
      [%sexp (Option.bind (Result.error gone) ~f:Rest.api_error : Api_error.t option)];
    [%expect
      {|
      (BTCUSDT ETHUSDT SOLUSDT)
      (New Gtc)
      (((price 101000) (qty 0.01)))
      101000
      (((code Gtx_order_reject)
        (msg
         "Due to the order could not be executed as maker, the Post Only order will be rejected. The order will not be recorded in the order history")))
      1
      Canceled
      1000000
      ((20))
      false
      (((code Invalid_listen_key) (msg "This listenKey does not exist.")))
      |}];
    return ())
;;

let%expect_test "venue: place, open orders, cancel, fills, books and account" =
  with_sim (fun config ->
    let maker = config ~api_key:"maker" in
    let%bind venue = Venue.connect (config ~api_key:"mm") >>| ok_exn in
    let%bind instrument = Venue.instrument venue btc >>| ok_exn in
    print_s [%sexp (instrument : Neutral.Instrument.t)];
    let%bind books = Venue.books venue btc in
    (* Liquidity from another account. *)
    let%bind () =
      Deferred.List.iter
        ~how:`Sequential
        [ Side.Sell, "101000"; Buy, "99000" ]
        ~f:(fun (side, price) ->
          Rest.new_order
            maker
            (New_order.Request.limit btc side ~price:(px price) ~quantity:(sz "0.05"))
          >>| ok_exn
          >>| ignore)
    in
    let%bind book =
      read_until books ~f:(fun (l2 : Neutral.L2.t) ->
        (not (List.is_empty l2.bids)) && not (List.is_empty l2.asks))
    in
    print_s
      [%sexp
        (Option.map book ~f:(fun l2 -> Neutral.L2.best_bid l2, Neutral.L2.best_ask l2)
         : (Neutral.L2.Level.t option * Neutral.L2.Level.t option) option)];
    (* A post-only bid inside the spread rests; one at the ask is rejected. *)
    let%bind statuses =
      Venue.place
        venue
        [ limit ~tif:Alo Buy ~price:"99500" ~size:"0.01"
        ; limit ~tif:Alo Buy ~price:"101000" ~size:"0.01"
        ]
      >>| ok_exn
    in
    show_statuses statuses;
    let%bind () = show_open_orders venue in
    let updates = Venue.order_updates venue in
    let%bind update = read_until updates ~f:(fun _ -> true) in
    print_s
      [%sexp
        (Option.map update ~f:(fun u -> u.status, u.px, u.remaining)
         : (Mm_core.Order_update.Status.t * Price.t * Size.t) option)];
    let resting_id =
      List.find_map_exn statuses ~f:(function
        | Resting { order_id; _ } -> Some order_id
        | _ -> None)
    in
    let%bind () = Venue.cancel venue btc [ resting_id ] >>| ok_exn in
    (* Cancelling it again is not an error. *)
    let%bind () = Venue.cancel venue btc [ resting_id ] >>| ok_exn in
    let%bind update = read_until updates ~f:(fun _ -> true) in
    print_s
      [%sexp
        (Option.map update ~f:(fun u -> u.status, u.remaining)
         : (Mm_core.Order_update.Status.t * Size.t) option)];
    let%bind () = show_open_orders venue in
    [%expect
      {|
      ((symbol BTCUSDT) (size_decimals 3) (price_tick 0.1)
       (max_significant_figures ()) (max_leverage 20)
       (fees ((taker_bps 5) (maker_bps 2))))
      (((((px 99000) (sz 0.05) (n 0))) (((px 101000) (sz 0.05) (n 0)))))
      resting
      (rejected (reason (Post_only_would_cross (best_bid ()) (best_ask ()))))
      open BUY 0.01 @ 99500
      ((New 99500 0.01))
      ((Cancelled 0))
      |}];
    (* Crossing the ask fills. *)
    let%bind statuses =
      Venue.place venue [ limit ~tif:Ioc Buy ~price:"101000" ~size:"0.02" ] >>| ok_exn
    in
    show_statuses statuses;
    let%bind fill = read_until (Venue.fills venue) ~f:(fun _ -> true) in
    print_s
      [%sexp
        (Option.map fill ~f:(fun f -> f.side, f.px, f.size, f.fee, f.is_maker)
         : (Side.t * Price.t * Size.t * Usd.t * bool) option)];
    let%bind account = Venue.account venue >>| ok_exn in
    print_s
      [%sexp
        { balance : Usd.t = account.balance
        ; equity : Usd.t = account.equity
        ; positions =
            (List.map account.positions ~f:(fun p ->
               p.symbol, p.size, p.entry_px, p.leverage)
             : (Symbol.t * Size.t * Price.t * int) list)
        ; available_positive : bool = Usd.is_positive account.available
        }];
    [%expect
      {|
      filled 0.02 @ 101000
      ((Buy 101000 0.02 1.01 false))
      ((balance 999998.99) (equity 999998.99)
       (positions ((BTCUSDT 0.02 101000 20))) (available_positive true))
      |}];
    (* Batches of more than one order go through batchOrders. *)
    let%bind statuses =
      Venue.place
        venue
        [ limit ~tif:Alo Sell ~price:"102000" ~size:"0.01"
        ; limit ~tif:Alo Sell ~price:"103000" ~size:"0.01"
        ; limit Sell ~price:"103000" ~size:"0.0001"
        ]
      >>| ok_exn
    in
    show_statuses statuses;
    let%bind () = show_open_orders venue in
    let%bind () = Venue.cancel_all venue btc >>| ok_exn in
    let%bind () = show_open_orders venue in
    [%expect
      {|
      resting
      resting
      (rejected (reason Invalid_size))
      open SELL 0.01 @ 102000
      open SELL 0.01 @ 103000
      |}];
    Venue.close venue)
;;

let%expect_test "market and user streams" =
  with_sim (fun config ->
    let alice = config ~api_key:"alice" in
    let bob = config ~api_key:"bob" in
    let%bind market = Market_ws.connect alice >>| ok_exn in
    let%bind user = User_ws.connect alice >>| ok_exn in
    let streams =
      [ Stream_name.Book_ticker btc
      ; Agg_trade btc
      ; Diff_depth { symbol = btc; speed = Some Ms_100 }
      ]
    in
    let%bind () = Market_ws.subscribe market streams >>| ok_exn in
    (* Already active: nothing is sent. *)
    let%bind () = Market_ws.subscribe market [ Book_ticker btc ] >>| ok_exn in
    print_s [%sexp (Market_ws.subscriptions market : Stream_name.t list)];
    let order client side tif =
      Rest.new_order
        client
        (New_order.Request.limit
           btc
           side
           ~time_in_force:tif
           ~price:(px "100000")
           ~quantity:(sz "0.01"))
      >>| ok_exn
      >>| ignore
    in
    let%bind () = order alice Sell Gtc in
    let%bind () = order bob Buy Ioc in
    let events = Market_ws.events market in
    let kinds = String.Hash_set.create () in
    let%bind (_ : _ option) =
      read_until events ~f:(fun ({ data; _ } : Market_event.t Combined.t) ->
        (match data with
         | Book_ticker _ -> Hash_set.add kinds "bookTicker"
         | Agg_trade trade ->
           Hash_set.add kinds "aggTrade";
           print_s
             [%message
               "trade"
                 ~price:(trade.price : Price.t)
                 ~taker:(Agg_trade.taker_side trade : Side.t)]
         | Depth_update _ -> Hash_set.add kinds "depthUpdate"
         | _ -> ());
        Hash_set.length kinds = 3)
    in
    print_s
      [%sexp (Hash_set.to_list kinds |> List.sort ~compare:String.compare : string list)];
    let%bind () =
      Market_ws.unsubscribe market [ Book_ticker btc; Agg_trade btc ] >>| ok_exn
    in
    print_s [%sexp (Market_ws.subscriptions market : Stream_name.t list)];
    let%bind trade =
      read_until (User_ws.events user) ~f:(function
        | Order_trade_update { order; _ } ->
          Execution_type.equal order.execution_type Trade
        | _ -> false)
    in
    (match trade with
     | Some (Order_trade_update { order; _ }) ->
       print_s
         [%message
           ""
             ~status:(order.status : Order_status.t)
             ~filled:(order.cum_filled_qty : Size.t)
             ~maker:(order.is_maker : bool)]
     | _ -> print_endline "no trade");
    [%expect
      {|
      ((Agg_trade BTCUSDT) (Book_ticker BTCUSDT)
       (Diff_depth (symbol BTCUSDT) (speed (Ms_100))))
      (trade (price 100000) (taker Buy))
      (aggTrade bookTicker depthUpdate)
      ((Diff_depth (symbol BTCUSDT) (speed (Ms_100))))
      ((status Filled) (filled 0.01) (maker true))
      |}];
    let%bind () = Market_ws.close market in
    User_ws.close ~close_key:true user)
;;
