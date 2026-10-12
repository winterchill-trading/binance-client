open! Core
open! Async
open Binance_protocol
module Neutral = Exchange_core
module Fill = Mm_core.Fill
module Neutral_update = Mm_core.Order_update

let base_fees = { Neutral.Fee_schedule.taker_bps = 5.; maker_bps = 2. }

module Convert = struct
  let ns_of_ms ms = Time_ns.to_int_ns_since_epoch (Timestamp_ms.to_time_ns ms)
  let usd_of_decimal d = Usd.of_units (Decimal.to_units d)

  let fill ({ order = o; _ } : Order_trade_update.t) : Fill.t option =
    match o.execution_type with
    | (Trade | Calculated) when Size.is_positive o.last_filled_qty ->
      Some
        { trade_id = o.trade_id
        ; order_id = o.order_id
        ; client_order_id = Some o.client_order_id
        ; symbol = o.symbol
        ; side = o.side
        ; px = o.last_filled_price
        ; size = o.last_filled_qty
        ; fee = Option.value_map o.commission ~default:Usd.zero ~f:usd_of_decimal
        ; is_maker = o.is_maker
        ; time = ns_of_ms o.trade_time
        ; closed_pnl = Some o.realized_profit
        }
    | _ -> None
  ;;

  let order_update ({ order = o; _ } : Order_trade_update.t) : Neutral_update.t option =
    let status : Neutral_update.Status.t option =
      match o.status with
      | New -> Some New
      | Partially_filled -> Some Partially_filled
      | Filled -> Some Filled
      | Canceled | Expired | Expired_in_match -> Some Cancelled
      | Other _ -> None
    in
    Option.map status ~f:(fun status : Neutral_update.t ->
      { order_id = o.order_id
      ; client_order_id = Some o.client_order_id
      ; symbol = o.symbol
      ; side = o.side
      ; px = o.price
      ; status
      ; filled = o.cum_filled_qty
      ; remaining =
          (if Neutral_update.Status.is_final status
           then Size.zero
           else Size.(max zero (o.orig_qty - o.cum_filled_qty)))
      ; time = ns_of_ms o.trade_time
      })
  ;;

  let reject_reason (e : Api_error.t) =
    let mentions pattern =
      String.is_substring (String.lowercase e.msg) ~substring:pattern
    in
    match Api_error.to_reject_reason e, e.code with
    | Some reason, _ -> reason
    | None, Other -4116 -> Neutral.Reject_reason.Duplicate_client_order_id
    | None, New_order_rejected when mentions "insufficient" -> Insufficient_margin
    | None, New_order_rejected when mentions "same account" -> Self_trade
    | None, _ ->
      Neutral.Reject_reason.Unsupported
        (sprintf "%d %s" (Api_error.Code.to_int e.code) e.msg)
  ;;

  let order_status (o : Order.t) : Neutral.Order_status.t =
    let client_order_id = Some o.client_order_id in
    let filled () =
      Neutral.Order_status.Filled
        { order_id = o.order_id
        ; total = o.executed_qty
        ; avg_px = Option.value o.avg_price ~default:o.price
        ; client_order_id
        }
    in
    let resting () =
      Neutral.Order_status.Resting { order_id = o.order_id; client_order_id }
    in
    match o.status with
    | New -> resting ()
    | Partially_filled ->
      (match o.time_in_force with
       | Ioc | Fok -> filled ()
       | Gtc | Gtx | Gtd | Rpi -> resting ())
    | Filled -> filled ()
    | Canceled | Expired | Expired_in_match | Other _ ->
      if Size.is_positive o.executed_qty
      then filled ()
      else (
        let reason : Neutral.Reject_reason.t =
          match o.status, o.time_in_force with
          | Expired_in_match, _ -> Self_trade
          | _, (Gtx | Rpi) -> Post_only_would_cross { best_bid = None; best_ask = None }
          | _, (Ioc | Fok) -> No_liquidity
          | _, (Gtc | Gtd) ->
            Unsupported ("order " ^ Order_status.to_wire o.status ^ " on placement")
        in
        Rejected reason)
  ;;

  let order_request (r : Neutral.Order_request.t) =
    let valid_cloid =
      match r.client_order_id with
      | None -> true
      | Some id ->
        Or_error.is_ok (Client_order_id.validate (Client_order_id.to_string id))
    in
    if not valid_cloid
    then
      Error
        (Neutral.Reject_reason.Unsupported
           "client order id must be 1-36 characters from [A-Za-z0-9.:/_-]")
    else (
      match r.order_type, r.px with
      | Limit _, None -> Error Neutral.Reject_reason.Invalid_price
      | _ ->
        Ok
          { (New_order.Request.of_order_request r) with
            new_order_resp_type = Some Result
          })
  ;;

  let account ~account (a : Account.t) (risks : Position_risk.t list) ~now
    : Neutral.Account_view.t
    =
    let leverage_of symbol =
      List.find_map a.positions ~f:(fun (p : Account.Position.t) ->
        if Symbol.equal p.symbol symbol then p.leverage else None)
    in
    let positions =
      List.filter risks ~f:(fun (p : Position_risk.t) ->
        not (Size.is_zero p.position_amt))
      |> List.map ~f:(fun (p : Position_risk.t) : Neutral.Position_view.t ->
        let leverage =
          Option.first_some p.leverage (leverage_of p.symbol) |> Option.value ~default:1
        in
        let notional =
          match p.notional with
          | Some n -> Usd.abs n
          | None ->
            Usd.of_units
              (Fixed_decimal.mul_units
                 (Size.to_units (Size.abs p.position_amt))
                 (Price.to_units p.mark_price))
        in
        { symbol = p.symbol
        ; size = p.position_amt
        ; entry_px = p.entry_price
        ; notional
        ; unrealized_pnl = p.unrealized_profit
        ; realized_pnl = Usd.zero
        ; cum_funding = Usd.zero
        ; liquidation_px =
            (if Price.is_zero p.liquidation_price then None else Some p.liquidation_price)
        ; leverage
        ; max_leverage = leverage
        ; margin_used =
            (match Option.first_some p.position_initial_margin p.initial_margin with
             | Some m -> m
             | None -> Usd.div_int notional (Int.max 1 leverage))
        })
      |> List.sort ~compare:(fun (x : Neutral.Position_view.t) y ->
        Symbol.compare x.symbol y.symbol)
    in
    let total_notional =
      Usd.sum (List.map positions ~f:(fun (p : Neutral.Position_view.t) -> p.notional))
    in
    let margin_used =
      match Option.first_some a.total_position_initial_margin a.total_initial_margin with
      | Some m -> m
      | None ->
        Usd.sum
          (List.map positions ~f:(fun (p : Neutral.Position_view.t) -> p.margin_used))
    in
    { account
    ; balance = a.total_wallet_balance
    ; equity = a.total_margin_balance
    ; total_notional
    ; margin_used
    ; maintenance_margin = Option.value a.total_maint_margin ~default:Usd.zero
    ; available = a.available_balance
    ; positions
    ; time =
        (match a.update_time with
         | Some ms when Timestamp_ms.to_int ms > 0 -> ns_of_ms ms
         | _ -> Time_ns.to_int_ns_since_epoch now)
    }
  ;;
end

module Seen_trades = struct
  type t = Neutral.Trade_id.Hash_set.t

  let create () = Neutral.Trade_id.Hash_set.create ()

  let first_time t trade_id =
    if Hash_set.mem t trade_id
    then false
    else (
      Hash_set.add t trade_id;
      true)
  ;;
end

type config = Config.t

type book =
  { sync : Book_sync.t
  ; mutable writers : Neutral.L2.t Pipe.Writer.t list
  }

type t =
  { config : Config.t
  ; account_id : Neutral.Account.t
  ; exchange_info : Exchange_info.t
  ; market : Market_ws.t
  ; user : User_ws.t
  ; seen_trades : Seen_trades.t
  ; fills_r : Fill.t Pipe.Reader.t
  ; fills_w : Fill.t Pipe.Writer.t
  ; updates_r : Neutral_update.t Pipe.Reader.t
  ; updates_w : Neutral_update.t Pipe.Writer.t
  ; books : book Symbol.Table.t
  ; reconnect_hooks : (unit -> unit) Queue.t
  ; mutable closed : bool
  }

let on_user_event t (event : User_event.t) =
  match event with
  | Order_trade_update update ->
    Option.iter (Convert.fill update) ~f:(fun fill ->
      if Seen_trades.first_time t.seen_trades fill.trade_id
      then Pipe.write_without_pushback_if_open t.fills_w fill);
    Option.iter (Convert.order_update update) ~f:(fun update ->
      Pipe.write_without_pushback_if_open t.updates_w update)
  | Account_update _ | Listen_key_expired _ | Unknown _ -> ()
;;

let on_market_event t ({ data; stream = _ } : Market_event.t Combined.t) =
  match data with
  | Depth_update update ->
    Option.iter (Hashtbl.find t.books update.symbol) ~f:(fun book ->
      Book_sync.handle book.sync update)
  | _ -> ()
;;

let connect config =
  let open Deferred.Or_error.Let_syntax in
  let%bind exchange_info = Rest.exchange_info config in
  let reconnect_hooks = Queue.create () in
  let run_hooks () = Queue.iter reconnect_hooks ~f:(fun f -> f ()) in
  let books = Symbol.Table.create () in
  let%bind user = User_ws.connect ~on_reconnect:run_hooks config in
  let%bind market =
    Market_ws.connect
      ~route:Public
      ~on_reconnect:(fun () ->
        Hashtbl.iter books ~f:(fun book -> Book_sync.reset book.sync);
        run_hooks ())
      config
  in
  let fills_r, fills_w = Pipe.create () in
  let updates_r, updates_w = Pipe.create () in
  let t =
    { config
    ; account_id = Neutral.Account.of_string config.keys.api_key
    ; exchange_info
    ; market
    ; user
    ; seen_trades = Seen_trades.create ()
    ; fills_r
    ; fills_w
    ; updates_r
    ; updates_w
    ; books
    ; reconnect_hooks
    ; closed = false
    }
  in
  don't_wait_for (Pipe.iter_without_pushback (User_ws.events user) ~f:(on_user_event t));
  don't_wait_for
    (Pipe.iter_without_pushback (Market_ws.events market) ~f:(on_market_event t));
  return t
;;

let instrument t symbol =
  match Exchange_info.find_symbol t.exchange_info symbol with
  | None -> Deferred.Or_error.error_s [%message "unknown symbol" (symbol : Symbol.t)]
  | Some info ->
    let%map risks = Rest.position_risk ~symbol t.config in
    let max_leverage =
      match risks with
      | Ok risks -> List.find_map risks ~f:(fun (r : Position_risk.t) -> r.leverage)
      | Error error ->
        Log.Global.info_s
          [%message "binance venue: positionRisk failed, assuming 20x" (error : Error.t)];
        None
    in
    (match
       Exchange_info.Symbol_info.to_instrument
         info
         ~fees:base_fees
         ~max_leverage:(Option.value max_leverage ~default:20)
     with
     | Some instrument -> Ok instrument
     | None ->
       Or_error.error_s
         [%message "symbol lacks PRICE_FILTER or LOT_SIZE" (symbol : Symbol.t)])
;;

let books t symbol =
  let r, w = Pipe.create () in
  match Hashtbl.find t.books symbol with
  | Some book ->
    book.writers <- w :: book.writers;
    Option.iter
      (Local_book.l2 (Book_sync.local_book book.sync) ~depth:50)
      ~f:(Pipe.write_without_pushback w);
    return r
  | None ->
    let rec book =
      lazy
        { sync =
            Book_sync.create t.config symbol ~on_book:(fun l2 ->
              let book = force book in
              book.writers
              <- List.filter book.writers ~f:(fun w -> not (Pipe.is_closed w));
              List.iter book.writers ~f:(fun w -> Pipe.write_without_pushback w l2))
        ; writers = [ w ]
        }
    in
    let book = force book in
    Hashtbl.set t.books ~key:symbol ~data:book;
    let%map result =
      Market_ws.subscribe t.market [ Diff_depth { symbol; speed = Some Ms_100 } ]
    in
    (match result with
     | Ok () -> ()
     | Error error ->
       Log.Global.error_s
         [%message
           "binance venue: depth subscription" (symbol : Symbol.t) (error : Error.t)]);
    Book_sync.start book.sync;
    r
;;

let fills t = t.fills_r
let order_updates t = t.updates_r

let open_orders t symbol =
  let%map.Deferred.Or_error orders = Rest.open_orders ~symbol t.config in
  List.filter_map orders ~f:(fun (o : Open_order.t) ->
    Option.some_if (Symbol.equal o.symbol symbol) (Open_order.to_exchange_core o))
;;

let account t =
  let%map account = Rest.account t.config
  and risks = Rest.position_risk t.config in
  let%map.Or_error account and risks in
  Convert.account ~account:t.account_id account risks ~now:(Time_ns.now ())
;;

(* One request per group of at most 5 orders, in order. *)
let place_group t requests : Neutral.Order_status.t list Or_error.t Deferred.t =
  match requests with
  | [] -> Deferred.Or_error.return []
  | [ request ] ->
    (match%map Rest.new_order t.config request with
     | Ok order -> Ok [ Convert.order_status order ]
     | Error error ->
       (match Rest.api_error error with
        | Some api_error -> Ok [ Rejected (Convert.reject_reason api_error) ]
        | None -> Error error))
  | requests ->
    (match Batch_orders.Request.create requests with
     | Error _ as error -> return error
     | Ok batch ->
       let%map items = Rest.batch_orders t.config batch in
       let%bind.Or_error items in
       if List.length items <> List.length requests
       then
         Or_error.error_s
           [%message
             "batchOrders: wrong number of results"
               ~sent:(List.length requests : int)
               ~received:(List.length items : int)]
       else
         Ok
           (List.map items ~f:(function
             | Ok order -> Convert.order_status order
             | Error api_error -> Rejected (Convert.reject_reason api_error))))
;;

let place t requests =
  let converted = List.map requests ~f:Convert.order_request in
  let to_send = List.filter_map converted ~f:Result.ok in
  let%map.Deferred.Or_error statuses =
    Deferred.Or_error.List.map
      (List.chunks_of to_send ~length:Batch_orders.Request.max_orders)
      ~how:`Sequential
      ~f:(place_group t)
    >>| Or_error.map ~f:List.concat
  in
  let rec merge converted statuses =
    match converted, statuses with
    | [], _ -> []
    | Error reason :: rest, statuses ->
      Neutral.Order_status.Rejected reason :: merge rest statuses
    | Ok _ :: rest, status :: statuses -> status :: merge rest statuses
    | Ok _ :: _, [] -> assert false
  in
  merge converted statuses
;;

let cancel t symbol order_ids =
  let%map results =
    Deferred.List.map order_ids ~how:`Parallel ~f:(fun order_id ->
      match%map Rest.cancel_order t.config { symbol; target = Order_id order_id } with
      | Ok (_ : Order.t) -> Ok ()
      | Error error ->
        (match Rest.api_error error with
         | Some { code = Cancel_rejected | No_such_order; _ } ->
           Log.Global.debug_s
             [%message "binance venue: cancel of a gone order" (order_id : Order_id.t)];
           Ok ()
         | _ -> Error error))
  in
  Or_error.combine_errors_unit results
;;

let cancel_all t symbol = Rest.cancel_all t.config symbol
let on_reconnect t ~f = Queue.enqueue t.reconnect_hooks f

let close t =
  if t.closed
  then return ()
  else (
    t.closed <- true;
    let%map () = User_ws.close t.user
    and () = Market_ws.close t.market in
    Pipe.close t.fills_w;
    Pipe.close t.updates_w;
    Hashtbl.iter t.books ~f:(fun book ->
      Book_sync.close book.sync;
      List.iter book.writers ~f:Pipe.close))
;;
