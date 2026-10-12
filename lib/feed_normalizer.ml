open! Core
open Binance_protocol

module Output = struct
  type t =
    | Event of Market_data.Event.t
    | Need_snapshot of Symbol.t
  [@@deriving sexp_of]
end

type t = { books : Local_book.t Symbol.Table.t }

let create ~l2_symbols =
  let books = Symbol.Table.create () in
  List.iter l2_symbols ~f:(fun symbol ->
    Hashtbl.set books ~key:symbol ~data:(Local_book.create symbol));
  { books }
;;

let instrument symbol = Market_data.Instrument.create Binance (Symbol.to_string symbol)
let ts = Timestamp_ms.to_time_ns

let trade ~recv_ts (e : Agg_trade.t) : Market_data.Trade.t =
  { instrument = instrument e.symbol
  ; exch_ts = ts e.trade_time
  ; recv_ts
  ; price = e.price
  ; size = e.qty
  ; side = Agg_trade.taker_side e
  ; trade_id = Int.to_string e.agg_trade_id
  }
;;

let bbo ~recv_ts (e : Book_ticker_event.t) : Market_data.Bbo.t =
  { instrument = instrument e.symbol
  ; exch_ts = ts e.transaction_time
  ; recv_ts
  ; bid_price = e.bid_price
  ; bid_size = e.bid_qty
  ; ask_price = e.ask_price
  ; ask_size = e.ask_qty
  }
;;

let funding ~recv_ts (e : Mark_price_update.t) : Market_data.Funding.t =
  { instrument = instrument e.symbol
  ; exch_ts = ts e.event_time
  ; recv_ts
  ; rate = Decimal.to_float e.funding_rate
  ; next_funding_time = ts e.next_funding_time
  ; mark_price = e.mark_price
  ; index_price = Option.value e.index_price ~default:e.mark_price
  }
;;

let levels (levels : Book_level.t list) =
  List.map levels ~f:(fun { price; qty } ->
    { Market_data.L2_update.Level.price; size = qty })
;;

let delta ~recv_ts (e : Depth_update.t) : Market_data.L2_update.t =
  { instrument = instrument e.symbol
  ; exch_ts = ts e.transaction_time
  ; recv_ts
  ; kind = Delta
  ; bids = levels e.bids
  ; asks = levels e.asks
  }
;;

let snapshot_of_book ~recv_ts book : Market_data.L2_update.t option =
  let levels =
    List.map ~f:(fun { Exchange_core.L2.Level.px; sz; n = _ } ->
      { Market_data.L2_update.Level.price = px; size = sz })
  in
  Option.map (Local_book.l2 book) ~f:(fun (l2 : Exchange_core.L2.t) ->
    { Market_data.L2_update.instrument = instrument l2.symbol
    ; exch_ts = Time_ns.of_int_ns_since_epoch l2.time
    ; recv_ts
    ; kind = Snapshot
    ; bids = levels l2.bids
    ; asks = levels l2.asks
    })
;;

let depth_update t ~recv_ts (e : Depth_update.t) : Output.t list =
  match Hashtbl.find t.books e.symbol with
  | None -> []
  | Some book ->
    (match Local_book.apply_update book e with
     | Applied -> [ Event (L2 (delta ~recv_ts e)) ]
     | Stale -> []
     | Buffered | Gap -> [ Need_snapshot e.symbol ])
;;

let decode frame =
  Or_error.try_with (fun () ->
    let json = Yojson.Safe.from_string frame in
    if Subscribe.Response.is_response json
    then `Response (Subscribe.Response.t_of_yojson json)
    else (
      match json with
      | `Assoc fields when List.Assoc.mem fields "stream" ~equal:String.equal ->
        `Event (Combined.t_of_yojson Market_event.t_of_yojson json).data
      | _ -> `Event (Market_event.t_of_yojson json)))
;;

let frame t ~recv_ts frame : Output.t list Or_error.t =
  match decode frame with
  | Error _ as error -> error
  | Ok (`Response (Result _)) -> Ok []
  | Ok (`Response (Error { code; msg; id })) ->
    Or_error.error_s
      [%message "subscription request failed" (code : int) msg (id : int option)]
  | Ok (`Event event) ->
    Ok
      (match event with
       | Agg_trade e -> [ Event (Trade (trade ~recv_ts e)) ]
       | Book_ticker e -> [ Event (Bbo (bbo ~recv_ts e)) ]
       | Mark_price_update e -> [ Event (Funding (funding ~recv_ts e)) ]
       | Depth_update e -> depth_update t ~recv_ts e
       | Depth_snapshot _ | Unknown _ -> [])
;;

let snapshot t ~recv_ts symbol depth =
  match Hashtbl.find t.books symbol with
  | None -> `Ignored
  | Some book when Local_book.is_synced book -> `Ignored
  | Some book ->
    (match Local_book.apply_snapshot book depth with
     | `Gap -> `Gap
     | `Synced ->
       (match snapshot_of_book ~recv_ts book with
        | Some update -> `Synced update
        | None -> `Gap))
;;

let is_synced t symbol =
  Hashtbl.find t.books symbol |> Option.exists ~f:Local_book.is_synced
;;

let reset t = Hashtbl.iter t.books ~f:Local_book.reset
