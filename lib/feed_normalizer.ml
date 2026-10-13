open! Core
open Binance_protocol

module Output = struct
  type t =
    | Event of Market_data.Event.t
    | Need_snapshot of Symbol.t
  [@@deriving sexp_of]
end

(* A symbol seen by the scanner: its bytes in the frame, and the values built from them
   once. *)
module Cached_symbol = struct
  type t =
    { raw : string
    ; symbol : Symbol.t
    ; instrument : Market_data.Instrument.t
    }
end

type t =
  { books : Local_book.t Symbol.Table.t
  ; replay : bool
  ; mutable last_l2_symbol : Symbol.t option
  ; scan : Frame_scan.t
  ; mutable symbols : Cached_symbol.t list
  ; mutable n_symbols : int
  }

let make ~replay books =
  { books
  ; replay
  ; last_l2_symbol = None
  ; scan = Frame_scan.create ()
  ; symbols = []
  ; n_symbols = 0
  }
;;

let create ~l2_symbols =
  let books = Symbol.Table.create () in
  List.iter l2_symbols ~f:(fun symbol ->
    Hashtbl.set books ~key:symbol ~data:(Local_book.create symbol));
  make ~replay:false books
;;

let create_replay () = make ~replay:true (Symbol.Table.create ())
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

let find_book t symbol =
  if t.replay
  then (
    t.last_l2_symbol <- Some symbol;
    Some
      (Hashtbl.find_or_add t.books symbol ~default:(fun () -> Local_book.create symbol)))
  else Hashtbl.find t.books symbol
;;

let depth_update t ~recv_ts (e : Depth_update.t) : Output.t list =
  match find_book t e.symbol with
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

(* A recorded [GET /depth] body has no symbol: it was fetched for the symbol whose updates
   came before it in the same recorded series. *)
let recorded_snapshot t ~recv_ts depth : Output.t list =
  match t.last_l2_symbol with
  | None -> []
  | Some symbol ->
    (match snapshot t ~recv_ts symbol depth with
     | `Synced update -> [ Event (L2 update) ]
     | `Gap -> [ Need_snapshot symbol ]
     | `Ignored -> [])
;;

let frame_reference t ~recv_ts frame : Output.t list Or_error.t =
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
       | Depth_snapshot depth when t.replay -> recorded_snapshot t ~recv_ts depth
       | Depth_snapshot _ | Unknown _ -> [])
;;

(* The scanned path. Builds what [frame_reference] builds from the fields {!Frame_scan}
   read; the fixture test checks both agree on every recorded frame. *)

let max_cached_symbols = 256

let rec find_symbol scan frame : Cached_symbol.t list -> Cached_symbol.t or_null
  = function
  | [] -> Null
  | cached :: rest ->
    if Frame_scan.symbol_equal scan frame cached.raw
    then This cached
    else find_symbol scan frame rest
;;

let scanned_symbol t frame : Cached_symbol.t =
  match find_symbol t.scan frame t.symbols with
  | This cached -> cached
  | Null ->
    let raw = String.sub frame ~pos:t.scan.symbol_pos ~len:t.scan.symbol_len in
    let symbol = Symbol.of_string raw in
    let cached = { Cached_symbol.raw; symbol; instrument = instrument symbol } in
    if t.n_symbols < max_cached_symbols
    then (
      t.symbols <- cached :: t.symbols;
      t.n_symbols <- t.n_symbols + 1);
    cached
;;

let ms x = ts (Timestamp_ms.of_int x)

let scanned_levels t frame pos =
  let rec go pos acc =
    let next = Frame_scan.next_level t.scan frame pos in
    if next < 0
    then List.rev acc
    else
      go
        next
        ({ Book_level.price = Price.of_units t.scan.level_price
         ; qty = Size.of_units t.scan.level_qty
         }
         :: acc)
  in
  go pos []
;;

let scanned t ~recv_ts frame : Output.t list =
  let s = t.scan in
  let cached = scanned_symbol t frame in
  match s.kind with
  | Agg_trade ->
    [ Event
        (Trade
           { instrument = cached.instrument
           ; exch_ts = ms s.transaction_time
           ; recv_ts
           ; price = Price.of_units s.price
           ; size = Size.of_units s.qty
           ; side = (if s.buyer_is_maker then Sell else Buy)
           ; trade_id = Int.to_string s.agg_trade_id
           })
    ]
  | Book_ticker ->
    [ Event
        (Bbo
           { instrument = cached.instrument
           ; exch_ts = ms s.transaction_time
           ; recv_ts
           ; bid_price = Price.of_units s.bid_price
           ; bid_size = Size.of_units s.bid_qty
           ; ask_price = Price.of_units s.ask_price
           ; ask_size = Size.of_units s.ask_qty
           })
    ]
  | Depth_update ->
    depth_update
      t
      ~recv_ts
      { event_time = Timestamp_ms.of_int s.event_time
      ; transaction_time = Timestamp_ms.of_int s.transaction_time
      ; symbol = cached.symbol
      ; first_update_id = s.first_update_id
      ; final_update_id = s.final_update_id
      ; prev_final_update_id = s.prev_final_update_id
      ; bids = scanned_levels t frame s.bids
      ; asks = scanned_levels t frame s.asks
      }
;;

let frame t ~recv_ts frame =
  if Frame_scan.scan t.scan frame
  then Ok (scanned t ~recv_ts frame)
  else frame_reference t ~recv_ts frame
;;

let is_synced t symbol =
  Hashtbl.find t.books symbol |> Option.exists ~f:Local_book.is_synced
;;

let reset t = Hashtbl.iter t.books ~f:Local_book.reset
