(** Converts Binance market-stream frames into {!Market_data} records. Pure: the Async
    driver is {!Live_feed}.

    - [@aggTrade] becomes a {!Market_data.Trade.t} ([exch_ts] = trade time [T], side =
      taker side, [trade_id] = aggregate trade id [a]);
    - [@bookTicker] a {!Market_data.Bbo.t} ([exch_ts] = transaction time [T]);
    - [@markPrice] a {!Market_data.Funding.t} ([exch_ts] = event time [E]; the index price
      falls back to the mark price when [i] is missing);
    - [@depth] diff events feed a {!Local_book.t} per symbol and become
      {!Market_data.L2_update.t} deltas once the book is synced from a [GET /depth]
      snapshot, which itself becomes one [Snapshot] (the full book, [exch_ts] = its
      transaction time). Updates of symbols without a book are ignored. *)

open! Core
open Binance_protocol

module Output : sig
  type t =
    | Event of Market_data.Event.t
    | Need_snapshot of Symbol.t
    (** the book of this symbol is not synced (start, gap): fetch a snapshot and pass it
        to {!snapshot} *)
  [@@deriving sexp_of]
end

type t

(** Books are kept for [l2_symbols]. *)
val create : l2_symbols:Symbol.t list -> t

(** For recorded frames (market-recorder raw files): a book for every symbol whose depth
    updates show up, and [GET /depth] bodies that {!Live_feed} records on the l2 stream
    are passed to {!frame} like websocket frames. Such a body has no symbol and is applied
    to the book of the symbol of the latest depth update; a body that does not join the
    buffered updates gives [Need_snapshot] and the book waits for the next recorded one. *)
val create_replay : unit -> t

val instrument : Symbol.t -> Market_data.Instrument.t

(** Decodes one websocket text frame (a combined-stream [{"stream", "data"}] object or a
    bare payload) received at [recv_ts]. Subscription acknowledgements give [[]];
    undecodable frames and error responses give [Error]. Depth snapshot bodies give [[]]
    unless [t] is from {!create_replay}. *)
val frame : t -> recv_ts:Time_ns.t -> string -> Output.t list Or_error.t

(** Applies a depth snapshot to the book of [symbol] and replays the buffered events.
    [`Gap] when the snapshot does not join them (fetch a newer one). [`Ignored] when the
    symbol has no book or its book is already synced. *)
val snapshot
  :  t
  -> recv_ts:Time_ns.t
  -> Symbol.t
  -> Depth.t
  -> [ `Synced of Market_data.L2_update.t | `Gap | `Ignored ]

val is_synced : t -> Symbol.t -> bool

(** Drops every book, e.g. after a reconnect; the next updates ask for snapshots. *)
val reset : t -> unit

(** Single-message conversions. *)

val trade : recv_ts:Time_ns.t -> Agg_trade.t -> Market_data.Trade.t
val bbo : recv_ts:Time_ns.t -> Book_ticker_event.t -> Market_data.Bbo.t
val funding : recv_ts:Time_ns.t -> Mark_price_update.t -> Market_data.Funding.t
val delta : recv_ts:Time_ns.t -> Depth_update.t -> Market_data.L2_update.t
