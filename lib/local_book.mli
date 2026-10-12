(** A full order book of one symbol, kept from a [GET /fapi/v1/depth] snapshot plus the
    diff-depth stream ([<symbol>@depth@100ms]), following Binance's procedure:

    + buffer diff events until a snapshot (last update id [L]) arrives;
    + drop events with [u < L];
    + the first applied event must have [U <= L <= u], or continue the snapshot directly
      ([pu = L]; Binance's simulator sends that after an idle snapshot);
    + every later event must have [pu] equal to the previous event's [u].

    Quantities are absolute and zero removes a level. When the sequence breaks the book is
    dropped and events are buffered again until the next snapshot ({!Outcome.Gap}): the
    caller fetches a new snapshot and calls {!apply_snapshot}. Pure; the Async driver is
    {!Book_sync}. *)

open! Core
open Binance_protocol

type t

module Outcome : sig
  type t =
    | Applied (** the book changed *)
    | Buffered (** no snapshot yet; kept for replay *)
    | Stale (** older than the book; ignored *)
    | Gap (** sequence broken: the book is dropped, fetch a new snapshot *)
  [@@deriving sexp, compare, equal]
end

(** [max_buffered] (default 1000) bounds the events kept while waiting for a snapshot; the
    oldest are dropped first. *)
val create : ?max_buffered:int -> Symbol.t -> t

val symbol : t -> Symbol.t
val is_synced : t -> bool

(** The [u] of the last applied event, or the snapshot's [lastUpdateId]. *)
val last_update_id : t -> int option

(** Drops the book and buffers events until the next snapshot, e.g. after a reconnect. *)
val reset : t -> unit

(** Number of events waiting for a snapshot. *)
val num_buffered : t -> int

(** Replaces the book with [depth] and replays the buffered events. [`Gap] when they do
    not connect to it (the snapshot is too old or the buffer lost events): the book is
    dropped again and a newer snapshot is needed. *)
val apply_snapshot : t -> Depth.t -> [ `Synced | `Gap ]

val apply_update : t -> Depth_update.t -> Outcome.t

(** The book, best price first, at most [depth] levels per side (default: all), stamped
    with the last transaction time in nanoseconds. [None] until synced. Binance does not
    report order counts, so [n] is [0]. *)
val l2 : ?depth:int -> t -> Exchange_core.L2.t option
