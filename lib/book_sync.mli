(** Keeps a {!Local_book} in sync: feeds it diff-depth events, fetches
    [GET /fapi/v1/depth] snapshots when it has none (at start, after a gap, after
    {!reset}), and reports the book after every change.

    Snapshots are fetched one at a time. A snapshot that does not join the buffered events
    (taken before them) is retried after [retry_delay]; a failed request after
    [error_delay]. *)

open! Core
open Binance_protocol

type t

(** [levels] (default 50) bounds each side of the books passed to [on_book];
    [snapshot_limit] (default 1000) is the [limit] of depth requests. *)
val create
  :  ?levels:int
  -> ?snapshot_limit:int
  -> ?retry_delay:Time_ns.Span.t
  -> ?error_delay:Time_ns.Span.t
  -> Config.t
  -> Symbol.t
  -> on_book:(Exchange_core.L2.t -> unit)
  -> t

val local_book : t -> Local_book.t

(** Fetches a snapshot unless one is already being fetched or the book is synced. Call it
    once the diff stream is subscribed. *)
val start : t -> unit

(** Applies a diff event of this symbol (others are ignored). *)
val handle : t -> Depth_update.t -> unit

(** Drops the book and fetches a new snapshot, e.g. after the stream reconnected. *)
val reset : t -> unit

(** Stops fetching and reporting. *)
val close : t -> unit
