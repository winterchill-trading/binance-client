(** A [@zero_alloc] scanner for the hot Binance market frames: [aggTrade], [bookTicker]
    and [depthUpdate], combined-stream ([{"stream":...,"data":{...}}]) or bare. {!scan}
    walks the frame once and reads the fields straight into a reused record, without
    building a JSON tree or allocating.

    The scanner is strict: any frame it is not sure about (another event type, a
    subscription response, whitespace, escapes in strings, a decimal sent as a JSON
    number, a missing or duplicated field, a value out of range) makes {!scan} return
    [false], and the caller decodes the frame with Yojson instead. When {!scan} returns
    [true], the fields hold what the Yojson decoders in [binance-protocol] would give.

    Decimals are in units of [10^-8] (the representation of [Price.t] and [Size.t]);
    timestamps are milliseconds. *)

open! Core

module Kind : sig
  type t =
    | Agg_trade
    | Book_ticker
    | Depth_update
  [@@deriving sexp_of, equal]
end

type t = private
  { mutable kind : Kind.t
  ; mutable symbol_pos : int (** offset of the [s] value in the frame *)
  ; mutable symbol_len : int
  ; mutable event_time : int (** [E] *)
  ; mutable transaction_time : int
  (** [T]: trade time of an [aggTrade], transaction time otherwise *)
  ; mutable agg_trade_id : int (** [a] of an [aggTrade] *)
  ; mutable price : int (** [p] *)
  ; mutable qty : int (** [q] *)
  ; mutable buyer_is_maker : bool (** [m] *)
  ; mutable update_id : int (** [u] of a [bookTicker] *)
  ; mutable bid_price : int (** [b] *)
  ; mutable bid_qty : int (** [B] *)
  ; mutable ask_price : int (** [a] *)
  ; mutable ask_qty : int (** [A] *)
  ; mutable first_update_id : int (** [U] of a [depthUpdate] *)
  ; mutable final_update_id : int (** [u] *)
  ; mutable prev_final_update_id : int (** [pu] *)
  ; mutable bids : int (** offset of the [b] array; read it with {!next_level} *)
  ; mutable asks : int (** offset of the [a] array *)
  ; mutable level_price : int (** set by {!next_level} *)
  ; mutable level_qty : int
  ; fields : fields
  }

and fields

val create : unit -> t

(** [true] when [frame] is one of the three event types and every field was read; the
    record is then valid until the next call. *)
val scan : t -> string -> bool
[@@zero_alloc]

(** [next_level t frame pos] reads the [[price, qty]] level after [pos] into
    [level_price] and [level_qty] and returns the position to pass next, or [-1] at the
    end of the array. Start with [pos = t.bids] or [pos = t.asks] after a successful
    {!scan} of a [Depth_update]. *)
val next_level : t -> string -> int -> int
[@@zero_alloc]

(** [symbol_equal t frame s] is whether the [s] field of the scanned frame is the bytes
    of [s]. *)
val symbol_equal : t -> string -> string -> bool
[@@zero_alloc]

(** Decimal string (without quotes) in [s] from [pos] to [stop], in units of [10^-8],
    under the rules of [Fixed_decimal.of_string]; [Int.min_value] when it is malformed
    or out of range. Exposed for tests. *)
val decimal : string -> pos:int -> stop:int -> int
[@@zero_alloc]
