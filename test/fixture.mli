(** Recorded Binance USD-M mainnet frames under [fixtures/], one websocket text frame per
    line, as [Live_feed]'s [on_raw] sees them: [trades.jsonl] ([btcusdt@aggTrade]),
    [bbo.jsonl] ([btcusdt@bookTicker]), [l2.jsonl] ([btcusdt@depth@100ms] with one
    [GET /depth] snapshot body in between, trimmed to 100 levels a side) and
    [funding.jsonl] ([btcusdt@markPrice@1s]). Each file starts with the subscription
    acknowledgement. *)

open! Core

val streams : Market_data.Stream.t list

(** The frames of one stream, in arrival order. *)
val frames : Market_data.Stream.t -> string list

(** A depth snapshot body (it has [lastUpdateId]) rather than a websocket frame. *)
val is_snapshot : string -> bool
