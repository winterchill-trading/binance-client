(** A live {!Market_data.Feed.t} from Binance USD-M public market streams (no API key).

    Each requested stream kind gets its own combined-stream connection, so every frame is
    known to belong to one {!Market_data.Stream.t} before it is parsed:

    - [Trades]: [<symbol>@aggTrade] on the [Market] route;
    - [Bbo]: [<symbol>@bookTicker] on the [Public] route (checked on mainnet: [/market]
      sends nothing for it);
    - [L2]: [<symbol>@depth@100ms] on the [Public] route, with a local book per symbol
      synced from [GET /fapi/v1/depth?limit=1000]. Each sync (at start, after a sequence
      gap and after a reconnect) emits one [Snapshot] of the full book, followed by the
      diff events as [Delta]s;
    - [Funding]: [<symbol>@markPrice@1s] on the [Market] route.

    Routes only matter when [Config.routed_ws] is set (mainnet). Records carry
    [recv_ts = Time_ns.now ()] when the frame is taken off the socket, [exch_ts] from the
    venue (see {!Feed_normalizer}) and [instrument = binance:<SYMBOL>].

    Disconnected sockets are reconnected with exponential backoff and resubscribed (see
    [Venue_net.Ws_client]). Frames that do not decode are logged at debug level and
    dropped. Pipes are written without pushback; keep reading the ones you asked for. *)

open! Core
open Async

(** Connects one websocket per stream in [streams] and subscribes [symbols] (venue names,
    e.g. ["BTCUSDT"]; case-insensitive).

    [on_raw ~stream ~recv_ts frame] sees every websocket text frame exactly as received,
    before it is parsed (subscription acknowledgements included). On the [L2] stream it
    also sees each depth snapshot body, re-serialized from the parsed JSON; it has a
    ["lastUpdateId"] field and no ["e"], which is how it differs from a diff event.

    Raises if [streams] contains [Private] or a first connection fails. *)
val create
  :  ?on_raw:(stream:Market_data.Stream.t -> recv_ts:Time_ns.t -> string -> unit)
  -> Config.t
  -> symbols:string list
  -> streams:Market_data.Stream.t list
  -> Market_data.Feed.t Deferred.t

(** A running feed with its counters. *)
type t

(** Like {!create}, keeping the handle for {!stats}. *)
val start
  :  ?on_raw:(stream:Market_data.Stream.t -> recv_ts:Time_ns.t -> string -> unit)
  -> Config.t
  -> symbols:string list
  -> streams:Market_data.Stream.t list
  -> t Deferred.t

val feed : t -> Market_data.Feed.t

module Stats : sig
  type t =
    { frames : int (** websocket text frames received, acknowledgements included *)
    ; parse_errors : int
    (** frames that did not decode, or were error responses, and were dropped *)
    ; reconnects : int (** successful reconnections, summed over the connections *)
    }
  [@@deriving sexp_of]
end

val stats : t -> Stats.t

(** The subscription of one stream kind for [symbols], and its route. *)
val stream_names
  :  Market_data.Stream.t
  -> Binance_protocol.Symbol.t list
  -> Config.Route.t * Binance_protocol.Stream_name.t list
