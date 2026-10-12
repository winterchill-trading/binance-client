(** Market streams over one combined-stream connection ([<ws_url>[/<route>]/stream]).

    Streams are added and removed with live [SUBSCRIBE] / [UNSUBSCRIBE] requests; the set
    of active streams is kept and resubscribed after every reconnect. Payloads arrive
    wrapped as [{"stream", "data"}] and are delivered as typed
    [Market_event.t Combined.t]. The transport (handshake, reconnect with backoff, pong
    replies to the server's ping frames) is [Venue_net.Ws_client]. *)

open! Core
open Async
open Binance_protocol

type t

(** Opens the connection and returns once the first handshake succeeded or failed.

    - [route] (default [Public]) picks the routed path when [Config.routed_ws] is set:
      diff depth is served on [Public], [@aggTrade], [@markPrice] and (probably)
      [@bookTicker] on [Market].
    - [ping] (default [`Frame 30s]): Binance pings the client every 3 minutes and
      venue-net answers; the client's own ping frames make the server answer with pongs,
      so a dead connection is detected after [max_silent_intervals] (default 2) quiet
      intervals. [`None] relies on the server alone.
    - [on_reconnect] runs after each reconnection once the streams were resubscribed.
      Updates sent while disconnected are lost: resync local books (see {!Book_sync}). *)
val connect
  :  ?route:Config.Route.t
  -> ?ping:Venue_net.Ws_client.Ping.t
  -> ?max_silent_intervals:int
  -> ?initial_backoff:Time_ns.Span.t
  -> ?max_backoff:Time_ns.Span.t
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> t Or_error.t Deferred.t

(** Every market event from every connection, in order. Frames that fail to decode are
    logged at debug level and dropped; subscription responses are consumed by
    {!subscribe}. Buffered without pushback, so keep reading. Closed by {!close}. *)
val events : t -> Market_event.t Combined.t Pipe.Reader.t

(** Adds [streams] and, when connected, waits for the server's acknowledgement (at most
    [timeout], default 10s). Streams already active are skipped. While disconnected it
    returns at once; the streams are sent on the next connection. *)
val subscribe
  :  ?timeout:Time_ns.Span.t
  -> t
  -> Stream_name.t list
  -> unit Or_error.t Deferred.t

val unsubscribe
  :  ?timeout:Time_ns.Span.t
  -> t
  -> Stream_name.t list
  -> unit Or_error.t Deferred.t

val subscriptions : t -> Stream_name.t list
val is_connected : t -> bool

(** Closes the socket, stops reconnecting, fails pending subscriptions and closes
    {!events}. *)
val close : t -> unit Deferred.t

module For_testing : sig
  type message =
    | Event of Market_event.t Combined.t
    | Response of Subscribe.Response.t
  [@@deriving sexp_of]

  val decode : string -> message Or_error.t
  val encode : Subscribe.Request.t -> string
end
