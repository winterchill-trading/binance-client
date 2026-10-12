(** The user data stream: [POST /fapi/v1/listenKey], then a websocket on
    [<ws_url>[/private]/ws/<listenKey>] delivering [ORDER_TRADE_UPDATE], [ACCOUNT_UPDATE]
    and [listenKeyExpired] events.

    - The key is kept alive with [PUT /fapi/v1/listenKey] every [keepalive_interval]
      (default 30 minutes; keys expire 60 minutes after the last keepalive).
    - When the keepalive answers [-1125] (unknown key) or a [listenKeyExpired] event
      arrives, a new key is created and the socket replaced; [on_reconnect] then runs,
      since events may have been missed.
    - Socket drops are retried with backoff by [Venue_net.Ws_client] on the same key, and
      [on_reconnect] runs after each. *)

open! Core
open Async
open Binance_protocol

type t

val connect
  :  ?keepalive_interval:Time_ns.Span.t
  -> ?ping:Venue_net.Ws_client.Ping.t
  -> ?max_silent_intervals:int
  -> ?initial_backoff:Time_ns.Span.t
  -> ?max_backoff:Time_ns.Span.t
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> t Or_error.t Deferred.t

(** Every decoded event, [Listen_key_expired] included, across key renewals. Frames that
    fail to decode are logged at debug level and dropped. Buffered without pushback;
    closed by {!close}. *)
val events : t -> User_event.t Pipe.Reader.t

val listen_key : t -> string
val is_connected : t -> bool

(** Creates a new key and replaces the socket now. *)
val renew : t -> unit Or_error.t Deferred.t

(** Closes the socket, stops the keepalive and closes {!events}. With [close_key] (default
    [false]) also sends [DELETE /fapi/v1/listenKey]; note that Binance shares one key per
    account between connections. *)
val close : ?close_key:bool -> t -> unit Deferred.t

module For_testing : sig
  val decode : string -> User_event.t Or_error.t
end
