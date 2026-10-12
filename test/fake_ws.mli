(** A scripted websocket server on a free local port: it accepts every upgrade, records
    the text frames clients send and lets a test push frames or drop connections. *)

open! Core
open Async

type t

val start : unit -> t Deferred.t

(** A config whose REST and websocket roots point at the server. *)
val config : t -> Binance_client.Config.t

(** Upgrades accepted so far. *)
val connections : t -> int

(** Text frames received from clients, oldest first. *)
val received : t -> string list

(** Sends a text frame on the newest connection. *)
val send : t -> string -> unit

(** Closes the newest connection. *)
val drop : t -> unit Deferred.t

(** Polls [f] in real time (loopback I/O takes milliseconds) until it holds or [timeout]
    (default 5s) passes. *)
val wait_until : ?timeout:Time_ns.Span.t -> (unit -> bool) -> bool Deferred.t

val close : t -> unit Deferred.t
