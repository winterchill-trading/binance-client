(** Reading test pipes with a deadline without losing values.

    [Clock_ns.with_timeout t (Pipe.read r)] leaves the read pending after a timeout, and
    that read later swallows the next value. These helpers wait on
    [Pipe.values_available], which consumes nothing, then take the value with
    [Pipe.read_now]. *)

open! Core
open Async

(** The next value, or [None] at end of pipe or after [timeout] (default 5s). *)
val next : ?timeout:Time_ns.Span.t -> 'a Pipe.Reader.t -> 'a option Deferred.t

(** The first value satisfying [f] (earlier ones are consumed), or [None] at end of pipe
    or once [timeout] (default 5s) has passed since the call. *)
val until
  :  ?timeout:Time_ns.Span.t
  -> 'a Pipe.Reader.t
  -> f:('a -> bool)
  -> 'a option Deferred.t
