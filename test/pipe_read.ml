open! Core
open Async

let default_timeout = Time_ns.Span.of_int_sec 5

let next_before ~deadline reader =
  match%map
    Clock_ns.with_timeout
      (Time_ns.diff deadline (Time_ns.now ()))
      (Pipe.values_available reader)
  with
  | `Timeout | `Result `Eof -> None
  | `Result `Ok ->
    (match Pipe.read_now reader with
     | `Ok x -> Some x
     | `Eof | `Nothing_available -> None)
;;

let next ?(timeout = default_timeout) reader =
  next_before ~deadline:(Time_ns.add (Time_ns.now ()) timeout) reader
;;

let until ?(timeout = default_timeout) reader ~f =
  let deadline = Time_ns.add (Time_ns.now ()) timeout in
  Deferred.repeat_until_finished () (fun () ->
    match%map next_before ~deadline reader with
    | None -> `Finished None
    | Some x -> if f x then `Finished (Some x) else `Repeat ())
;;
