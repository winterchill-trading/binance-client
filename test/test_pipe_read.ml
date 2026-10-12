open! Core
open Async

let ms = Time_ns.Span.of_int_ms

let%expect_test "a timed out read does not swallow the next value" =
  let reader, writer = Pipe.create () in
  let%bind racy = Clock_ns.with_timeout (ms 10) (Pipe.read reader) in
  print_s [%message "with_timeout" ~timed_out:(Poly.equal racy `Timeout : bool)];
  Pipe.write_without_pushback writer 1;
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (* The abandoned read took 1. *)
  print_s [%message "" ~left_in_pipe:(Pipe.length reader : int)];
  let%bind first = Pipe_read.next ~timeout:(ms 10) reader in
  Pipe.write_without_pushback writer 2;
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  let%bind second = Pipe_read.next ~timeout:(ms 10) reader in
  print_s [%message "Pipe_read" (first : int option) (second : int option)];
  Pipe.write_without_pushback writer 3;
  Pipe.write_without_pushback writer 4;
  let%bind four = Pipe_read.until reader ~f:(Int.equal 4) in
  print_s [%message (four : int option)];
  [%expect
    {|
    (with_timeout (timed_out true))
    (left_in_pipe 0)
    (Pipe_read (first ()) (second (2)))
    (four (4))
    |}];
  return ()
;;
