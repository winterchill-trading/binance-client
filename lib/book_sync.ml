open! Core
open Async
open Binance_protocol

type t =
  { config : Config.t
  ; book : Local_book.t
  ; levels : int
  ; snapshot_limit : int
  ; retry_delay : Time_ns.Span.t
  ; error_delay : Time_ns.Span.t
  ; on_book : Exchange_core.L2.t -> unit
  ; mutable fetching : bool
  ; mutable closed : bool
  }

let create
  ?(levels = 50)
  ?(snapshot_limit = 1000)
  ?(retry_delay = Time_ns.Span.of_int_ms 200)
  ?(error_delay = Time_ns.Span.of_int_sec 1)
  config
  symbol
  ~on_book
  =
  { config
  ; book = Local_book.create symbol
  ; levels
  ; snapshot_limit
  ; retry_delay
  ; error_delay
  ; on_book
  ; fetching = false
  ; closed = false
  }
;;

let local_book t = t.book

let report t =
  if not t.closed then Option.iter (Local_book.l2 t.book ~depth:t.levels) ~f:t.on_book
;;

let rec fetch t =
  if t.closed || Local_book.is_synced t.book
  then (
    t.fetching <- false;
    return ())
  else (
    let symbol = Local_book.symbol t.book in
    match%bind Rest.depth ~limit:t.snapshot_limit t.config symbol with
    | Error error ->
      Log.Global.error_s
        [%message
          "binance book: depth snapshot failed" (symbol : Symbol.t) (error : Error.t)];
      let%bind () = Clock_ns.after t.error_delay in
      fetch t
    | Ok depth ->
      if t.closed || Local_book.is_synced t.book
      then fetch t
      else (
        match Local_book.apply_snapshot t.book depth with
        | `Synced ->
          report t;
          fetch t
        | `Gap ->
          Log.Global.debug_s
            [%message
              "binance book: snapshot does not join the stream, retrying"
                (symbol : Symbol.t)
                ~last_update_id:(depth.last_update_id : int)];
          let%bind () = Clock_ns.after t.retry_delay in
          fetch t))
;;

let start t =
  if (not t.fetching) && (not t.closed) && not (Local_book.is_synced t.book)
  then (
    t.fetching <- true;
    don't_wait_for (fetch t))
;;

let handle t (event : Depth_update.t) =
  if Symbol.equal event.symbol (Local_book.symbol t.book) && not t.closed
  then (
    match Local_book.apply_update t.book event with
    | Applied -> report t
    | Stale -> ()
    | Buffered -> start t
    | Gap ->
      Log.Global.info_s
        [%message
          "binance book: sequence gap, resyncing"
            ~symbol:(event.symbol : Symbol.t)
            ~pu:(event.prev_final_update_id : int)
            ~last:(Local_book.last_update_id t.book : int option)];
      start t)
;;

let reset t =
  Local_book.reset t.book;
  start t
;;

let close t = t.closed <- true
