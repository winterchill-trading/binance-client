open! Core
open Binance_protocol

module Outcome = struct
  type t =
    | Applied
    | Buffered
    | Stale
    | Gap
  [@@deriving sexp, compare, equal]
end

type book =
  { mutable bids : Size.t Price.Map.t
  ; mutable asks : Size.t Price.Map.t
  ; mutable last : int
  ; mutable expect_first : bool
  ; mutable time : Timestamp_ms.t
  }

type state =
  | Waiting of Depth_update.t Queue.t
  | Synced of book

type t =
  { symbol : Symbol.t
  ; max_buffered : int
  ; mutable state : state
  }

let create ?(max_buffered = 1000) symbol =
  { symbol; max_buffered; state = Waiting (Queue.create ()) }
;;

let symbol t = t.symbol

let is_synced t =
  match t.state with
  | Synced _ -> true
  | Waiting _ -> false
;;

let last_update_id t =
  match t.state with
  | Synced book -> Some book.last
  | Waiting _ -> None
;;

let num_buffered t =
  match t.state with
  | Synced _ -> 0
  | Waiting q -> Queue.length q
;;

let reset t = t.state <- Waiting (Queue.create ())

let buffer t queue event =
  Queue.enqueue queue event;
  while Queue.length queue > t.max_buffered do
    ignore (Queue.dequeue_exn queue : Depth_update.t)
  done
;;

let set_levels side (levels : Book_level.t list) =
  List.fold levels ~init:side ~f:(fun side { price; qty } ->
    if Size.is_zero qty then Map.remove side price else Map.set side ~key:price ~data:qty)
;;

let apply book (e : Depth_update.t) =
  book.bids <- set_levels book.bids e.bids;
  book.asks <- set_levels book.asks e.asks;
  book.last <- e.final_update_id;
  book.expect_first <- false;
  book.time <- e.transaction_time
;;

(* Applies [e] to a synced book; on a gap the caller drops the book. *)
let step book (e : Depth_update.t) : Outcome.t =
  let u = e.final_update_id in
  if book.expect_first
  then
    if u < book.last
    then Stale
    else if (e.first_update_id <= book.last && book.last <= u)
            || e.prev_final_update_id = book.last
    then (
      apply book e;
      Applied)
    else Gap
  else if e.prev_final_update_id = book.last
  then (
    apply book e;
    Applied)
  else if u <= book.last
  then Stale
  else Gap
;;

let drop_book t event =
  let queue = Queue.create () in
  t.state <- Waiting queue;
  buffer t queue event
;;

let apply_update t (e : Depth_update.t) : Outcome.t =
  match t.state with
  | Waiting queue ->
    buffer t queue e;
    Buffered
  | Synced book ->
    (match step book e with
     | Gap ->
       drop_book t e;
       Gap
     | outcome -> outcome)
;;

let apply_snapshot t (depth : Depth.t) =
  let pending =
    match t.state with
    | Waiting queue -> Queue.to_list queue
    | Synced _ -> []
  in
  let side levels = set_levels Price.Map.empty levels in
  let book =
    { bids = side depth.bids
    ; asks = side depth.asks
    ; last = depth.last_update_id
    ; expect_first = true
    ; time = depth.transaction_time
    }
  in
  t.state <- Synced book;
  let rec replay = function
    | [] -> `Synced
    | e :: rest ->
      (match apply_update t e with
       | Gap ->
         (match t.state with
          | Waiting queue -> List.iter rest ~f:(buffer t queue)
          | Synced _ -> ());
         `Gap
       | Applied | Stale | Buffered -> replay rest)
  in
  replay pending
;;

let l2 ?depth t =
  match t.state with
  | Waiting _ -> None
  | Synced book ->
    let take side ~order =
      let levels = Map.to_sequence side ~order in
      let levels =
        match depth with
        | None -> levels
        | Some n -> Sequence.take levels n
      in
      Sequence.to_list levels
      |> List.map ~f:(fun (px, sz) -> { Exchange_core.L2.Level.px; sz; n = 0 })
    in
    Some
      { Exchange_core.L2.symbol = t.symbol
      ; time = Time_ns.to_int_ns_since_epoch (Timestamp_ms.to_time_ns book.time)
      ; bids = take book.bids ~order:`Decreasing_key
      ; asks = take book.asks ~order:`Increasing_key
      }
;;
