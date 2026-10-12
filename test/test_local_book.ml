open! Core
open Async
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"
let level (px, qty) = { Book_level.price = Price.of_string px; qty = Size.of_string qty }

let snapshot ~last ?(bids = []) ?(asks = []) () =
  { Depth.last_update_id = last
  ; event_time = Timestamp_ms.of_int 1
  ; transaction_time = Timestamp_ms.of_int 1
  ; bids = List.map bids ~f:level
  ; asks = List.map asks ~f:level
  }
;;

let event ~first ~last ~prev ?(bids = []) ?(asks = []) () =
  { Depth_update.event_time = Timestamp_ms.of_int last
  ; transaction_time = Timestamp_ms.of_int last
  ; symbol = btc
  ; first_update_id = first
  ; final_update_id = last
  ; prev_final_update_id = prev
  ; bids = List.map bids ~f:level
  ; asks = List.map asks ~f:level
  }
;;

let show_book t =
  match Local_book.l2 t ~depth:3 with
  | None -> print_endline "(not synced)"
  | Some l2 ->
    let side levels =
      List.map levels ~f:(fun (l : Exchange_core.L2.Level.t) ->
        Price.to_string l.px ^ "x" ^ Size.to_string l.sz)
      |> String.concat ~sep:" "
    in
    printf
      "last=%s bids=[%s] asks=[%s]\n"
      (Option.value_map (Local_book.last_update_id t) ~default:"-" ~f:Int.to_string)
      (side l2.bids)
      (side l2.asks)
;;

let update t e =
  let outcome = Local_book.apply_update t e in
  print_s [%sexp (outcome : Local_book.Outcome.t)];
  show_book t
;;

let resync t depth =
  let result = Local_book.apply_snapshot t depth in
  print_s [%sexp (result : [ `Synced | `Gap ])];
  show_book t
;;

let%expect_test "buffer, snapshot, replay and continuous updates" =
  let t = Local_book.create btc in
  (* Events arrive before the snapshot and are buffered. *)
  update t (event ~first:8 ~last:9 ~prev:7 ~bids:[ "99", "5" ] ());
  update t (event ~first:10 ~last:12 ~prev:9 ~asks:[ "101", "2" ] ());
  update t (event ~first:13 ~last:15 ~prev:12 ~bids:[ "100", "1" ] ());
  print_s [%sexp (Local_book.num_buffered t : int)];
  [%expect
    {|
    Buffered
    (not synced)
    Buffered
    (not synced)
    Buffered
    (not synced)
    3
    |}];
  (* Snapshot at 11: the event ending at 9 is dropped, [10, 12] straddles it. *)
  resync
    t
    (snapshot ~last:11 ~bids:[ "99", "4"; "98", "3" ] ~asks:[ "101", "1"; "102", "7" ] ());
  [%expect {|
    Synced
    last=15 bids=[100x1 99x4 98x3] asks=[101x2 102x7]
    |}];
  (* Zero removes a level; removing an unknown level is fine. *)
  update t (event ~first:16 ~last:16 ~prev:15 ~bids:[ "100", "0"; "50", "0" ] ());
  [%expect {|
    Applied
    last=16 bids=[99x4 98x3] asks=[101x2 102x7]
    |}];
  (* A duplicate is ignored. *)
  update t (event ~first:16 ~last:16 ~prev:15 ~bids:[ "100", "9" ] ());
  [%expect {|
    Stale
    last=16 bids=[99x4 98x3] asks=[101x2 102x7]
    |}];
  return ()
;;

let%expect_test "a gap drops the book until a newer snapshot" =
  let t = Local_book.create btc in
  resync t (snapshot ~last:20 ~bids:[ "99", "1" ] ~asks:[ "101", "1" ] ());
  update t (event ~first:21 ~last:22 ~prev:20 ~bids:[ "99", "2" ] ());
  [%expect
    {|
    Synced
    last=20 bids=[99x1] asks=[101x1]
    Applied
    last=22 bids=[99x2] asks=[101x1]
    |}];
  (* [pu = 25] but the book is at 22: events 23..25 were lost. *)
  update t (event ~first:26 ~last:27 ~prev:25 ~asks:[ "100.5", "3" ] ());
  update t (event ~first:28 ~last:28 ~prev:27 ~bids:[ "99.5", "1" ] ());
  [%expect {|
    Gap
    (not synced)
    Buffered
    (not synced)
    |}];
  (* A snapshot older than the buffered events cannot be joined. *)
  resync t (snapshot ~last:22 ~bids:[ "99", "2" ] ~asks:[ "101", "1" ] ());
  [%expect {|
    Gap
    (not synced)
    |}];
  (* A newer one can: 26..27 straddles 26, then 28 continues. *)
  resync t (snapshot ~last:26 ~bids:[ "99", "2" ] ~asks:[ "101", "1" ] ());
  [%expect {|
    Synced
    last=28 bids=[99.5x1 99x2] asks=[100.5x3 101x1]
    |}];
  return ()
;;

let%expect_test "an idle snapshot is continued by pu = lastUpdateId" =
  let t = Local_book.create btc in
  resync t (snapshot ~last:5 ~bids:[ "99", "1" ] ());
  update t (event ~first:6 ~last:6 ~prev:5 ~asks:[ "101", "1" ] ());
  [%expect
    {|
    Synced
    last=5 bids=[99x1] asks=[]
    Applied
    last=6 bids=[99x1] asks=[101x1]
    |}];
  (* After a reset (e.g. a reconnect) events buffer again. *)
  Local_book.reset t;
  update t (event ~first:7 ~last:7 ~prev:6 ~bids:[ "99", "0" ] ());
  [%expect {|
    Buffered
    (not synced)
    |}];
  return ()
;;

let%expect_test "the buffer is bounded" =
  let t = Local_book.create ~max_buffered:2 btc in
  List.iter [ 1; 2; 3 ] ~f:(fun i ->
    ignore
      (Local_book.apply_update t (event ~first:i ~last:i ~prev:(i - 1) ())
       : Local_book.Outcome.t));
  print_s [%sexp (Local_book.num_buffered t : int)];
  (* Event 1 was dropped, so a snapshot at 0 cannot be joined. *)
  resync t (snapshot ~last:0 ());
  [%expect {|
    2
    Gap
    (not synced)
    |}];
  return ()
;;
