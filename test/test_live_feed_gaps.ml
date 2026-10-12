(* [Live_feed.feed_with_gaps]: resync gaps on a sequence break and on a reconnect, with a
   scripted local websocket and depth server on a synthetic clock. *)

open! Core
open Async
open Binance_client

let depth_update ~first ~final ~prev =
  sprintf
    {|{"stream":"btcusdt@depth@100ms","data":{"e":"depthUpdate","E":%d,"T":%d,"s":"BTCUSDT","U":%d,"u":%d,"pu":%d,"b":[["99.0","%d"]],"a":[]}}|}
    (1_760_000_000_000 + final)
    (1_760_000_000_000 + final)
    first
    final
    prev
    final
;;

let depth ~last_update_id =
  sprintf
    {|{"lastUpdateId":%d,"E":1760000000010,"T":1760000000009,"bids":[["100.0","1"]],"asks":[["101.0","3"]]}|}
    last_update_id
;;

let ms ts = Time_ns.to_int_ns_since_epoch ts / 1_000_000

let print_gap (gap : Market_data.Gap.t option) =
  match gap with
  | None -> print_endline "no gap"
  | Some gap ->
    print_s
      [%message
        "gap"
          ~recv_ts_ms:(ms gap.recv_ts : int)
          ~instrument:(gap.instrument : Market_data.Instrument.t option)
          ~stream:(gap.stream : Market_data.Stream.t option)
          ~reason:(gap.reason : Market_data.Gap.Reason.t)]
;;

let print_l2 (update : Market_data.L2_update.t option) =
  match update with
  | None -> print_endline "no l2"
  | Some update ->
    print_s
      [%message
        ""
          ~kind:(update.kind : Market_data.L2_update.Kind.t)
          ~recv_ts_ms:(ms update.recv_ts : int)]
;;

let%expect_test "a sequence break and a reconnect each report a Resync gap" =
  Log.Global.set_output [];
  let%bind server = Fake_ws.start () in
  let clock = Time_source.create ~now:Time_ns.epoch () in
  let%bind live =
    Live_feed.start
      ~time_source:(Time_source.read_only clock)
      (Fake_ws.config server)
      ~symbols:[ "BTCUSDT" ]
      ~streams:[ L2 ]
  in
  let module F = (val Live_feed.feed_with_gaps live : Market_data.Feed.With_gaps.S) in
  let no_gap_yet () =
    match Pipe.read_now F.gaps with
    | `Ok gap -> print_gap (Some gap)
    | `Nothing_available -> print_endline "no gap"
    | `Eof -> print_endline "eof"
  in
  let subscribes () =
    List.count (Fake_ws.received server) ~f:(String.is_substring ~substring:"SUBSCRIBE")
  in
  let%bind (_ : bool) = Fake_ws.wait_until (fun () -> subscribes () = 1) in
  (* Start: the first sync is not a gap. *)
  Fake_ws.set_depth server (depth ~last_update_id:7);
  Fake_ws.send server (depth_update ~first:5 ~final:9 ~prev:4);
  let%bind () = Pipe_read.next F.l2 >>| print_l2 in
  Fake_ws.send server (depth_update ~first:10 ~final:12 ~prev:9);
  let%bind () = Pipe_read.next F.l2 >>| print_l2 in
  no_gap_yet ();
  [%expect
    {|
    ((kind Snapshot) (recv_ts_ms 0))
    ((kind Delta) (recv_ts_ms 0))
    no gap
    |}];
  (* A sequence break ([pu] 29 after [u] 12): one gap for the symbol, then a resync. *)
  let%bind () = Time_source.advance_by_alarms_by clock (Time_ns.Span.of_int_ms 100) in
  Fake_ws.set_depth server (depth ~last_update_id:30);
  Fake_ws.send server (depth_update ~first:30 ~final:32 ~prev:29);
  let%bind () = Pipe_read.next F.gaps >>| print_gap in
  let%bind () = Pipe_read.next F.l2 >>| print_l2 in
  [%expect
    {|
    (gap (recv_ts_ms 100) (instrument (((venue Binance) (symbol BTCUSDT))))
     (stream (L2)) (reason Resync))
    ((kind Snapshot) (recv_ts_ms 100))
    |}];
  (* A dropped connection: one gap for the stream once reconnected, and the book resyncs
     without a second, per-symbol gap. *)
  let%bind () = Fake_ws.drop server in
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 50) in
  let%bind () = Time_source.advance_by_alarms_by clock (Time_ns.Span.of_int_ms 600) in
  let%bind (_ : bool) = Fake_ws.wait_until (fun () -> subscribes () = 2) in
  let%bind () = Pipe_read.next F.gaps >>| print_gap in
  Fake_ws.set_depth server (depth ~last_update_id:40);
  Fake_ws.send server (depth_update ~first:40 ~final:41 ~prev:39);
  let%bind () = Pipe_read.next F.l2 >>| print_l2 in
  no_gap_yet ();
  print_s
    [%message
      ""
        ~depth_requests:(Fake_ws.depth_requests server : int)
        ~stats:(Live_feed.stats live : Live_feed.Stats.t)];
  [%expect
    {|
    (gap (recv_ts_ms 700) (instrument ()) (stream (L2)) (reason Resync))
    ((kind Snapshot) (recv_ts_ms 700))
    no gap
    ((depth_requests 3) (stats ((frames 4) (parse_errors 0) (reconnects 1))))
    |}];
  let%bind () = F.close () in
  print_s [%sexp (Pipe.is_closed F.gaps : bool)];
  [%expect {| true |}];
  Fake_ws.close server
;;
