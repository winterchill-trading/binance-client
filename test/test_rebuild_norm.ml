(* [Market_data.Rebuild_norm] with [Raw_normalizer] over raw minute files written from the
   fixtures the way market-recorder lays them out, then the event logs read back. A
   frame's [recv_ts] is its event time [E] (the time of the frame before it for
   acknowledgements). *)

open! Core
open Async
open Binance_client
module Md = Market_data

let date = Date.of_string "2026-10-12"

let with_temp_dir f =
  let%bind dir = Unix.mkdtemp (Filename.temp_dir_name ^/ "binance-client-test") in
  Monitor.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      Process.run_expect_no_stdout_exn ~prog:"rm" ~args:[ "-rf"; dir ] ())
;;

let event_time frame =
  let json = Yojson.Safe.from_string frame in
  let json =
    match Yojson.Safe.Util.member "data" json with
    | `Null -> json
    | data -> data
  in
  match Yojson.Safe.Util.member "E" json with
  | `Int ms -> Some (Time_ns.of_int_ns_since_epoch (ms * 1_000_000))
  | _ -> None
;;

let format_ts time = Time_ns.to_string_iso8601_basic time ~zone:Time_float.Zone.utc

(* One file per minute under raw/binance/<stream>/BTCUSDT/2026/10/12. *)
let write_raw ~data_dir stream =
  let frames = Fixture.frames stream in
  let first = List.find_map_exn frames ~f:event_time in
  let lines =
    List.folding_map frames ~init:first ~f:(fun prev frame ->
      let recv_ts =
        Option.value_map (event_time frame) ~default:prev ~f:(Time_ns.max prev)
      in
      recv_ts, (recv_ts, frame))
  in
  let dir =
    data_dir ^/ sprintf "raw/binance/%s/BTCUSDT/2026/10/12" (Md.Stream.to_string stream)
  in
  let%bind () = Unix.mkdir ~p:() dir in
  let minute (t, _) = Time_ns.to_int_ns_since_epoch t / 60_000_000_000 in
  List.group lines ~break:(fun a b -> minute a <> minute b)
  |> Deferred.List.iter ~how:`Sequential ~f:(fun group ->
    let minute =
      Time_ns.to_ofday (fst (List.hd_exn group)) ~zone:Time_float.Zone.utc
      |> Time_ns.Ofday.to_parts
    in
    let file = dir ^/ sprintf "%02d-%02d.ndjson" minute.hr minute.min in
    Writer.save
      file
      ~contents:
        (String.concat
           (List.map group ~f:(fun (recv_ts, frame) ->
              String.concat [ format_ts recv_ts; " "; frame; "\n" ]))))
;;

let%expect_test "rebuild norm from recorded raw files" =
  let%map () =
    with_temp_dir (fun dir ->
      let data_dir = dir ^/ "data" in
      let%bind () =
        Deferred.List.iter Fixture.streams ~how:`Sequential ~f:(write_raw ~data_dir)
      in
      let out_dir = dir ^/ "norm" in
      let%bind stats =
        Md.Rebuild_norm.run
          ~normalizers:[ (module Raw_normalizer) ]
          ~data_dir
          ~out_dir
          date
      in
      print_s [%sexp (stats : Md.Rebuild_norm.Stats.t Or_error.t)];
      Deferred.List.iter Fixture.streams ~how:`Sequential ~f:(fun stream ->
        let path =
          Md.Event_log.path
            ~dir:out_dir
            (Feed_normalizer.instrument (Binance_protocol.Symbol.of_string "BTCUSDT"))
            stream
            date
        in
        let%map events, tail = Md.Event_log.load_file path in
        let snapshots =
          List.count events ~f:(function
            | L2 { kind = Snapshot; _ } -> true
            | L2 { kind = Delta; _ } | Trade _ | Bbo _ | Funding _ -> false)
        in
        print_s
          [%message
            ""
              ~file:(String.chop_prefix_exn path ~prefix:(dir ^ "/") : string)
              ~events:(List.length events : int)
              (snapshots : int)
              (tail : Md.Framed.Tail.t)]))
  in
  [%expect
    {|
    (Ok
     ((files 5) (frames 459) (disconnects 0) (events 450) (frame_errors 0)
      (first_errors ()) (skipped_files 0) (skipped_venues ())))
    ((file norm/binance/BTCUSDT/norm/trades/2026-10-12.bin) (events 150)
     (snapshots 0) (tail Clean))
    ((file norm/binance/BTCUSDT/norm/bbo/2026-10-12.bin) (events 200)
     (snapshots 0) (tail Clean))
    ((file norm/binance/BTCUSDT/norm/l2/2026-10-12.bin) (events 41) (snapshots 1)
     (tail Clean))
    ((file norm/binance/BTCUSDT/norm/funding/2026-10-12.bin) (events 59)
     (snapshots 0) (tail Clean))
    |}]
;;

let%expect_test "an existing output directory is refused" =
  let%map () =
    with_temp_dir (fun dir ->
      let out_dir = dir ^/ "norm" in
      let%bind () = Unix.mkdir ~p:() (out_dir ^/ "binance") in
      let%map stats =
        Md.Rebuild_norm.run
          ~normalizers:[ (module Raw_normalizer) ]
          ~data_dir:dir
          ~out_dir
          date
      in
      print_s [%sexp (Result.is_error stats : bool)])
  in
  [%expect {| true |}]
;;
