(* Feed_normalizer per frame on the recorded mainnet frames (test/fixtures).

   [trades], [bbo] and [funding] cycle through their recorded frames, one frame per run.
   [l2 replay] is the whole depth recording per run: a fresh normalizer, the buffered
   diffs, the 100-level snapshot and 40 deltas (46 frames). The [yojson] rows run the same
   frames through [frame_reference] (no scanner); the [scan] rows only [Frame_scan.scan].
   Funding frames always take the Yojson path. *)

open! Core
open Core_bench
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"
let lines s = String.split_lines s |> List.filter ~f:(Fn.non String.is_empty)

let decode ~reference =
  if reference then Feed_normalizer.frame_reference else Feed_normalizer.frame
;;

let suffix ~reference = if reference then " (yojson)" else ""

let per_frame ?(reference = false) name text =
  let frames = Array.of_list (lines text) in
  let t = Feed_normalizer.create ~l2_symbols:[] in
  let decode = decode ~reference in
  let i = ref 0 in
  Bench.Test.create
    ~name:(name ^ suffix ~reference)
    (fun () ->
      let frame = frames.(!i) in
      i := (!i + 1) % Array.length frames;
      ignore
        (decode t ~recv_ts:Time_ns.epoch frame : Feed_normalizer.Output.t list Or_error.t))
;;

let scan_only name text =
  let frames =
    Array.of_list (lines text |> List.filter ~f:(String.is_prefix ~prefix:{|{"stream"|}))
  in
  let t = Frame_scan.create () in
  let i = ref 0 in
  Bench.Test.create ~name:(name ^ " (scan)") (fun () ->
    let frame = frames.(!i) in
    i := (!i + 1) % Array.length frames;
    ignore (Frame_scan.scan t frame : bool))
;;

let l2_replay ?(reference = false) () =
  let frames = lines Frames.l2 in
  let decode = decode ~reference in
  Bench.Test.create
    ~name:("l2 replay" ^ suffix ~reference)
    (fun () ->
      let t = Feed_normalizer.create ~l2_symbols:[ btc ] in
      List.iter frames ~f:(fun frame ->
        if String.is_prefix frame ~prefix:{|{"lastUpdateId"|}
        then
          ignore
            (Feed_normalizer.snapshot
               t
               ~recv_ts:Time_ns.epoch
               btc
               (Depth.t_of_yojson (Yojson.Safe.from_string frame))
             : [ `Synced of _ | `Gap | `Ignored ])
        else
          ignore
            (decode t ~recv_ts:Time_ns.epoch frame
             : Feed_normalizer.Output.t list Or_error.t)))
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ per_frame "trades" Frames.trades
       ; per_frame "trades" Frames.trades ~reference:true
       ; scan_only "trades" Frames.trades
       ; per_frame "bbo" Frames.bbo
       ; per_frame "bbo" Frames.bbo ~reference:true
       ; scan_only "bbo" Frames.bbo
       ; scan_only "depth" Frames.l2
       ; per_frame "funding" Frames.funding
       ; l2_replay ()
       ; l2_replay ~reference:true ()
       ])
;;
