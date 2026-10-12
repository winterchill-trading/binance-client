(* Feed_normalizer per frame on the recorded mainnet frames (test/fixtures).

   [trades], [bbo] and [funding] cycle through their recorded frames, one frame per run.
   [l2 replay] is the whole depth recording per run: a fresh normalizer, the buffered
   diffs, the 100-level snapshot and 40 deltas (46 frames). *)

open! Core
open Core_bench
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"
let lines s = String.split_lines s |> List.filter ~f:(Fn.non String.is_empty)

let per_frame name text =
  let frames = Array.of_list (lines text) in
  let t = Feed_normalizer.create ~l2_symbols:[] in
  let i = ref 0 in
  Bench.Test.create ~name (fun () ->
    let frame = frames.(!i) in
    i := (!i + 1) % Array.length frames;
    ignore
      (Feed_normalizer.frame t ~recv_ts:Time_ns.epoch frame
       : Feed_normalizer.Output.t list Or_error.t))
;;

let l2_replay () =
  let frames = lines Frames.l2 in
  Bench.Test.create ~name:"l2 replay" (fun () ->
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
          (Feed_normalizer.frame t ~recv_ts:Time_ns.epoch frame
           : Feed_normalizer.Output.t list Or_error.t)))
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ per_frame "trades" Frames.trades
       ; per_frame "bbo" Frames.bbo
       ; per_frame "funding" Frames.funding
       ; l2_replay ()
       ])
;;
