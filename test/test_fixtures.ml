(* A trimmed minute of recorded mainnet frames per stream, decoded and normalized the way
   [Live_feed] does it. *)

open! Core
open Binance_protocol
open Binance_client

let recv_ts = Time_ns.epoch
let btc = Symbol.of_string "BTCUSDT"

type counts =
  { frames : int
  ; acks : int
  ; errors : int
  ; trades : int
  ; bbos : int
  ; fundings : int
  ; snapshots : int
  ; deltas : int
  ; need_snapshot : int
  }
[@@deriving sexp_of]

let zero =
  { frames = 0
  ; acks = 0
  ; errors = 0
  ; trades = 0
  ; bbos = 0
  ; fundings = 0
  ; snapshots = 0
  ; deltas = 0
  ; need_snapshot = 0
  }
;;

let count_event c (event : Market_data.Event.t) =
  match event with
  | Trade _ -> { c with trades = c.trades + 1 }
  | Bbo _ -> { c with bbos = c.bbos + 1 }
  | Funding _ -> { c with fundings = c.fundings + 1 }
  | L2 { kind = Snapshot; _ } -> { c with snapshots = c.snapshots + 1 }
  | L2 { kind = Delta; _ } -> { c with deltas = c.deltas + 1 }
;;

let replay stream =
  let t = Feed_normalizer.create ~l2_symbols:[ btc ] in
  List.fold (Fixture.frames stream) ~init:zero ~f:(fun c frame ->
    let c = { c with frames = c.frames + 1 } in
    if Fixture.is_snapshot frame
    then (
      let depth = Depth.t_of_yojson (Yojson.Safe.from_string frame) in
      match Feed_normalizer.snapshot t ~recv_ts btc depth with
      | `Synced update -> count_event c (L2 update)
      | `Gap | `Ignored -> { c with errors = c.errors + 1 })
    else (
      match Feed_normalizer.frame t ~recv_ts frame with
      | Error _ -> { c with errors = c.errors + 1 }
      | Ok [] -> { c with acks = c.acks + 1 }
      | Ok outputs ->
        List.fold outputs ~init:c ~f:(fun c -> function
          | Event event -> count_event c event
          | Need_snapshot _ -> { c with need_snapshot = c.need_snapshot + 1 })))
;;

let%expect_test "every recorded frame decodes" =
  List.iter Fixture.streams ~f:(fun stream ->
    print_s [%message "" (stream : Market_data.Stream.t) ~_:(replay stream : counts)]);
  [%expect
    {|
    ((stream Trades)
     ((frames 151) (acks 1) (errors 0) (trades 150) (bbos 0) (fundings 0)
      (snapshots 0) (deltas 0) (need_snapshot 0)))
    ((stream Bbo)
     ((frames 201) (acks 1) (errors 0) (trades 0) (bbos 200) (fundings 0)
      (snapshots 0) (deltas 0) (need_snapshot 0)))
    ((stream L2)
     ((frames 47) (acks 1) (errors 0) (trades 0) (bbos 0) (fundings 0)
      (snapshots 1) (deltas 40) (need_snapshot 5)))
    ((stream Funding)
     ((frames 60) (acks 1) (errors 0) (trades 0) (bbos 0) (fundings 59)
      (snapshots 0) (deltas 0) (need_snapshot 0)))
    |}]
;;
