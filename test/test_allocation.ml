(* Minor words allocated per Live_feed message: what [Feed_normalizer.frame] (JSON decode
   plus conversion, the work [Live_feed] does per websocket frame) allocates on the
   recorded mainnet frames. The timing benchmark is in bench/. *)

open! Core
open Binance_protocol
open Binance_client

let btc = Symbol.of_string "BTCUSDT"

let words f =
  let before = Gc.minor_words () in
  f ();
  Gc.minor_words () - before
;;

let%expect_test "minor words per Live_feed message" =
  List.iter Fixture.streams ~f:(fun stream ->
    let t = Feed_normalizer.create ~l2_symbols:[ btc ] in
    let frames = Fixture.frames stream in
    let per_frame =
      List.filter_map frames ~f:(fun frame ->
        if Fixture.is_snapshot frame
        then (
          let depth = Depth.t_of_yojson (Yojson.Safe.from_string frame) in
          ignore
            (Feed_normalizer.snapshot t ~recv_ts:Time_ns.epoch btc depth
             : [ `Synced of _ | `Gap | `Ignored ]);
          None)
        else
          Some
            ( words (fun () ->
                ignore
                  (Sys.opaque_identity
                     (Feed_normalizer.frame t ~recv_ts:Time_ns.epoch frame)
                   : Feed_normalizer.Output.t list Or_error.t))
            , String.length frame ))
    in
    let n = List.length per_frame in
    let total = List.sum (module Int) per_frame ~f:fst in
    let bytes = List.sum (module Int) per_frame ~f:snd in
    let max = List.fold per_frame ~init:0 ~f:(fun acc (w, _) -> Int.max acc w) in
    print_s
      [%message
        ""
          (stream : Market_data.Stream.t)
          ~frames:(n : int)
          ~words_per_frame:(total / n : int)
          ~max_words:(max : int)
          ~bytes_per_frame:(bytes / n : int)]);
  [%expect
    {|
    ((stream Trades) (frames 151) (words_per_frame 436) (max_words 439)
     (bytes_per_frame 203))
    ((stream Bbo) (frames 201) (words_per_frame 416) (max_words 418)
     (bytes_per_frame 201))
    ((stream L2) (frames 46) (words_per_frame 21156) (max_words 116600)
     (bytes_per_frame 4647))
    ((stream Funding) (frames 60) (words_per_frame 415) (max_words 421)
     (bytes_per_frame 219))
    |}]
;;
