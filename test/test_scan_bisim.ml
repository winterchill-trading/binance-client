(* The scanned path of Feed_normalizer.frame against the Yojson path it replaced
   (frame_reference): every recorded frame, in both normalizer modes, and randomly
   corrupted frames. *)

open! Core
open Binance_protocol
open Binance_client

let recv_ts = Time_ns.of_int_ns_since_epoch 1_791_817_545_000_000_000
let btc = Symbol.of_string "BTCUSDT"
let show result = [%sexp (result : Feed_normalizer.Output.t list Or_error.t)]

(* Runs the frames through a [frame] normalizer and a [frame_reference] normalizer side by
   side; returns how many outputs were compared. *)
let run ~create ~snapshots_through_frame frames =
  let fast = create ()
  and reference = create () in
  List.fold frames ~init:0 ~f:(fun n frame ->
    if Fixture.is_snapshot frame && not snapshots_through_frame
    then (
      let depth = Depth.t_of_yojson (Yojson.Safe.from_string frame) in
      let a = Feed_normalizer.snapshot fast ~recv_ts btc depth
      and b = Feed_normalizer.snapshot reference ~recv_ts btc depth in
      let sexp = [%sexp_of: [ `Synced of Market_data.L2_update.t | `Gap | `Ignored ]] in
      [%test_result: Sexp.t] (sexp a) ~expect:(sexp b);
      n + 1)
    else (
      let a = show (Feed_normalizer.frame fast ~recv_ts frame)
      and b = show (Feed_normalizer.frame_reference reference ~recv_ts frame) in
      if not (Sexp.equal a b)
      then raise_s [%message "paths disagree" frame (a : Sexp.t) (b : Sexp.t)];
      n + 1))
;;

let%expect_test "every recorded frame gives the same outputs on both paths" =
  List.iter Fixture.streams ~f:(fun stream ->
    let frames = Fixture.frames stream in
    let live =
      run
        ~create:(fun () -> Feed_normalizer.create ~l2_symbols:[ btc ])
        ~snapshots_through_frame:false
        frames
    in
    (* Replay normalizers take the recorded snapshot bodies as frames. *)
    let replay =
      run ~create:Feed_normalizer.create_replay ~snapshots_through_frame:true frames
    in
    print_s [%message (stream : Market_data.Stream.t) (live : int) (replay : int)]);
  [%expect
    {|
    ((stream Trades) (live 151) (replay 151))
    ((stream Bbo) (live 201) (replay 201))
    ((stream L2) (live 47) (replay 47))
    ((stream Funding) (live 60) (replay 60))
    |}]
;;

(* Corrupt one byte, drop one or duplicate one. The scanner must either reject the frame
   or read what Yojson reads. *)
let mutate =
  let open Quickcheck.Generator.Let_syntax in
  let chars = {|"{}[],:.-+0123456789eEtrufalsn \x|} in
  fun frame ->
    let n = String.length frame in
    let%bind pos = Int.gen_incl 0 (n - 1)
    and c = Int.gen_incl 0 (String.length chars - 1) >>| String.get chars
    and op = Int.gen_incl 0 2 in
    return
      (match op with
       | 0 -> String.mapi frame ~f:(fun i x -> if i = pos then c else x)
       | 1 -> String.prefix frame pos ^ String.drop_prefix frame (pos + 1)
       | _ -> String.prefix frame pos ^ String.of_char c ^ String.drop_prefix frame pos)
;;

let%expect_test "corrupted frames" =
  let scan = Frame_scan.create () in
  let accepted = ref 0 in
  List.iter [ Market_data.Stream.Trades; Bbo; L2 ] ~f:(fun stream ->
    let frames =
      Fixture.frames stream
      |> List.filter ~f:(fun f -> String.is_prefix f ~prefix:{|{"stream"|})
      |> Fn.flip List.take 3
    in
    List.iter frames ~f:(fun frame ->
      Quickcheck.test
        (mutate frame)
        ~trials:3_000
        ~sexp_of:String.sexp_of_t
        ~f:(fun frame ->
          (* A fresh replay normalizer per frame: a depth update then only asks for a
             snapshot, so its levels are compared below instead. *)
          let a =
            show (Feed_normalizer.frame (Feed_normalizer.create_replay ()) ~recv_ts frame)
          and b =
            show
              (Feed_normalizer.frame_reference
                 (Feed_normalizer.create_replay ())
                 ~recv_ts
                 frame)
          in
          if not (Sexp.equal a b)
          then raise_s [%message "paths disagree" frame (a : Sexp.t) (b : Sexp.t)];
          if Frame_scan.scan scan frame
          then (
            incr accepted;
            match scan.kind with
            | Agg_trade | Book_ticker -> ()
            | Depth_update ->
              let json = Yojson.Safe.from_string frame in
              (match (Combined.t_of_yojson Market_event.t_of_yojson json).data with
               | Depth_update e ->
                 let levels pos =
                   let rec go pos acc =
                     let next = Frame_scan.next_level scan frame pos in
                     if next < 0
                     then List.rev acc
                     else
                       go
                         next
                         ({ Book_level.price = Price.of_units scan.level_price
                          ; qty = Size.of_units scan.level_qty
                          }
                          :: acc)
                   in
                   go pos []
                 in
                 [%test_result: Book_level.t list] (levels scan.bids) ~expect:e.bids;
                 [%test_result: Book_level.t list] (levels scan.asks) ~expect:e.asks;
                 [%test_result: int] scan.first_update_id ~expect:e.first_update_id;
                 [%test_result: int] scan.final_update_id ~expect:e.final_update_id;
                 [%test_result: int]
                   scan.prev_final_update_id
                   ~expect:e.prev_final_update_id;
                 [%test_result: int]
                   scan.event_time
                   ~expect:(Timestamp_ms.to_int e.event_time)
               | _ ->
                 raise_s [%message "scanned a depth update Yojson does not see" frame])))));
  print_s [%message "" ~mutants:(9 * 3_000 : int) ~scanned:(!accepted : int)];
  [%expect {| ((mutants 27000) (scanned 8054)) |}]
;;
