(* The recorded frames of every stream through [Raw_normalizer], the adapter
   [rebuild-norm] uses: one fresh normalizer per stream, as per recorded series. *)

open! Core
open Binance_client

let recv_ts = Time_ns.epoch

let%expect_test "recorded frames through the Normalizer.S adapter" =
  let module N = (val (module Raw_normalizer : Market_data.Normalizer.S)) in
  print_s [%sexp (N.venue : Market_data.Venue.t)];
  List.iter Fixture.streams ~f:(fun stream ->
    let t = N.create () in
    let errors = ref 0 in
    let events =
      List.concat_map (Fixture.frames stream) ~f:(fun frame ->
        match N.frame t ~recv_ts frame with
        | Ok events -> events
        | Error _ ->
          incr errors;
          [])
    in
    let kinds =
      List.map events ~f:(fun (event : Market_data.Event.t) ->
        match event with
        | Trade _ -> "trade"
        | Bbo _ -> "bbo"
        | Funding _ -> "funding"
        | L2 { kind = Snapshot; _ } -> "snapshot"
        | L2 { kind = Delta; _ } -> "delta")
      |> List.sort_and_group ~compare:String.compare
      |> List.map ~f:(fun group -> List.hd_exn group, List.length group)
    in
    print_s
      [%message
        ""
          (stream : Market_data.Stream.t)
          (kinds : (string * int) list)
          ~errors:(!errors : int)]);
  [%expect
    {|
    Binance
    ((stream Trades) (kinds ((trade 150))) (errors 0))
    ((stream Bbo) (kinds ((bbo 200))) (errors 0))
    ((stream L2) (kinds ((delta 40) (snapshot 1))) (errors 0))
    ((stream Funding) (kinds ((funding 59))) (errors 0))
    |}]
;;

let%expect_test "the snapshot comes before the deltas it joins" =
  let t = Raw_normalizer.create () in
  let l2 =
    List.concat_map (Fixture.frames L2) ~f:(fun frame ->
      Raw_normalizer.frame t ~recv_ts frame |> ok_exn)
    |> List.filter_map ~f:(fun (event : Market_data.Event.t) ->
      match event with
      | L2 update -> Some update
      | Trade _ | Bbo _ | Funding _ -> None)
  in
  let first = List.hd_exn l2 in
  print_s
    [%message
      ""
        ~first_kind:(first.kind : Market_data.L2_update.Kind.t)
        ~bids:(List.length first.bids : int)
        ~asks:(List.length first.asks : int)];
  (* A disconnect drops the book: replaying the deltas alone gives nothing. *)
  Raw_normalizer.disconnect t;
  let after =
    List.filter (Fixture.frames L2) ~f:(Fn.non Fixture.is_snapshot)
    |> List.concat_map ~f:(fun frame -> Raw_normalizer.frame t ~recv_ts frame |> ok_exn)
  in
  print_s [%message "" ~events_after_disconnect:(List.length after : int)];
  [%expect
    {|
    ((first_kind Snapshot) (bids 100) (asks 100))
    (events_after_disconnect 0)
    |}]
;;
