(* Binance frames (shapes from binance-protocol's spec) into market-data records. *)

open! Core
open Binance_protocol
open Binance_client
module N = Feed_normalizer

let recv_ts = Time_ns.of_string_with_utc_offset "2026-10-12 00:00:00Z"
let btc = Symbol.of_string "BTCUSDT"

(* Time_ns sexps use the local zone; print every timestamp in UTC so the output does not
   depend on the machine. *)
let rec utc : Sexp.t -> Sexp.t = function
  | List [ Atom date; Atom time ] as sexp ->
    (match
       Option.try_with (fun () -> Time_ns.of_string_with_utc_offset (date ^ " " ^ time))
     with
     | Some t -> Atom (Time_ns.to_string_utc t)
     | None -> sexp)
  | List l -> List (List.map l ~f:utc)
  | Atom _ as atom -> atom
;;

let print_utc sexp = print_s (utc sexp)

let show t frame =
  match N.frame t ~recv_ts frame with
  | Ok outputs ->
    List.iter outputs ~f:(function
      | Event event -> print_utc [%sexp (event : Market_data.Event.t)]
      | Need_snapshot symbol -> print_s [%message "need snapshot" (symbol : Symbol.t)])
  | Error _ -> print_endline "error"
;;

let%expect_test "aggTrade, bookTicker and markPriceUpdate" =
  let t = N.create ~l2_symbols:[] in
  show
    t
    {|{"stream":"btcusdt@aggTrade","data":{"e":"aggTrade","E":123456789,"s":"BTCUSDT","a":5933014,"p":"0.001","q":"100","f":100,"l":105,"T":123456785,"m":true}}|};
  [%expect
    {|
    (Trade
     ((instrument ((venue Binance) (symbol BTCUSDT)))
      (exch_ts "1970-01-02 10:17:36.785000000Z")
      (recv_ts "2026-10-12 00:00:00.000000000Z") (price 0.001) (size 100)
      (side Sell) (trade_id 5933014)))
    |}];
  show
    t
    {|{"stream":"bnbusdt@bookTicker","data":{"e":"bookTicker","u":400900217,"E":1568014460893,"T":1568014460891,"s":"BNBUSDT","b":"25.35190000","B":"31.21000000","a":"25.36520000","A":"40.66000000"}}|};
  [%expect
    {|
    (Bbo
     ((instrument ((venue Binance) (symbol BNBUSDT)))
      (exch_ts "2019-09-09 07:34:20.891000000Z")
      (recv_ts "2026-10-12 00:00:00.000000000Z") (bid_price 25.3519)
      (bid_size 31.21) (ask_price 25.3652) (ask_size 40.66)))
    |}];
  show
    t
    {|{"stream":"btcusdt@markPrice@1s","data":{"e":"markPriceUpdate","E":1562305380000,"s":"BTCUSDT","p":"11794.15000000","i":"11784.62659091","P":"11784.25641265","r":"0.00038167","T":1562306400000}}|};
  [%expect
    {|
    (Funding
     ((instrument ((venue Binance) (symbol BTCUSDT)))
      (exch_ts "2019-07-05 05:43:00.000000000Z")
      (recv_ts "2026-10-12 00:00:00.000000000Z") (rate 0.00038167)
      (next_funding_time "2019-07-05 06:00:00.000000000Z") (mark_price 11794.15)
      (index_price 11784.62659091)))
    |}];
  (* A bare payload (a /ws connection) decodes too; [i] missing falls back to the mark. *)
  show
    t
    {|{"e":"markPriceUpdate","E":1562305380000,"s":"BTCUSDT","p":"11794.15","r":"-0.0001","T":1562306400000}|};
  [%expect
    {|
    (Funding
     ((instrument ((venue Binance) (symbol BTCUSDT)))
      (exch_ts "2019-07-05 05:43:00.000000000Z")
      (recv_ts "2026-10-12 00:00:00.000000000Z") (rate -0.0001)
      (next_funding_time "2019-07-05 06:00:00.000000000Z") (mark_price 11794.15)
      (index_price 11794.15)))
    |}]
;;

let%expect_test "acknowledgements, errors and other events" =
  let t = N.create ~l2_symbols:[ btc ] in
  show t {|{"result":null,"id":1}|};
  show t {|{"code":2,"msg":"Invalid request","id":3}|};
  show t {|not json|};
  show t {|{"stream":"btcusdt@kline_1m","data":{"e":"kline","E":1,"s":"BTCUSDT"}}|};
  [%expect {|
    error
    error
    |}]
;;

let depth_update ~first ~final ~prev ~bids ~asks =
  sprintf
    {|{"stream":"btcusdt@depth@100ms","data":{"e":"depthUpdate","E":%d,"T":%d,"s":"BTCUSDT","U":%d,"u":%d,"pu":%d,"b":%s,"a":%s}}|}
    (1_760_000_000_000 + final)
    (1_760_000_000_000 + final)
    first
    final
    prev
    bids
    asks
;;

let depth ~last_update_id =
  Depth.t_of_yojson
    (Yojson.Safe.from_string
       (sprintf
          {|{"lastUpdateId":%d,"E":1760000000010,"T":1760000000009,"bids":[["100.0","1"],["99.5","2"]],"asks":[["101.0","3"]]}|}
          last_update_id))
;;

let%expect_test "diff depth: buffered until a snapshot, then deltas; a gap resyncs" =
  let t = N.create ~l2_symbols:[ btc ] in
  show t (depth_update ~first:5 ~final:9 ~prev:4 ~bids:{|[["100.0","0"]]|} ~asks:"[]");
  show t (depth_update ~first:10 ~final:12 ~prev:9 ~bids:{|[["99.0","4"]]|} ~asks:"[]");
  [%expect
    {|
    ("need snapshot" (symbol BTCUSDT))
    ("need snapshot" (symbol BTCUSDT))
    |}];
  let snapshot depth =
    match N.snapshot t ~recv_ts btc depth with
    | `Synced update -> print_utc [%sexp (update : Market_data.L2_update.t)]
    | `Gap -> print_endline "gap"
    | `Ignored -> print_endline "ignored"
  in
  (* Taken after the first event: it joins, and the second is replayed into it. *)
  snapshot (depth ~last_update_id:7);
  [%expect
    {|
    ((instrument ((venue Binance) (symbol BTCUSDT)))
     (exch_ts "2025-10-09 08:53:20.012000000Z")
     (recv_ts "2026-10-12 00:00:00.000000000Z") (kind Snapshot)
     (bids (((price 99.5) (size 2)) ((price 99) (size 4))))
     (asks (((price 101) (size 3)))))
    |}];
  print_s [%sexp (N.is_synced t btc : bool)];
  snapshot (depth ~last_update_id:7);
  [%expect {|
    true
    ignored
    |}];
  show
    t
    (depth_update ~first:13 ~final:14 ~prev:12 ~bids:"[]" ~asks:{|[["101.0","0.5"]]|});
  [%expect
    {|
    (L2
     ((instrument ((venue Binance) (symbol BTCUSDT)))
      (exch_ts "2025-10-09 08:53:20.014000000Z")
      (recv_ts "2026-10-12 00:00:00.000000000Z") (kind Delta) (bids ())
      (asks (((price 101) (size 0.5))))))
    |}];
  (* [pu] does not continue the last [u]: the book is dropped. *)
  show t (depth_update ~first:20 ~final:21 ~prev:19 ~bids:"[]" ~asks:"[]");
  print_s [%sexp (N.is_synced t btc : bool)];
  [%expect {|
    ("need snapshot" (symbol BTCUSDT))
    false
    |}];
  (* Other symbols have no book. *)
  show
    t
    {|{"stream":"ethusdt@depth@100ms","data":{"e":"depthUpdate","E":1,"T":1,"s":"ETHUSDT","U":1,"u":2,"pu":0,"b":[],"a":[]}}|};
  print_endline
    (match
       N.snapshot t ~recv_ts (Symbol.of_string "ETHUSDT") (depth ~last_update_id:1)
     with
     | `Ignored -> "ignored"
     | `Gap | `Synced _ -> "unexpected");
  [%expect {| ignored |}]
;;

let%expect_test "reset drops the books" =
  let t = N.create ~l2_symbols:[ btc ] in
  show t (depth_update ~first:5 ~final:9 ~prev:4 ~bids:"[]" ~asks:"[]");
  (match N.snapshot t ~recv_ts btc (depth ~last_update_id:7) with
   | `Synced _ -> print_endline "synced"
   | `Gap | `Ignored -> print_endline "unexpected");
  N.reset t;
  print_s [%sexp (N.is_synced t btc : bool)];
  [%expect {|
    ("need snapshot" (symbol BTCUSDT))
    synced
    false
    |}]
;;
