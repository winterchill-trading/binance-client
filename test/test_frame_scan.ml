(* Frame_scan: words per call, the fields it reads, the frames it leaves to Yojson, and
   its decimal parser against Fixed_decimal.of_string. *)

open! Core
open Binance_client

let words_per_call f =
  let n = 1_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    f ()
  done;
  (Gc.minor_words () - before) / n
;;

let first_frame stream = List.nth_exn (Fixture.frames stream) 1

let%expect_test "no allocation per frame" =
  let t = Frame_scan.create () in
  List.iter [ Market_data.Stream.Trades; Bbo; L2 ] ~f:(fun stream ->
    let frame = first_frame stream in
    let words = words_per_call (fun () -> ignore (Frame_scan.scan t frame : bool)) in
    print_s [%message (stream : Market_data.Stream.t) (words : int)]);
  let frame = first_frame L2 in
  assert (Frame_scan.scan t frame);
  let walk () =
    let mutable pos = t.bids in
    while pos >= 0 do
      pos <- Frame_scan.next_level t frame pos
    done
  in
  print_s [%message "" ~levels:(words_per_call walk : int)];
  [%expect
    {|
    ((stream Trades) (words 0))
    ((stream Bbo) (words 0))
    ((stream L2) (words 0))
    (levels 0)
    |}]
;;

let%expect_test "fields" =
  let t = Frame_scan.create () in
  let show frame =
    let ok = Frame_scan.scan t frame in
    let symbol = String.sub frame ~pos:t.symbol_pos ~len:t.symbol_len in
    print_s [%message (ok : bool) (t.kind : Frame_scan.Kind.t) symbol]
  in
  show (first_frame Trades);
  print_s
    [%message
      (t.event_time : int)
        (t.transaction_time : int)
        (t.agg_trade_id : int)
        (t.price : int)
        (t.qty : int)
        (t.buyer_is_maker : bool)];
  show (first_frame Bbo);
  print_s
    [%message
      (t.update_id : int)
        (t.transaction_time : int)
        (t.bid_price : int)
        (t.bid_qty : int)
        (t.ask_price : int)
        (t.ask_qty : int)];
  let frame = first_frame L2 in
  show frame;
  print_s
    [%message
      (t.first_update_id : int) (t.final_update_id : int) (t.prev_final_update_id : int)];
  let first_levels pos =
    let next = Frame_scan.next_level t frame pos in
    let first = t.level_price, t.level_qty in
    let rec count pos n =
      if pos < 0 then n else count (Frame_scan.next_level t frame pos) (n + 1)
    in
    first, count next 1
  in
  print_s
    [%message
      ""
        ~bids:(first_levels t.bids : (int * int) * int)
        ~asks:(first_levels t.asks : (int * int) * int)];
  [%expect
    {|
    ((ok true) (t.kind Agg_trade) BTCUSDT)
    ((t.event_time 1791817544988) (t.transaction_time 1791817544843)
     (t.agg_trade_id 3482328964) (t.price 8261360000000) (t.qty 500000)
     (t.buyer_is_maker false))
    ((ok true) (t.kind Book_ticker) BTCUSDT)
    ((t.update_id 11797199215064) (t.transaction_time 1791817544896)
     (t.bid_price 8261350000000) (t.bid_qty 1323800000)
     (t.ask_price 8261360000000) (t.ask_qty 243200000))
    ((ok true) (t.kind Depth_update) BTCUSDT)
    ((t.first_update_id 11797199333709) (t.final_update_id 11797199345282)
     (t.prev_final_update_id 11797199333569))
    ((bids ((3211350000000 120000000) 37)) (asks ((8261360000000 666600000) 36)))
    |}]
;;

let%expect_test "frames left to Yojson" =
  let t = Frame_scan.create () in
  let trade =
    {|{"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}|}
  in
  List.iter
    [ trade
    ; {|{"stream":"btcusdt@aggTrade","data":|} ^ trade ^ "}"
    ; {|{"result":null,"id":1}|}
    ; {|{"code":2,"msg":"bad","id":1}|}
    ; {|{"lastUpdateId":1,"E":1,"T":1,"bids":[],"asks":[]}|}
    ; {|{"e":"markPriceUpdate","E":1,"s":"BTCUSDT","p":"1","r":"0","T":2}|}
    ; String.substr_replace_first trade ~pattern:{|"p":"1.5"|} ~with_:{|"p":1.5|}
    ; String.substr_replace_first trade ~pattern:{|"f":3,|} ~with_:""
    ; String.substr_replace_first trade ~pattern:{|"f":3,|} ~with_:{|"f":3,"f":3,|}
    ; String.substr_replace_first trade ~pattern:{|"m":true|} ~with_:{|"m":1|}
    ; String.substr_replace_first trade ~pattern:{|BTCUSDT|} ~with_:{|BTC\u0055SDT|}
    ; String.substr_replace_first trade ~pattern:{|,"T"|} ~with_:{|, "T"|}
    ; String.substr_replace_first trade ~pattern:{|"T":5|} ~with_:{|"T":05|}
    ; String.substr_replace_first trade ~pattern:{|"T":5|} ~with_:{|"T":5.0|}
    ; String.substr_replace_first trade ~pattern:{|"p":"1.5"|} ~with_:{|"p":"1e5"|}
    ; String.substr_replace_first
        trade
        ~pattern:{|"p":"1.5"|}
        ~with_:{|"p":"123456789012"|}
    ; String.substr_replace_first trade ~pattern:{|"q":"0.1"|} ~with_:{|"q":"0.1","x":{}|}
    ; String.substr_replace_first
        trade
        ~pattern:{|"q":"0.1"|}
        ~with_:{|"q":"0.1","st":tru|}
    ; String.substr_replace_first
        trade
        ~pattern:{|"q":"0.1"|}
        ~with_:{|"q":"0.1","nq":null|}
    ; String.substr_replace_first
        trade
        ~pattern:{|"q":"0.1"|}
        ~with_:{|"q":"0.1","nq":"x"|}
    ; String.substr_replace_first trade ~pattern:"aggTrade" ~with_:"aggTrades"
    ; trade ^ " "
    ; String.chop_suffix_exn trade ~suffix:"}"
    ; {|{"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[],"a":[["1","2"]]}|}
    ; {|{"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[["1"]],"a":[]}|}
    ; {|{"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[[1,2]],"a":[]}|}
    ; ""
    ]
    ~f:(fun frame -> printf "%b %s\n" (Frame_scan.scan t frame) frame);
  [%expect
    {|
    true {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}
    true {"stream":"btcusdt@aggTrade","data":{"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}}
    false {"result":null,"id":1}
    false {"code":2,"msg":"bad","id":1}
    false {"lastUpdateId":1,"E":1,"T":1,"bids":[],"asks":[]}
    false {"e":"markPriceUpdate","E":1,"s":"BTCUSDT","p":"1","r":"0","T":2}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":1.5,"q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":1}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTC\u0055SDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4, "T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":05,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5.0,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1e5","q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"123456789012","q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","x":{},"f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","st":tru,"f":3,"l":4,"T":5,"m":true}
    true {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","nq":null,"f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","nq":"x","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrades","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true}
    false {"e":"aggTrade","E":1,"a":2,"s":"BTCUSDT","p":"1.5","q":"0.1","f":3,"l":4,"T":5,"m":true
    true {"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[],"a":[["1","2"]]}
    false {"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[["1"]],"a":[]}
    false {"e":"depthUpdate","E":1,"T":2,"s":"X","U":1,"u":2,"pu":0,"b":[[1,2]],"a":[]}
    false
    |}]
;;

let%expect_test "decimal agrees with Fixed_decimal.of_string" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind len = Int.gen_incl 0 24 in
    String.gen_with_length len (Char.gen_uniform_inclusive '+' '9')
  in
  Quickcheck.test gen ~trials:100_000 ~f:(fun s ->
    let scanned = Frame_scan.decimal s ~pos:0 ~stop:(String.length s) in
    let expected =
      Option.value_map
        (Binance_protocol.Price.of_string_opt s)
        ~default:Int.min_value
        ~f:Binance_protocol.Price.to_units
    in
    if scanned <> expected
    then raise_s [%message "mismatch" s (scanned : int) (expected : int)]);
  List.iter
    [ "0.001"
    ; "-12.5"
    ; "+3"
    ; "5."
    ; ".5"
    ; "."
    ; "-"
    ; ""
    ; "1.2.3"
    ; "007.100000009"
    ; "46116860184.27387903"
    ; "46116860184.27387904"
    ; "99999999999"
    ; "100000000000"
    ]
    ~f:(fun s ->
      printf "%-24s %d\n" s (Frame_scan.decimal s ~pos:0 ~stop:(String.length s)));
  [%expect
    {|
    0.001                    100000
    -12.5                    -1250000000
    +3                       300000000
    5.                       500000000
    .5                       50000000
    .                        -4611686018427387904
    -                        -4611686018427387904
                             -4611686018427387904
    1.2.3                    -4611686018427387904
    007.100000009            710000000
    46116860184.27387903     4611686018427387903
    46116860184.27387904     -4611686018427387904
    99999999999              -4611686018427387904
    100000000000             -4611686018427387904
    |}]
;;
