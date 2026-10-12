open! Core
open Async
open Binance_protocol
open Binance_client
module Neutral = Exchange_core

let order_trade_update ~x ~status ~trade_id ~last ~cum =
  sprintf
    {|{"e":"ORDER_TRADE_UPDATE","E":1700000000001,"T":1700000000000,"o":{"s":"BTCUSDT","c":"mm-b0-1","S":"BUY","o":"LIMIT","f":"GTX","q":"0.03","p":"100000","ap":"100000","sp":"0","x":"%s","X":"%s","i":42,"l":"%s","z":"%s","L":"100000","N":"USDT","n":"0.4","T":1700000000000,"t":%d,"b":"0","a":"0","m":true,"R":false,"wt":"CONTRACT_PRICE","ot":"LIMIT","ps":"BOTH","cp":false,"rp":"1.5","pP":false,"si":0,"ss":0,"V":"NONE","pm":"NONE","gtd":0}}|}
    x
    status
    last
    cum
    trade_id
  |> User_ws.For_testing.decode
  |> ok_exn
  |> function
  | User_event.Order_trade_update u -> u
  | event -> raise_s [%message "unexpected" (event : User_event.t)]
;;

let%expect_test "order trade updates to fills and order updates" =
  let show u =
    print_s
      [%message
        ""
          ~fill:(Venue.Convert.fill u : Mm_core.Fill.t option)
          ~update:(Venue.Convert.order_update u : Mm_core.Order_update.t option)]
  in
  show (order_trade_update ~x:"NEW" ~status:"NEW" ~trade_id:0 ~last:"0" ~cum:"0");
  show
    (order_trade_update
       ~x:"TRADE"
       ~status:"PARTIALLY_FILLED"
       ~trade_id:7
       ~last:"0.01"
       ~cum:"0.01");
  show
    (order_trade_update ~x:"TRADE" ~status:"FILLED" ~trade_id:8 ~last:"0.02" ~cum:"0.03");
  show (order_trade_update ~x:"EXPIRED" ~status:"EXPIRED" ~trade_id:0 ~last:"0" ~cum:"0");
  [%expect
    {|
    ((fill ())
     (update
      (((order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT) (side Buy)
        (px 100000) (status New) (filled 0) (remaining 0.03)
        (time 1700000000000000000)))))
    ((fill
      (((trade_id 7) (order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT)
        (side Buy) (px 100000) (size 0.01) (fee 0.4) (is_maker true)
        (time 1700000000000000000) (closed_pnl (1.5)))))
     (update
      (((order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT) (side Buy)
        (px 100000) (status Partially_filled) (filled 0.01) (remaining 0.02)
        (time 1700000000000000000)))))
    ((fill
      (((trade_id 8) (order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT)
        (side Buy) (px 100000) (size 0.02) (fee 0.4) (is_maker true)
        (time 1700000000000000000) (closed_pnl (1.5)))))
     (update
      (((order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT) (side Buy)
        (px 100000) (status Filled) (filled 0.03) (remaining 0)
        (time 1700000000000000000)))))
    ((fill ())
     (update
      (((order_id 42) (client_order_id (mm-b0-1)) (symbol BTCUSDT) (side Buy)
        (px 100000) (status Cancelled) (filled 0) (remaining 0)
        (time 1700000000000000000)))))
    |}];
  return ()
;;

let order ~status ~tif ~executed =
  { Order.order_id = Order_id.of_int 5
  ; symbol = Symbol.of_string "BTCUSDT"
  ; client_order_id = Client_order_id.of_string "c1"
  ; side = Buy
  ; position_side = None
  ; status
  ; order_type = Limit
  ; orig_type = None
  ; time_in_force = tif
  ; price = Price.of_string "100"
  ; orig_qty = Size.of_string "2"
  ; executed_qty = Size.of_string executed
  ; cum_qty = None
  ; cum_quote = None
  ; avg_price = Some (Price.of_string "99.5")
  ; stop_price = None
  ; activate_price = None
  ; price_rate = None
  ; reduce_only = false
  ; close_position = false
  ; price_protect = false
  ; working_type = None
  ; price_match = None
  ; self_trade_prevention_mode = None
  ; good_till_date = None
  ; time = None
  ; update_time = Timestamp_ms.of_int 0
  }
;;

let%expect_test "order responses to placement statuses" =
  List.iter
    [ Order_status.New, Time_in_force.Gtc, "0"
    ; Partially_filled, Gtc, "1"
    ; Partially_filled, Ioc, "1"
    ; Filled, Ioc, "2"
    ; Expired, Ioc, "1"
    ; Expired, Ioc, "0"
    ; Expired, Gtx, "0"
    ; Expired_in_match, Gtc, "0"
    ; Canceled, Gtc, "0"
    ]
    ~f:(fun (status, tif, executed) ->
      print_s
        [%sexp
          (Venue.Convert.order_status (order ~status ~tif ~executed)
           : Neutral.Order_status.t)]);
  [%expect
    {|
    (Resting (order_id 5) (client_order_id (c1)))
    (Resting (order_id 5) (client_order_id (c1)))
    (Filled (order_id 5) (total 1) (avg_px 99.5) (client_order_id (c1)))
    (Filled (order_id 5) (total 2) (avg_px 99.5) (client_order_id (c1)))
    (Filled (order_id 5) (total 1) (avg_px 99.5) (client_order_id (c1)))
    (Rejected No_liquidity)
    (Rejected (Post_only_would_cross (best_bid ()) (best_ask ())))
    (Rejected Self_trade)
    (Rejected (Unsupported "order CANCELED on placement"))
    |}];
  List.iter
    [ { Api_error.code = Gtx_order_reject; msg = "post only" }
    ; { code = Min_notional; msg = "Order's notional must be no smaller than 100" }
    ; { code = Other (-4116); msg = "ClientOrderId is duplicated." }
    ; { code = New_order_rejected
      ; msg = "Account has insufficient balance for requested action."
      }
    ; { code = New_order_rejected
      ; msg = "Order would immediately match against an order of the same account."
      }
    ; { code = Request_throttled; msg = "Too many requests." }
    ]
    ~f:(fun e ->
      print_s [%sexp (Venue.Convert.reject_reason e : Neutral.Reject_reason.t)]);
  [%expect
    {|
    (Post_only_would_cross (best_bid ()) (best_ask ()))
    (Below_min_value (min 100))
    Duplicate_client_order_id
    Insufficient_margin
    Self_trade
    (Unsupported "-1008 Too many requests.")
    |}];
  return ()
;;

let%expect_test "order requests" =
  let show r =
    print_s
      [%sexp
        (Venue.Convert.order_request r |> Result.map ~f:New_order.Request.to_params
         : ((string * string) list, Neutral.Reject_reason.t) Result.t)]
  in
  let btc = Symbol.of_string "BTCUSDT" in
  show
    (Neutral.Order_request.limit
       ~tif:Alo
       ~client_order_id:(Neutral.Client_order_id.of_string "mm-b0-1")
       btc
       Buy
       ~px:(Price.of_string "100000.1")
       ~size:(Size.of_string "0.01"));
  show
    (Neutral.Order_request.market ~reduce_only:true btc Sell ~size:(Size.of_string "1"));
  show
    (Neutral.Order_request.limit
       ~client_order_id:(Neutral.Client_order_id.of_string "has space")
       btc
       Buy
       ~px:(Price.of_string "1")
       ~size:(Size.of_string "1"));
  [%expect
    {|
    (Ok
     ((symbol BTCUSDT) (side BUY) (type LIMIT) (timeInForce GTX) (quantity 0.01)
      (price 100000.1) (newClientOrderId mm-b0-1) (newOrderRespType RESULT)))
    (Ok
     ((symbol BTCUSDT) (side SELL) (type MARKET) (quantity 1) (reduceOnly true)
      (newOrderRespType RESULT)))
    (Error
     (Unsupported
      "client order id must be 1-36 characters from [A-Za-z0-9.:/_-]"))
    |}];
  return ()
;;

let%expect_test "market frames" =
  List.iter
    [ {|{"stream":"btcusdt@depth@100ms","data":{"e":"depthUpdate","E":2,"T":1,"s":"BTCUSDT","U":5,"u":6,"pu":4,"b":[["99","1"]],"a":[]}}|}
    ; {|{"result":null,"id":3}|}
    ; {|{"code":2,"msg":"Invalid request: unknown variant","id":4}|}
    ; {|{"stream":"btcusdt@kline_1m","data":{"e":"kline","E":2}}|}
    ; {|not json|}
    ]
    ~f:(fun frame ->
      print_s
        [%sexp
          (Market_ws.For_testing.decode frame : Market_ws.For_testing.message Or_error.t)]);
  print_endline
    (Market_ws.For_testing.encode
       { method_ = Subscribe
       ; params =
           [ Diff_depth { symbol = Symbol.of_string "BTCUSDT"; speed = Some Ms_100 } ]
       ; id = 1
       });
  [%expect
    {|
    (Ok
     (Event
      ((stream (Diff_depth (symbol BTCUSDT) (speed (Ms_100))))
       (data
        (Depth_update
         ((event_time 2) (transaction_time 1) (symbol BTCUSDT)
          (first_update_id 5) (final_update_id 6) (prev_final_update_id 4)
          (bids (((price 99) (qty 1)))) (asks ())))))))
    (Ok (Response (Result (result null) (id 3))))
    (Ok
     (Response
      (Error (code 2) (msg "Invalid request: unknown variant") (id (4)))))
    (Ok
     (Event
      ((stream (Other btcusdt@kline_1m))
       (data (Unknown (event_type (kline)) (json "{\"e\":\"kline\",\"E\":2}"))))))
    (Error
     ("Yojson__Common.Json_error(\"Line 1, bytes 0-8:\\nInvalid token 'not json'\")"))
    {"method":"SUBSCRIBE","params":["btcusdt@depth@100ms"],"id":1}
    |}];
  return ()
;;
