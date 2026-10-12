open! Core
open Async
open Binance_protocol
open Binance_client

let secret = "2b5eb11e18796d12d88f13dc27dbbd02c2cc51ff7059765ed9821957d82bb4d9"
let api_key = "dbefbc809e3e83c283a984c3a1459732ea7db1360ca80c5c2c8867408d28cc83"

let config ?api_secret ?recv_window () =
  Config.create
    ?api_secret
    ?recv_window
    ~base_url:(Uri.of_string "https://fapi.binance.com")
    ~api_key
    ()
;;

let now = Time_ns.of_int_ns_since_epoch (1591702613943 * 1_000_000)

let order_params =
  [ "symbol", "BTCUSDT"
  ; "side", "BUY"
  ; "type", "LIMIT"
  ; "quantity", "1"
  ; "price", "9000"
  ; "timeInForce", "GTC"
  ]
;;

let%expect_test "signed query matches the documented example" =
  let config = config ~api_secret:secret ~recv_window:5000 () in
  print_endline (Rest.encode_params config Signed ~now order_params);
  [%expect
    {| symbol=BTCUSDT&side=BUY&type=LIMIT&quantity=1&price=9000&timeInForce=GTC&recvWindow=5000&timestamp=1591702613943&signature=3c661234138461fcc7a7d8746c6558c9842d4e10870d2ecbedf7777cad694af9 |}];
  return ()
;;

let%expect_test "without a secret, signed requests carry only the timestamp" =
  let config = config () in
  print_endline (Rest.encode_params config Signed ~now [ "symbol", "BTCUSDT" ]);
  print_endline (Rest.encode_params config Public ~now [ "symbol", "BTCUSDT" ]);
  print_s [%sexp (Rest.headers config Signed : (string * string) list)];
  print_s [%sexp (Rest.headers config Api_key : (string * string) list)];
  print_s [%sexp (Rest.headers config Public : (string * string) list)];
  [%expect
    {|
    symbol=BTCUSDT&timestamp=1591702613943
    symbol=BTCUSDT
    ((X-MBX-APIKEY
      dbefbc809e3e83c283a984c3a1459732ea7db1360ca80c5c2c8867408d28cc83))
    ((X-MBX-APIKEY
      dbefbc809e3e83c283a984c3a1459732ea7db1360ca80c5c2c8867408d28cc83))
    ()
    |}];
  return ()
;;

let%expect_test "batch orders are encoded as one percent-encoded JSON parameter" =
  let request =
    Batch_orders.Request.create
      [ New_order.Request.limit
          (Symbol.of_string "BTCUSDT")
          Sell
          ~time_in_force:Gtx
          ~price:(Price.of_string "101000")
          ~quantity:(Size.of_string "0.01")
      ]
    |> ok_exn
  in
  let params = Batch_orders.Request.to_params request in
  let config = config ~api_secret:secret () in
  print_endline (Rest.encode_params config Signed ~now params);
  (* [Uri] would re-encode [%3A] as [:]; the [for_uri] form is what [Uri] sends, and it is
     what gets signed. *)
  let encoded = Rest.encode_params ~for_uri:true config Signed ~now params in
  print_endline encoded;
  let uri = Uri.of_string ("http://localhost:3300/fapi/v1/batchOrders?" ^ encoded) in
  print_s [%sexp (String.is_suffix (Uri.to_string uri) ~suffix:encoded : bool)];
  [%expect
    {|
    batchOrders=%5B%7B%22symbol%22%3A%22BTCUSDT%22%2C%22side%22%3A%22SELL%22%2C%22type%22%3A%22LIMIT%22%2C%22timeInForce%22%3A%22GTX%22%2C%22quantity%22%3A%220.01%22%2C%22price%22%3A%22101000%22%7D%5D&timestamp=1591702613943&signature=932f7e0b4e200a7fadc66dfcbcbeda2e8e070b822570e8c8e08b3f348091bb53
    batchOrders=%5B%7B%22symbol%22:%22BTCUSDT%22%2C%22side%22:%22SELL%22%2C%22type%22:%22LIMIT%22%2C%22timeInForce%22:%22GTX%22%2C%22quantity%22:%220.01%22%2C%22price%22:%22101000%22%7D%5D&timestamp=1591702613943&signature=0410f99e04efb45fdde37ab9b626aa887cebd7f7789a0acd0a31af6f5d8142e3
    true
    |}];
  return ()
;;

let%expect_test "decode" =
  let show response =
    match Rest.decode response with
    | Ok json -> print_endline (Yojson.Safe.to_string json)
    | Error error ->
      print_s
        [%message
          "error"
            ~api_error:
              (Rest.api_error (Error.tag error ~tag:"outer") : Api_error.t option)]
  in
  show (200, {|{"listenKey":"abc"}|});
  show (200, {|{"code":200,"msg":"The operation of cancel all open order is done."}|});
  show (400, {|{"code":-5022,"msg":"Due to the order could not be executed as maker."}|});
  show (400, {|{"code":-2011,"msg":"Unknown order sent."}|});
  show (502, "Bad gateway");
  show (500, {|{"oops":1}|});
  show (200, "");
  [%expect
    {|
    {"listenKey":"abc"}
    {"code":200,"msg":"The operation of cancel all open order is done."}
    (error
     (api_error
      (((code Gtx_order_reject)
        (msg "Due to the order could not be executed as maker.")))))
    (error (api_error (((code Cancel_rejected) (msg "Unknown order sent.")))))
    (error (api_error ()))
    (error (api_error ()))
    null
    |}];
  return ()
;;

let%expect_test "endpoints" =
  let show (config : Config.t) =
    print_s
      [%message
        ""
          ~rest:(Uri.to_string (Config.rest_uri config "/fapi/v1/depth") : string)
          ~public:
            (Uri.to_string (Config.combined_stream_uri config ~route:Public) : string)
          ~market:
            (Uri.to_string (Config.combined_stream_uri config ~route:Market) : string)
          ~user:(Uri.to_string (Config.user_stream_uri config ~listen_key:"KEY") : string)]
  in
  show (Config.local ~api_key:"alice" ());
  show (Config.mainnet ~api_key:"k" ~api_secret:"s" ());
  show (Config.testnet ~api_key:"k" ~api_secret:"s" ());
  show
    (Config.create ~base_url:(Uri.of_string "https://example.com/proxy/") ~api_key:"k" ());
  [%expect
    {|
    ((rest http://localhost:3300/fapi/v1/depth)
     (public ws://localhost:3300/stream) (market ws://localhost:3300/stream)
     (user ws://localhost:3300/ws/KEY))
    ((rest https://fapi.binance.com/fapi/v1/depth)
     (public wss://fstream.binance.com/public/stream)
     (market wss://fstream.binance.com/market/stream)
     (user wss://fstream.binance.com/private/ws/KEY))
    ((rest https://demo-fapi.binance.com/fapi/v1/depth)
     (public wss://demo-fstream.binance.com/stream)
     (market wss://demo-fstream.binance.com/stream)
     (user wss://demo-fstream.binance.com/ws/KEY))
    ((rest https://example.com/proxy/fapi/v1/depth)
     (public wss://example.com/proxy/stream)
     (market wss://example.com/proxy/stream)
     (user wss://example.com/proxy/ws/KEY))
    |}];
  return ()
;;
