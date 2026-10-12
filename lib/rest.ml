open! Core
open Async
open Binance_protocol

module Security = struct
  type t =
    | Public
    | Api_key
    | Signed
  [@@deriving sexp, compare, equal]
end

(* API errors are an [Error.t] whose sexp contains [(binance_api_error <Api_error.t>)];
   found anywhere in the tree, so tags added by callers do not hide it. *)
let api_error_tag = "binance_api_error"

let of_api_error (e : Api_error.t) =
  Error.create_s
    (List
       [ Atom api_error_tag
       ; Api_error.sexp_of_t e
       ; Atom (Api_error.Code.to_int e.code |> Int.to_string)
       ])
;;

let api_error error =
  let rec find (sexp : Sexp.t) =
    match sexp with
    | List (Atom tag :: e :: _) when String.equal tag api_error_tag ->
      Option.try_with (fun () -> Api_error.t_of_sexp e)
    | List l -> List.find_map l ~f:find
    | Atom _ -> None
  in
  find (Error.sexp_of_t error)
;;

let encode_params (config : Config.t) (security : Security.t) ~now params =
  match security with
  | Public | Api_key -> Query.encode params
  | Signed ->
    let timestamp = Timestamp_ms.of_time_ns now in
    (match config.api_secret with
     | Some secret ->
       Signature.signed_query ?recv_window:config.recv_window ~secret ~timestamp params
     | None ->
       Query.encode
         (params
          @ List.filter_opt
              [ Option.map config.recv_window ~f:(fun w -> "recvWindow", Int.to_string w)
              ; Some ("timestamp", Int.to_string (Timestamp_ms.to_int timestamp))
              ]))
;;

let headers (config : Config.t) (security : Security.t) =
  match security with
  | Public -> []
  | Api_key | Signed -> [ "X-MBX-APIKEY", config.api_key ]
;;

let decode ((status, body) : Venue_net.Http.response) =
  let json =
    if String.is_empty (String.strip body)
    then Ok `Null
    else Or_error.try_with (fun () -> Yojson.Safe.from_string body)
  in
  match json with
  | Error _ ->
    Or_error.error_s [%message "invalid JSON response" (status : int) (body : string)]
  | Ok json ->
    (match Api_error.of_yojson_opt json with
     | Some error when Api_error.Code.to_int error.code <> 200 ->
       Error (of_api_error error)
     | _ ->
       if Venue_net.Http.is_success status
       then Ok json
       else Or_error.error_s [%message "HTTP error" (status : int) (body : string)])
;;

let call ?(params = []) config security (meth : Cohttp.Code.meth) path =
  let encoded = encode_params config security ~now:(Time_ns.now ()) params in
  let headers = headers config security in
  let uri = Config.rest_uri config path in
  let response =
    match meth with
    | `POST | `PUT ->
      Venue_net.Http.request
        ~headers:(("Content-Type", "application/x-www-form-urlencoded") :: headers)
        ~body:encoded
        meth
        uri
    | _ ->
      (* Built from the string so the query is sent exactly as encoded (and signed). *)
      let uri =
        if String.is_empty encoded
        then uri
        else Uri.of_string (Uri.to_string uri ^ "?" ^ encoded)
      in
      Venue_net.Http.request ~headers meth uri
  in
  let%map response in
  Or_error.tag_s
    (Or_error.bind response ~f:decode)
    ~tag:[%message (Cohttp.Code.string_of_method meth) path]
;;

let typed ?params config security meth path ~f =
  let%map json = call ?params config security meth path in
  Or_error.bind json ~f:(fun json ->
    Or_error.try_with (fun () -> f json)
    |> Or_error.tag_s ~tag:[%message "cannot decode response" path])
;;

let list_of f : Yojson.Safe.t -> _ = function
  | `List items -> List.map items ~f
  | json -> raise_s [%message "expected a JSON array" (Yojson.Safe.to_string json)]
;;

let symbol_param symbol = "symbol", Symbol.to_string symbol

let exchange_info config =
  typed config Public `GET "/fapi/v1/exchangeInfo" ~f:Exchange_info.t_of_yojson
;;

let depth ?limit config symbol =
  typed
    config
    Public
    `GET
    "/fapi/v1/depth"
    ~params:
      (symbol_param symbol
       :: List.filter_opt [ Option.map limit ~f:(fun l -> "limit", Int.to_string l) ])
    ~f:Depth.t_of_yojson
;;

let book_ticker config symbol =
  typed
    config
    Public
    `GET
    "/fapi/v1/ticker/bookTicker"
    ~params:[ symbol_param symbol ]
    ~f:Book_ticker.t_of_yojson
;;

let premium_index config symbol =
  typed
    config
    Public
    `GET
    "/fapi/v1/premiumIndex"
    ~params:[ symbol_param symbol ]
    ~f:Premium_index.t_of_yojson
;;

let new_order config request =
  typed
    config
    Signed
    `POST
    "/fapi/v1/order"
    ~params:(New_order.Request.to_params request)
    ~f:Order.t_of_yojson
;;

let batch_orders config request =
  typed
    config
    Signed
    `POST
    "/fapi/v1/batchOrders"
    ~params:(Batch_orders.Request.to_params request)
    ~f:Batch_orders.Response.t_of_yojson
;;

let cancel_order config request =
  typed
    config
    Signed
    `DELETE
    "/fapi/v1/order"
    ~params:(Cancel_order.Request.to_params request)
    ~f:Order.t_of_yojson
;;

let cancel_all config symbol =
  typed
    config
    Signed
    `DELETE
    "/fapi/v1/allOpenOrders"
    ~params:(Cancel_all.Request.to_params { symbol })
    ~f:(fun json -> ignore (Cancel_all.Response.t_of_yojson json : Cancel_all.Response.t))
;;

let open_orders ?symbol config =
  typed
    config
    Signed
    `GET
    "/fapi/v1/openOrders"
    ~params:(List.filter_opt [ Option.map symbol ~f:symbol_param ])
    ~f:(list_of Open_order.t_of_yojson)
;;

let user_trades ?order_id ?from_id ?limit config symbol =
  typed
    config
    Signed
    `GET
    "/fapi/v1/userTrades"
    ~params:
      (symbol_param symbol
       :: List.filter_opt
            [ Option.map order_id ~f:(fun id -> "orderId", Order_id.to_string id)
            ; Option.map from_id ~f:(fun id -> "fromId", Trade_id.to_string id)
            ; Option.map limit ~f:(fun l -> "limit", Int.to_string l)
            ])
    ~f:(list_of User_trade.t_of_yojson)
;;

let account config = typed config Signed `GET "/fapi/v2/account" ~f:Account.t_of_yojson

let position_risk ?symbol config =
  typed
    config
    Signed
    `GET
    "/fapi/v2/positionRisk"
    ~params:(List.filter_opt [ Option.map symbol ~f:symbol_param ])
    ~f:(list_of Position_risk.t_of_yojson)
;;

let listen_key meth config =
  typed config Api_key meth "/fapi/v1/listenKey" ~f:(fun json -> json)
;;

let create_listen_key config =
  let%map json = listen_key `POST config in
  Or_error.bind json ~f:(fun json ->
    Or_error.try_with (fun () -> (Listen_key.t_of_yojson json).listen_key))
;;

let keepalive_listen_key config = listen_key `PUT config |> Deferred.Or_error.ignore_m
let close_listen_key config = listen_key `DELETE config |> Deferred.Or_error.ignore_m
