open! Core

module Uri = struct
  include Uri

  let sexp_of_t t = Sexp.Atom (Uri.to_string t)
end

module Route = struct
  type t =
    | Public
    | Market
    | Private
  [@@deriving sexp, compare, equal, enumerate]

  let to_path = function
    | Public -> "/public"
    | Market -> "/market"
    | Private -> "/private"
  ;;
end

module Keys = struct
  type t =
    { api_key : string
    ; api_secret : (string option[@sexp.opaque])
    ; recv_window : int option
    }
  [@@deriving sexp_of]
end

type 'keys gen =
  { base_url : Uri.t
  ; ws_url : Uri.t
  ; routed_ws : bool
  ; keys : 'keys
  }
[@@deriving sexp_of]

type t = Keys.t gen [@@deriving sexp_of]

let ws_url_of_base_url base_url =
  let scheme =
    match Uri.scheme base_url with
    | Some "https" | Some "wss" -> "wss"
    | _ -> "ws"
  in
  Uri.with_scheme base_url (Some scheme)
;;

let create ?ws_url ?(routed_ws = false) ?api_secret ?recv_window ~base_url ~api_key () =
  let ws_url =
    Option.value_or_thunk ws_url ~default:(fun () -> ws_url_of_base_url base_url)
  in
  { base_url; ws_url; routed_ws; keys = { Keys.api_key; api_secret; recv_window } }
;;

let local ?(port = 3300) ?api_secret ~api_key () =
  create
    ?api_secret
    ~base_url:(Uri.of_string (sprintf "http://localhost:%d" port))
    ~api_key
    ()
;;

let mainnet ?recv_window ~api_key ~api_secret () =
  create
    ~ws_url:(Uri.of_string "wss://fstream.binance.com")
    ~routed_ws:true
    ~api_secret
    ?recv_window
    ~base_url:(Uri.of_string "https://fapi.binance.com")
    ~api_key
    ()
;;

let testnet ?recv_window ~api_key ~api_secret () =
  create
    ~ws_url:(Uri.of_string "wss://demo-fstream.binance.com")
    ~api_secret
    ?recv_window
    ~base_url:(Uri.of_string "https://demo-fapi.binance.com")
    ~api_key
    ()
;;

module Read_only = struct
  type t = unit gen [@@deriving sexp_of]

  let of_config (config : _ gen) = { config with keys = () }

  let create ?ws_url ?routed_ws ~base_url () =
    of_config (create ?ws_url ?routed_ws ~base_url ~api_key:"" ())
  ;;

  let local ?port () = of_config (local ?port ~api_key:"" ())
  let mainnet () = of_config (mainnet ~api_key:"" ~api_secret:"" ())
  let testnet () = of_config (testnet ~api_key:"" ~api_secret:"" ())
end

let join base path =
  let base_path = String.rstrip (Uri.path base) ~drop:(Char.equal '/') in
  Uri.with_query (Uri.with_path base (base_path ^ path)) []
;;

let rest_uri t path = join t.base_url path
let route_prefix t route = if t.routed_ws then Route.to_path route else ""
let combined_stream_uri t ~route = join t.ws_url (route_prefix t route ^ "/stream")

let user_stream_uri t ~listen_key =
  join t.ws_url (route_prefix t Private ^ "/ws/" ^ listen_key)
;;
