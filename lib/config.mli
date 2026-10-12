(** Where to reach a Binance USD-M Futures compatible API, and the credentials to use. *)

open! Core

(** Which of Binance's routed websocket paths a connection uses. Live USD-M serves
    high-frequency public data (diff depth, [@bookTicker]) on [/public], other market data
    ([@aggTrade], [@markPrice]) on [/market] and user data on [/private]; an unrouted
    [/ws] or [/stream] connection only gets public data. *)
module Route : sig
  type t =
    | Public
    | Market
    | Private
  [@@deriving sexp, compare, equal, enumerate]

  val to_path : t -> string
end

type t =
  { base_url : Uri.t (** REST root, e.g. [https://fapi.binance.com] *)
  ; ws_url : Uri.t (** websocket root, e.g. [wss://fstream.binance.com] *)
  ; routed_ws : bool
  (** prefix websocket paths with [/public], [/market] or [/private] (see {!Route}) *)
  ; api_key : string (** sent as [X-MBX-APIKEY]; the simulator uses it as the account *)
  ; api_secret : string option
  (** HMAC-SHA256 key; without it signed requests carry no [signature] (enough for the
      simulator) *)
  ; recv_window : int option (** [recvWindow] in milliseconds on signed requests *)
  }
[@@deriving sexp_of]

(** [ws_url] defaults to [base_url] with scheme [ws]/[wss], [routed_ws] to [false]. *)
val create
  :  ?ws_url:Uri.t
  -> ?routed_ws:bool
  -> ?api_secret:string
  -> ?recv_window:int
  -> base_url:Uri.t
  -> api_key:string
  -> unit
  -> t

(** The local simulator: [http://localhost:3300] and [ws://localhost:3300], unrouted. *)
val local : ?port:int -> ?api_secret:string -> api_key:string -> unit -> t

(** [https://fapi.binance.com] and [wss://fstream.binance.com], routed. *)
val mainnet : ?recv_window:int -> api_key:string -> api_secret:string -> unit -> t

(** [https://demo-fapi.binance.com] and [wss://demo-fstream.binance.com], unrouted
    (UNVERIFIED: whether the testnet serves the routed paths). *)
val testnet : ?recv_window:int -> api_key:string -> api_secret:string -> unit -> t

(** [base_url] joined with [path], e.g. [rest_uri t "/fapi/v1/depth"]. *)
val rest_uri : t -> string -> Uri.t

(** The combined-stream endpoint [<ws_url>[/<route>]/stream]. *)
val combined_stream_uri : t -> route:Route.t -> Uri.t

(** The user data stream [<ws_url>[/private]/ws/<listenKey>]. *)
val user_stream_uri : t -> listen_key:string -> Uri.t

val ws_url_of_base_url : Uri.t -> Uri.t
