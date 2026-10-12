(** Binance USD-M Futures REST endpoints over [Venue_net.Http].

    Parameters are form-encoded in insertion order ({!Binance_protocol.Query.encode}): in
    the query string for [GET] and [DELETE], in an [application/x-www-form-urlencoded]
    body for [POST] and [PUT]. Signed requests append [recvWindow] (when configured),
    [timestamp] and, when the config has a secret, [signature] computed over the exact
    string sent. Every request with a key carries [X-MBX-APIKEY].

    Every call returns [_ Or_error.t Deferred.t]: transport failures, non-2xx statuses,
    undecodable bodies and Binance [{"code", "msg"}] errors become [Error]. The latter can
    be recovered with {!api_error}. *)

open! Core
open Async
open Binance_protocol

module Security : sig
  (** Binance's endpoint security types: [NONE] (market data), [USER_STREAM] and
      [MARKET_DATA] (key only), [TRADE] and [USER_DATA] (signed). *)
  type t =
    | Public
    | Api_key
    | Signed
  [@@deriving sexp, compare, equal]
end

(** The {!Binance_protocol.Api_error.t} carried by an [Error] from this module, if any. *)
val api_error : Error.t -> Api_error.t option

(** {2 Generic requests} *)

(** [encode_params config security ~now params] is the string sent as query or body:
    [params], then for [Signed] [recvWindow], [timestamp] and [signature] (over everything
    before it). With [for_uri] (default [false]) the string is first normalised the way
    [Uri] re-encodes query strings, so what is signed is exactly what is sent. *)
val encode_params
  :  ?for_uri:bool
  -> Config.t
  -> Security.t
  -> now:Time_ns.t
  -> Query.t
  -> string

(** Headers sent for [security]: [X-MBX-APIKEY] unless [Public]. *)
val headers : Config.t -> Security.t -> (string * string) list

(** Decodes a response: [Api_error] when the body is [{"code" <> 200, "msg"}], an error
    for non-2xx statuses and invalid JSON, the JSON otherwise. *)
val decode : Venue_net.Http.response -> Yojson.Safe.t Or_error.t

(** Sends one request to [path] (e.g. ["/fapi/v1/depth"]) and decodes the JSON. *)
val call
  :  ?params:Query.t
  -> Config.t
  -> Security.t
  -> Cohttp.Code.meth
  -> string
  -> Yojson.Safe.t Or_error.t Deferred.t

(** {2 Market data} *)

val exchange_info : Config.t -> Exchange_info.t Or_error.t Deferred.t

(** [limit]: 5, 10, 20, 50, 100, 500 (Binance's default) or 1000. *)
val depth : ?limit:int -> Config.t -> Symbol.t -> Depth.t Or_error.t Deferred.t

val book_ticker : Config.t -> Symbol.t -> Book_ticker.t Or_error.t Deferred.t
val premium_index : Config.t -> Symbol.t -> Premium_index.t Or_error.t Deferred.t

(** {2 Orders} *)

val new_order : Config.t -> New_order.Request.t -> Order.t Or_error.t Deferred.t

(** One item per order: an order, or that order's own [Api_error]. *)
val batch_orders
  :  Config.t
  -> Batch_orders.Request.t
  -> Batch_orders.Response.t Or_error.t Deferred.t

val cancel_order : Config.t -> Cancel_order.Request.t -> Order.t Or_error.t Deferred.t

(** [DELETE /fapi/v1/allOpenOrders]. *)
val cancel_all : Config.t -> Symbol.t -> unit Or_error.t Deferred.t

(** Without [symbol], every symbol (weight 40 on Binance). *)
val open_orders : ?symbol:Symbol.t -> Config.t -> Open_order.t list Or_error.t Deferred.t

val user_trades
  :  ?order_id:Order_id.t
  -> ?from_id:Trade_id.t
  -> ?limit:int
  -> Config.t
  -> Symbol.t
  -> User_trade.t list Or_error.t Deferred.t

(** {2 Account} *)

(** [GET /fapi/v2/account]. *)
val account : Config.t -> Account.t Or_error.t Deferred.t

(** [GET /fapi/v2/positionRisk]. *)
val position_risk
  :  ?symbol:Symbol.t
  -> Config.t
  -> Position_risk.t list Or_error.t Deferred.t

(** {2 User data stream keys} *)

(** [POST /fapi/v1/listenKey]: a new key, or the account's active one, extended. *)
val create_listen_key : Config.t -> string Or_error.t Deferred.t

(** [PUT /fapi/v1/listenKey]: extends the key by 60 minutes. [-1125] means it is gone. *)
val keepalive_listen_key : Config.t -> unit Or_error.t Deferred.t

(** [DELETE /fapi/v1/listenKey]. *)
val close_listen_key : Config.t -> unit Or_error.t Deferred.t
