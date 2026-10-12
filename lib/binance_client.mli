(** Async client for the Binance USD-M Futures API, built on [Venue_net] (transport) and
    [Binance_protocol] (wire types and signing). *)

(** Endpoints and credentials. *)
module Config = Config

(** REST endpoints: market data, orders, account, listen keys. *)
module Rest = Rest
