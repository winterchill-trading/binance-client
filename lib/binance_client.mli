(** Async client for the Binance USD-M Futures API, built on [Venue_net] (transport) and
    [Binance_protocol] (wire types and signing). *)

(** Endpoints and credentials. *)
module Config = Config

(** REST endpoints: market data, orders, account, listen keys. *)
module Rest = Rest

(** A full book from a depth snapshot plus diff events, with Binance's sequencing rules. *)
module Local_book = Local_book

(** Combined market streams with live subscriptions and resubscription on reconnect. *)
module Market_ws = Market_ws

(** Async driver of a [Local_book]: snapshots, resyncs and book reports. *)
module Book_sync = Book_sync

(** Zero-allocation scanner for aggTrade, bookTicker and depthUpdate frames. *)
module Frame_scan = Frame_scan

(** Binance market frames to normalized [Market_data] records (pure). *)
module Feed_normalizer = Feed_normalizer

(** [Feed_normalizer] as a [Market_data.Normalizer.S], for rebuilding recorded raw files. *)
module Raw_normalizer = Raw_normalizer

(** [Market_data.Feed.t] from the public market streams. *)
module Live_feed = Live_feed

(** The user data stream with listen-key keepalive and renewal. *)
module User_ws = User_ws

(** [Mm_core.Venue.S] adapter: the market maker on Binance or the simulator. *)
module Venue = Venue
