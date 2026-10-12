# binance-client

Async client for the Binance USD-M Futures API

Built with [OxCaml](https://oxcaml.org) and the Jane Street library stack (Core, Async),
on top of the wire types and signing in
[binance-protocol](https://github.com/winterchill-trading/binance-protocol) and the
venue-independent transport in [venue-net](https://github.com/winterchill-trading/venue-net).
The Binance counterpart of [hl-client](https://github.com/winterchill-trading/hl-client),
plus an [mm-core](https://github.com/winterchill-trading/mm-core) venue adapter so the
market maker runs on Binance. It targets the local simulator
([binance-sim](https://github.com/winterchill-trading/binance-sim)) and the real API.

## Build

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
for repo in fixed-decimal exchange-core parquet market-data order-book; do
  opam pin add -n -y $repo https://github.com/winterchill-trading/$repo.git
done
opam pin add -n -y exchange-core-engine https://github.com/winterchill-trading/exchange-core.git
for repo in venue-net binance-protocol mm-core binance-sim; do
  opam pin add -n -y $repo https://github.com/winterchill-trading/$repo.git
done
opam install . --deps-only --with-test
dune build && dune test
```

`binance-sim` and `exchange-core-engine` are test dependencies only. Inside a dune
workspace that also contains the sibling repositories, dune uses their sources and the
pins are not needed. `https://` and `wss://` need `async_ssl` built against OpenSSL 3; on
macOS with Homebrew's `openssl@4`, see
[hl-client's notes](https://github.com/winterchill-trading/hl-client#macos-and-openssl).

CI installs the pinned siblings and the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.

## Quick start

```ocaml
open! Core
open Async
open Binance_protocol
open Binance_client

let main () =
  let config = Config.local ~api_key:"alice" () in          (* http://localhost:3300 *)
  let btc = Symbol.of_string "BTCUSDT" in
  let%bind depth = Rest.depth ~limit:5 config btc >>| ok_exn in
  print_s [%sexp (depth : Depth.t)];
  let%bind order =
    Rest.new_order config
      (New_order.Request.limit btc Buy ~time_in_force:Gtx
         ~price:(Price.of_string "99000") ~quantity:(Size.of_string "0.01"))
  in
  print_s [%sexp (order : Order.t Or_error.t)];
  let%bind market = Market_ws.connect config >>| ok_exn in
  let%bind () = Market_ws.subscribe market [ Book_ticker btc ] >>| ok_exn in
  Pipe.iter_without_pushback (Market_ws.events market) ~f:(fun event ->
    print_s [%sexp (event : Market_event.t Combined.t)])
```

## Running against the simulator and Binance

```sh
# the simulator, in another terminal
dune exec binance-sim -- -port 3300

binance-cli depth BTCUSDT -limit 5
binance-cli book-ticker BTCUSDT
binance-cli watch -book BTCUSDT -agg-trade BTCUSDT -user      # local book, trades, user stream
binance-cli feed -symbol BTCUSDT -streams trades,bbo           # normalized market-data records
```

The simulator takes the `X-MBX-APIKEY` header as the account (any value; new keys are
credited 1,000,000 USDT) and does not check signatures, so `Config.local ~api_key ()`
needs no secret. `binance-cli` uses `BINANCE_API_KEY` (default `binance-cli`) and, when set,
`BINANCE_API_SECRET`.

Against Binance, create API keys for the futures testnet (or mainnet) and export them:

```sh
export BINANCE_API_KEY=... BINANCE_API_SECRET=...
binance-cli book-ticker BTCUSDT -network testnet
binance-cli watch -network testnet -book BTCUSDT -user
binance-cli watch -network mainnet -route market -agg-trade BTCUSDT -mark-price BTCUSDT
binance-cli feed -network mainnet -symbol BTCUSDT -streams trades,bbo,l2,funding
```

In code: `Config.testnet ~api_key ~api_secret ()` or `Config.mainnet ...`, reading the
keys from the environment yourself. Market data needs no key: use
`Config.Read_only.mainnet ()`.

## API

All modules live under `Binance_client`. Every network call returns `_ Or_error.t
Deferred.t`: transport failures, non-2xx statuses, undecodable bodies and Binance
`{"code", "msg"}` errors are `Error`.

### `Config`

```ocaml
module Keys : sig
  type t = { api_key : string; api_secret : string option; recv_window : int option }
end

type 'keys gen = { base_url : Uri.t; ws_url : Uri.t; routed_ws : bool; keys : 'keys }
type t = Keys.t gen                    (* with credentials: every endpoint *)

val create  : ?ws_url:Uri.t -> ?routed_ws:bool -> ?api_secret:string -> ?recv_window:int
  -> base_url:Uri.t -> api_key:string -> unit -> t
val local   : ?port:int -> ?api_secret:string -> api_key:string -> unit -> t   (* :3300 *)
val mainnet : ?recv_window:int -> api_key:string -> api_secret:string -> unit -> t
val testnet : ?recv_window:int -> api_key:string -> api_secret:string -> unit -> t

module Read_only : sig
  type t = unit gen                    (* no keys: public endpoints only *)
  val of_config : _ gen -> t
  val create : ?ws_url:Uri.t -> ?routed_ws:bool -> base_url:Uri.t -> unit -> t
  val local : ?port:int -> unit -> t
  val mainnet : unit -> t
  val testnet : unit -> t
end

val rest_uri : _ gen -> string -> Uri.t
val combined_stream_uri : _ gen -> route:Route.t -> Uri.t  (* <ws>[/public|/market]/stream *)
val user_stream_uri : _ gen -> listen_key:string -> Uri.t  (* <ws>[/private]/ws/<key> *)
```

Access is in the type. Public endpoints (`Rest.exchange_info`, `depth`, `book_ticker`,
`premium_index`, `call_public`, `Market_ws`, `Book_sync`, `Live_feed`) take any
`_ Config.gen`; everything that sends a key (orders, account, listen keys, `User_ws`,
`Venue`) takes `Config.t`. Passing a `Config.Read_only.t` to those is a compile error
(`test/must_fail` pins the message), so a market-data process can run without
credentials and cannot trade by accident.

| Constructor | REST | Websocket | Routed |
| --- | --- | --- | --- |
| `local` | `http://localhost:3300` | `ws://localhost:3300` | no |
| `mainnet` | `https://fapi.binance.com` | `wss://fstream.binance.com` | yes |
| `testnet` | `https://demo-fapi.binance.com` | `wss://demo-fstream.binance.com` | no |

Live USD-M routes websocket traffic by path: diff depth and `@bookTicker` on `/public`,
other market data on `/market`, user data on `/private`; an unrouted `/ws` or `/stream` connection only
receives public data, and silently gets nothing for `@aggTrade` or `@markPrice`.
`routed_ws` adds the prefixes.

### `Rest`

```ocaml
val call : ?params:Query.t -> Config.t -> Security.t -> Cohttp.Code.meth -> string
  -> Yojson.Safe.t Or_error.t Deferred.t
val call_public : ?params:Query.t -> _ Config.gen -> Cohttp.Code.meth -> string
  -> Yojson.Safe.t Or_error.t Deferred.t
val api_error : Error.t -> Api_error.t option

val exchange_info : _ Config.gen -> Exchange_info.t Or_error.t Deferred.t
val depth         : ?limit:int -> _ Config.gen -> Symbol.t -> Depth.t Or_error.t Deferred.t
val book_ticker   : _ Config.gen -> Symbol.t -> Book_ticker.t Or_error.t Deferred.t
val premium_index : _ Config.gen -> Symbol.t -> Premium_index.t Or_error.t Deferred.t
val new_order     : Config.t -> New_order.Request.t -> Order.t Or_error.t Deferred.t
val batch_orders  : Config.t -> Batch_orders.Request.t -> Batch_orders.Response.t Or_error.t Deferred.t
val cancel_order  : Config.t -> Cancel_order.Request.t -> Order.t Or_error.t Deferred.t
val cancel_all    : Config.t -> Symbol.t -> unit Or_error.t Deferred.t
val open_orders   : ?symbol:Symbol.t -> Config.t -> Open_order.t list Or_error.t Deferred.t
val user_trades   : ?order_id:Order_id.t -> ?from_id:Trade_id.t -> ?limit:int -> Config.t
  -> Symbol.t -> User_trade.t list Or_error.t Deferred.t
val account       : Config.t -> Account.t Or_error.t Deferred.t            (* /fapi/v2 *)
val position_risk : ?symbol:Symbol.t -> Config.t -> Position_risk.t list Or_error.t Deferred.t
val create_listen_key / keepalive_listen_key / close_listen_key
```

- Parameters are encoded in insertion order: in the query string for `GET` and `DELETE`,
  as an `application/x-www-form-urlencoded` body for `POST` and `PUT`.
- `Signed` requests (`TRADE`, `USER_DATA`) append `recvWindow` (when configured),
  `timestamp` (the local clock) and, when the config has a secret, the HMAC-SHA256
  `signature` of everything before it. `Api_key` and `Signed` requests carry
  `X-MBX-APIKEY`.
- `Uri` re-encodes query strings (`%3A` becomes `:`), so `GET`/`DELETE` queries are
  signed in the form `Uri` actually sends (`encode_params ~for_uri:true`).
- A body `{"code": c, "msg": m}` with `c <> 200` is an `Error` from which `api_error`
  recovers the `Api_error.t`, even under added tags. `DELETE /allOpenOrders` answers
  success as `{"code": 200, ...}`.

### `Market_ws`

```ocaml
val connect : ?route:Config.Route.t -> ?ping:Venue_net.Ws_client.Ping.t
  -> ?max_silent_intervals:int -> ?initial_backoff:Time_ns.Span.t
  -> ?max_backoff:Time_ns.Span.t -> ?on_reconnect:(unit -> unit) -> _ Config.gen
  -> t Or_error.t Deferred.t
val events : t -> Market_event.t Combined.t Pipe.Reader.t
val subscribe   : ?timeout:Time_ns.Span.t -> t -> Stream_name.t list -> unit Or_error.t Deferred.t
val unsubscribe : ?timeout:Time_ns.Span.t -> t -> Stream_name.t list -> unit Or_error.t Deferred.t
val subscriptions : t -> Stream_name.t list
val close : t -> unit Deferred.t
```

- One combined-stream connection; streams are added with live `SUBSCRIBE` requests and
  `subscribe` waits for the `{"result": null, "id": n}` acknowledgement (while
  disconnected it returns at once). Active streams are resubscribed after every
  reconnect, then `on_reconnect` runs.
- Keepalive: the server pings every 3 minutes and venue-net answers. By default the
  client also sends a ping frame every 30s, so a dead socket is replaced after two
  silent intervals; pass `` `None `` to rely on the server alone.
- `events` is buffered without pushback; keep reading.

### `Local_book` and `Book_sync`

`Local_book` is a pure full book per symbol fed by a `GET /depth` snapshot and diff
events, with Binance's rules: buffer until a snapshot, drop `u < lastUpdateId`, start at
the event with `U <= lastUpdateId <= u` (or `pu = lastUpdateId`, which the simulator sends
after an idle snapshot), then require `pu` = previous `u`. A break drops the book and
buffers again (`Gap`); `l2 ?depth` gives an `Exchange_core.L2.t` (order counts `n = 0`,
Binance has none).

`Book_sync` drives it: it fetches snapshots (one at a time, retrying when a snapshot is
older than the buffered events), applies `<symbol>@depth@100ms` events, calls `on_book`
after every change, and resyncs after `reset` (call it on reconnect).

### `User_ws`

```ocaml
val connect : ?keepalive_interval:Time_ns.Span.t -> ?ping:Venue_net.Ws_client.Ping.t
  -> ?max_silent_intervals:int -> ?initial_backoff:Time_ns.Span.t
  -> ?max_backoff:Time_ns.Span.t -> ?on_reconnect:(unit -> unit) -> Config.t
  -> t Or_error.t Deferred.t
val events : t -> User_event.t Pipe.Reader.t
val renew : t -> unit Or_error.t Deferred.t
val close : ?close_key:bool -> t -> unit Deferred.t
```

Creates a listen key, connects to `<ws>[/private]/ws/<listenKey>`, keeps the key alive
every 30 minutes, and on `-1125` from the keepalive or a `listenKeyExpired` event creates a
new key and replaces the socket (then calls `on_reconnect`, since events may have been
lost).

### `Live_feed`: normalized market data

```ocaml
val create
  :  ?on_raw:(stream:Market_data.Stream.t -> recv_ts:Time_ns.t -> string -> unit)
  -> ?time_source:Time_source.t
  -> _ Config.gen -> symbols:string list -> streams:Market_data.Stream.t list
  -> Market_data.Feed.t Deferred.t

type t
val start : (* same arguments as [create] *) -> t Deferred.t
val feed  : t -> Market_data.Feed.t
val stats : t -> Stats.t   (* { frames; parse_errors; reconnects } *)
```

A [market-data](https://github.com/winterchill-trading/market-data) `Feed.t` from the
public streams (no API key), one combined-stream connection per requested stream:

| Stream | Binance | Route | `exch_ts` |
| --- | --- | --- | --- |
| `Trades` | `<symbol>@aggTrade` (`trade_id` = aggregate id, side = taker) | `/market` | `T` |
| `Bbo` | `<symbol>@bookTicker` | `/public` | `T` |
| `L2` | `<symbol>@depth@100ms` + `GET /depth?limit=1000` | `/public` | `T` |
| `Funding` | `<symbol>@markPrice@1s` (index falls back to mark) | `/market` | `E` |

- `recv_ts` is taken when the frame comes off the socket, before parsing; `on_raw` sees
  the frame text at that point (subscription acknowledgements included), which is what a
  `Market_data.Frame_log` recorder stores. On `L2` it also sees each depth snapshot body
  (re-serialized JSON with `lastUpdateId` and no `e`), so raw logs can be normalized again.
- `L2` keeps a `Local_book` per symbol: one `Snapshot` (the full book) when it syncs, then
  every diff event as a `Delta`. A sequence gap or a reconnect drops the book and emits a
  fresh `Snapshot` after the next sync.
- Reconnects (backoff, resubscription) come from `Venue_net.Ws_client`; undecodable frames
  are logged at debug level and skipped. `close` closes the sockets and all four pipes.
- `stats` counts text frames received, frames dropped because they did not decode (or
  were error responses) and successful reconnections over all connections.
- `time_source` (default the wall clock) stamps `recv_ts` and drives reconnection backoff,
  keepalives and snapshot retries, so tests can run them on a synthetic clock.
- The conversions are pure and tested on their own in `Feed_normalizer`.

### `Venue`: the market maker on Binance

`Venue` implements `Mm_core.Venue.S` with `config = Config.t`, so
`Mm_core.Live.Make (Binance_client.Venue)` runs the strategy on Binance or the simulator.

| Venue call | Binance |
| --- | --- |
| `connect` | `exchangeInfo`, user data stream, one public combined-stream connection |
| `instrument` | `exchangeInfo` filters (`PRICE_FILTER` tick, `LOT_SIZE` step); `max_leverage` = the account's current leverage from `positionRisk` (default 20); fees = base tier, taker 5 bps, maker 2 bps |
| `books` | a local book per symbol (`GET /depth` + `@depth@100ms`), top 50 levels, after every applied diff; resynced after reconnects and gaps |
| `fills` | `ORDER_TRADE_UPDATE` with `x = TRADE` (or `CALCULATED`), once per trade id; fee = `n` taken as USD, `closed_pnl = rp` |
| `order_updates` | `ORDER_TRADE_UPDATE` status: `NEW`, `PARTIALLY_FILLED`, `FILLED`; `CANCELED`/`EXPIRED`/`EXPIRED_IN_MATCH` are `Cancelled` |
| `open_orders` | `GET /fapi/v1/openOrders?symbol=` |
| `account` | `GET /fapi/v2/account` + `GET /fapi/v2/positionRisk`; account id = API key |
| `place` | `POST /batchOrders` per 5 orders, `POST /order` for a single one, `newOrderRespType=RESULT` |
| `cancel` | `DELETE /order` per id in parallel; `-2011`/`-2013` (already gone) are not errors |
| `cancel_all` | `DELETE /allOpenOrders` |

Design choices:

- Books come from a full local book rather than `@depth20` partial snapshots: the partial
  depth payload shape is unverified for USD-M (see below) and partial snapshots cannot
  reveal lost updates, while the diff stream's `pu` chain does.
- Placement statuses: `NEW` (and a resting `PARTIALLY_FILLED`) is `Resting`; `FILLED`, or
  an IOC/expired order with a fill, is `Filled`; an order that expired unfilled is
  `Rejected` (`GTX` as `Post_only_would_cross`, IOC as `No_liquidity`, `EXPIRED_IN_MATCH`
  as `Self_trade`). Per-order `{code, msg}` errors map through
  `Api_error.to_reject_reason`, plus `-4116` (duplicate client id) and `-2010` by its
  wording; anything else is `Unsupported "<code> <msg>"`. A whole-request error (e.g.
  `-1102`) is an `Error`.
- Client order ids Binance would reject (more than 36 characters, or outside
  `[A-Za-z0-9.:/_-]`) are rejected locally as `Unsupported`.
- Binance's user stream does not replay history, so de-duplication by trade id
  (`Venue.Seen_trades`) only guards against repeated events; no time filter is applied,
  which avoids dropping fills on clock skew.
- `on_reconnect` hooks run after a reconnect of either stream and after a listen-key
  renewal.

## CLI

`binance-cli` (`bin/binance_cli.ml`) is a smoke-testing tool: `depth`, `book-ticker` and
`watch` (`-depth`, `-book` (local book, top 5), `-book-ticker`, `-agg-trade`,
`-mark-price`, `-user`), with `-network local|mainnet|testnet`, `-base-url`, `-ws-url`,
`-routed`, `-route public|market` and `-debug`.

`feed` prints `Market_data.Event.t` sexps, one per line, from `Live_feed`:
`-symbol` (repeatable) and `-streams` (comma-separated `trades`, `bbo`, `l2`, `funding`;
default all). Checked against the simulator with

```sh
# binance-sim directory
dune exec binance-sim -- -port 3300
# binance-client directory
dune exec binance-cli -- feed -symbol BTCUSDT -streams trades,bbo
```

```
(Bbo((instrument((venue Binance)(symbol BTCUSDT)))(exch_ts(2026-10-12 12:21:24.653000000+01:00))(recv_ts(2026-10-12 12:21:24.655117000+01:00))(bid_price 100214.7)(bid_size 0.035)(ask_price 100237.3)(ask_size 0.068)))
(Trade((instrument((venue Binance)(symbol BTCUSDT)))(exch_ts(2026-10-12 12:21:24.759000000+01:00))(recv_ts(2026-10-12 12:21:24.760146000+01:00))(price 100237.3)(size 0.068)(side Buy)(trade_id 290)))
```

and against mainnet with `-network mainnet` (all four streams).

## Unverified

Inherited from binance-protocol (see its
[spec](https://github.com/winterchill-trading/binance-protocol/blob/main/docs/binance-usdm-spec.md)),
and only checked against the simulator, which implements the same assumptions:

- The partial-depth payload shape. (`depthUpdate`, `aggTrade`, `bookTicker` and
  `markPriceUpdate` decode on mainnet, checked with `binance-cli feed`.)
- `ACCOUNT_UPDATE` position fields; `ORDER_TRADE_UPDATE` `er`.
- `exchangeInfo` filters other than `PRICE_FILTER` (`LOT_SIZE` is what the venue uses for
  the size step).
- `batchOrders` sent as a JSON array string with string values.
- Routing: whether the testnet serves routed paths, and the legacy `/ws/<listenKey>` user stream path used when unrouted.
- The testnet hosts (`demo-fapi`, `demo-fstream`).

Not implemented: RSA/Ed25519 keys, server time synchronisation (`timestamp` is the local
clock; set `recv_window` if it drifts), rate-limit tracking, the leverage brackets and
commission rate endpoints.

## Tests

`dune test` runs without network access:

- request encoding and signing (the documented HMAC example, `Uri` normalisation),
  headers, error decoding, endpoint URLs;
- `Local_book` sequencing with crafted events: buffering, the start rule, stale events,
  a gap and its resync, a too-old snapshot, the bounded buffer;
- venue conversions (fills, order updates, placement statuses, reject reasons, order
  requests), trade-id de-duplication, market frame decoding;
- `Feed_normalizer` on sample frames: trades, book tickers, mark prices (with and without
  index), acknowledgements and error responses, diff depth buffered until a snapshot,
  the snapshot with replayed events, deltas, a gap, other symbols, `reset`;
- `Feed_normalizer` on recorded mainnet frames (`test/fixtures`, see below): every frame
  decodes, with per-stream counts, and the minor words allocated per message;
- `Live_feed` against a scripted local websocket server: a malformed frame is counted in
  `stats` and skipped; a dropped connection is replaced on a synthetic clock (nothing
  before 0.4s, reconnected and resubscribed by 0.6s, `recv_ts` taken from that clock);
- against `Binance_sim.Server` on a free port: every REST call, market and user streams
  with acknowledged subscriptions, and the `Venue` (post-only order rests, shows in
  `open_orders` and is cancelled, a crossing post-only order is rejected, an IOC fills and
  the fill arrives once, `Account_view`, batch placement and `cancel_all`), and
  `Live_feed` delivering all four streams, raw frames and closing its pipes;
- a `Config.Read_only.t` on every public endpoint of the simulator; `test/must_fail`
  compiles a call of `Rest.account` with one and pins the type error;
- the mm-core live loop quoting for a few seconds on the simulator with noise traders.

Pipes in tests are read with `Pipe_read` (wait on `values_available`, then `read_now`):
`Clock_ns.with_timeout` around `Pipe.read` leaves the read pending after a timeout and it
swallows the next value, which `test_pipe_read.ml` demonstrates.

### Fixtures

`test/fixtures/*.jsonl` hold BTCUSDT frames recorded from the public mainnet streams (no
key) on 2026-10-12, one websocket text frame per line, as `on_raw` sees them, each file
starting with the subscription acknowledgement:

| File | Stream | Frames |
| --- | --- | --- |
| `trades.jsonl` | `btcusdt@aggTrade` (`/market`) | first 150 of the minute |
| `bbo.jsonl` | `btcusdt@bookTicker` (`/public`) | first 200 |
| `l2.jsonl` | `btcusdt@depth@100ms` (`/public`) | 5 diffs, the `GET /depth?limit=1000` snapshot (cut to 100 levels a side), 40 diffs |
| `funding.jsonl` | `btcusdt@markPrice@1s` (`/market`) | the whole minute (59) |

Normalized: 150 trades, 200 BBOs, 59 funding records, and for L2 one snapshot and 40
deltas (the 5 earlier diffs ask for a snapshot, none is a gap).

## Performance

`bench/main.exe` (core_bench) runs `Feed_normalizer.frame` over the recorded frames:

```sh
dune exec ./bench/main.exe -- -quota 2 -ascii
```

On an Apple M-series laptop:

| Benchmark | Time | Minor words |
| --- | --- | --- |
| `trades` (per frame, ~200 bytes) | 2.5 us | 482 |
| `bbo` (per frame) | 2.6 us | 475 |
| `funding` (per frame) | 2.6 us | 478 |
| `l2 replay` (46 frames, ~4.6 KB each, with the snapshot) | 4.7 ms | 1.28 M |

About 2.4 words per input byte for the small frames and 6 per byte for depth diffs,
almost all of it `Yojson.Safe` trees and decimal parsing. The expect test in
`test_allocation.ml` pins the same counts per Live_feed message: 478 words (trades), 472
(BBO), 474 (funding) and 27,087 on average for depth diffs, up to 150,200 for the
largest.
