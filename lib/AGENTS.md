# AGENTS.md (lib/)

Library `binance_client`.

- `Config`: `local`, `mainnet`, `testnet`, and `Config.Read_only` with no keys. Signed endpoints
  take `Config.t`, so a read-only config is a compile error (pinned in `test/must_fail`).
- `Rest` covers the endpoints of `binance-protocol`. `User_ws` manages the listen key and its
  30-minute keepalive. `Market_ws` is one combined-stream connection with acknowledged subscribe.
- `Local_book` and `Book_sync` implement Binance's diff-depth procedure (`U`/`u`/`pu`) with gap
  detection and resync.
- `Feed_normalizer` is pure: frame in, events out. `Live_feed` owns the sockets and reports
  stats; `feed_with_gaps` adds `Resync` gaps per reconnect and per broken L2 sequence.
- `Venue` is the `Mm_core.Venue.S` adapter. `Raw_normalizer` replays recorded frames for
  `rebuild-norm`.
- Build: `dune build --root .. @binance-client/all` from the repository root.
