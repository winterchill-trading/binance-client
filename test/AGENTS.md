# AGENTS.md (test/)

Tests for `binance_client`. `dune test` runs without network access.

- `fixtures/*.jsonl` are BTCUSDT mainnet frames from 2026-10-12, one file per stream. They are
  data: do not edit them by hand.
- `fake_ws.ml` and `pipe_read.ml` are test helpers; read pipes with `Pipe_read`, not with
  `with_timeout` around `Pipe.read`.
- `must_fail/` pins the compile error for passing a read-only config to a signed call.
- `blast/` is a websocket server that writes pre-encoded frames; the words-per-frame test and the feed benchmark both use it.
- `test_allocation.ml` and `test_live_feed_words.ml` pin allocations. Explain any change in the
  commit message.
- Promote expect output with `dune promote` only after reading the diff.
- Run: `dune build --root .. @binance-client/runtest` from the repository root.
