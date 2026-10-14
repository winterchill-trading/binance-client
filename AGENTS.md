# AGENTS.md

## Purpose

Async client for the Binance USD-M Futures API: REST, market and user-data websockets, a local
order book, a normalized `Live_feed` for market data, and a `Mm_core.Venue.S` adapter so the
market maker runs on Binance or on `binance-sim`. A read-only configuration cannot send
credentials, and the compiler enforces that.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `binance_client`: `Config`, `Rest`, `Market_ws`, `User_ws`, `Local_book`, `Book_sync`, `Live_feed`, `Feed_normalizer`, `Raw_normalizer`, `Venue` (see `lib/AGENTS.md`) |
| `bin/` | `binance-cli`: `depth`, `book-ticker`, `watch`, `feed`, `rebuild-norm` (see `bin/AGENTS.md`) |
| `bench/` | Feed decode benchmark and the feed delivery benchmark (see `bench/AGENTS.md`) |
| `test/` | Expect tests, allocation pins, fixtures, a fake websocket server, `must_fail` compile-error pin (see `test/AGENTS.md`) |
| `.github/` | CI workflow (see `.github/AGENTS.md`) |
| `dune-project`, `binance-client.opam` | Package metadata; the `.opam` file is generated from `dune-project` |

## Build, test, docs

Run from this directory. The workspace root `..` holds the sibling checkouts and the shared
`dune-workspace`.

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @binance-client/all
opam exec --switch 5.2.0+ox -- dune build --root .. @binance-client/runtest
DUNE_BUILD_DIR=_build-binance-client opam exec --switch 5.2.0+ox -- dune build --root .. @binance-client/all
```

The last line is for parallel work: a private build directory avoids the lock shared with
other repositories in the workspace.

Run against the simulator: in `../binance-sim`, start `dune exec binance-sim -- -port 3300`
then, here, `dune exec binance-cli -- depth BTCUSDT -limit 5`.

Standalone (outside the workspace), the `Build` section of `README.md` lists the opam pins and
the install commands. `binance-sim` and `exchange-core-engine` are test-only dependencies.

## Conventions

- Formatting: `dune build --root .. @binance-client/fmt --auto-promote`, run after the tests
  pass. Never `dune fmt` at the workspace root; it reformats every sibling repository.
- Every module has an `.mli`. Use `open! Core`, labelled arguments and `ppx_jane`.
- Every endpoint that sends a key takes `Config.t`. Read-only paths take `Config.Read_only`.
  Do not widen a read-only path to a signed one.
- The live path is allocation-pinned (`test/test_allocation.ml`, `test/test_live_feed_words.ml`).
  A change that moves a pin needs a measured reason in the commit message.
- Market data reaches consumers through `Market_data.Feed.t`; `Live_feed` reports gaps as
  `Resync` events before the new snapshot.
- Add a short comment at any non-obvious choice, for example the Binance depth rules or a
  pinned word count.
- Document every public value in its `.mli` with odoc comments.

## Dependencies and dependents

- Depends on: `venue-net`, `binance-protocol`, `exchange-core`, `mm-core`, `market-data`,
  `fixed-decimal`.
- Test-only: `binance-sim`, `exchange-core-engine`.
- Used by: `market-maker`, `market-recorder`.
- CI pin order: `fixed-decimal`, `exchange-core`, `parquet`, `market-data`, `order-book`,
  `exchange-core-engine`, `venue-net`, `binance-protocol`, `mm-core`, `binance-sim`.

## CI

`.github/workflows/ci.yml` runs the `ox-setup` action with the pins above (its opam switch is
cached between runs), then `dune build` and `dune test`. The `obj-magic` job calls the shared
reusable workflow. To check a run: `gh run list --limit 1`, then at most one
`gh run watch <id> --exit-status`. Do not poll in a loop.

## Pitfalls

- `Uri` re-encodes query strings (`%3A` becomes `:`). Sign the query in the form `Uri` actually
  sends, or the real venue rejects the signature.
- `Clock_ns.with_timeout` around `Pipe.read` leaves the read pending, and it then swallows the
  next value. Wait on `Pipe.values_available` and then `read_now`. Tests use `Pipe_read`.
- Read websocket frames with your own `iter_without_pushback`. A `Pipe.filter_map` stage stalls
  acknowledgements when nothing reads downstream.
- Binance mainnet sends `@bookTicker` on the `/public` route only; `/market` stays silent for it.
- OxCaml rejects `exception E of t [@@deriving sexp_of]` when a field's sexp converter is not
  portable. Carry a tagged sexp or an int code instead.
- Exception sexps embed the source path. Keep them out of expect output.
- `dune promote` overwrites test edits made after the failing run. Promote only after reading
  the diff.
- `Time_ns` sexps print in the local zone; use the `Alternate_sexp` form in expect output.

## Where to read more

- `README.md` (quick start, simulator and mainnet use, build, benchmarks).
- `../specs/repos/binance-client.md` for status, known gaps and the CI pin order.
- `../specs/repos/binance-protocol.md` for the wire types.
- Skills in `../oxcaml-claude-skills/skills/`: `async-http-websocket`, `oxcaml-performance`,
  `janestreet-ocaml-style`.
- `../oxcaml-claude-skills/AGENTS.md`: the workspace-wide guide.
- `../oxcaml-claude-skills/GOTCHAS.md`: the dated log these pitfalls come from.

## Never do

- Run `opam install`, `opam upgrade` or `opam pin` on the shared `5.2.0+ox` switch without a
  dry run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`. Abort if the plan touches
  `ocaml-variants`, `core`, `base`, `async` or `ppx_jane`.
- Send an order or touch an account against mainnet from a test. Tests run against the
  simulator or recorded frames.
- Publish, tag or release a version from this repository without the maintainers asking.
- Commit credentials or print secrets. Keys come from the environment.
- Poll CI in a loop.
