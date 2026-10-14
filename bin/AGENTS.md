# AGENTS.md (bin/)

`binance-cli`: `depth`, `book-ticker`, `watch`, `feed` and `rebuild-norm` (rebuilds event logs
from recorded raw minute files).

- `-network` selects `local` (default), `mainnet` or `testnet`; keys come from `BINANCE_API_KEY`
  and `BINANCE_API_SECRET`. Do not
  place orders on mainnet from experiments.
- Build: `dune build --root .. @binance-client/all` from the repository root.
