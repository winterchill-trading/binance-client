# AGENTS.md (bench/)

- `main.exe` times decode plus normalize per recorded frame (trades, bbo, funding, l2) with
  `core_bench`.
- `feed/feed.exe` measures the feed's delivery cost against an in-process websocket server.

Numbers depend on the machine. The README and the spec quote them with the machine they were
taken on; quote the same way. Run from the repository root with `dune exec bench/main.exe`.
