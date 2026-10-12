(lang dune 3.17)

(name binance-client)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/binance-client))

(package
 (name binance-client)
 (synopsis "Async client for the Binance USD-M Futures API")
 (description
  "REST, market and user-data websocket clients for Binance USD-M Futures on venue-net and binance-protocol, a local order book, and an mm-core venue adapter.")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  async_ssl
  cohttp
  uri
  yojson
  ppx_jane
  ppx_yojson_conv
  venue-net
  binance-protocol
  exchange-core
  mm-core
  fixed-decimal
  (cohttp-async :with-test)
  (binance-sim :with-test)
  (exchange-core-engine :with-test)))
