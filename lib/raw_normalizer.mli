(** {!Feed_normalizer} as a {!Market_data.Normalizer.S}, so [rebuild-norm] can turn
    market-recorder raw files of Binance back into events:
    [Market_data.Rebuild_norm.command [ (module Binance_client.Raw_normalizer) ]].

    It uses {!Feed_normalizer.create_replay}: books exist for every symbol seen, and the
    [GET /depth] bodies {!Live_feed} records on the l2 stream sync them. [Need_snapshot]
    outputs are dropped (there is no connection to fetch with); the book waits for the
    next recorded snapshot, as the live feed did. A [disconnect] clears the books. *)

include Market_data.Normalizer.S with type t = Feed_normalizer.t
