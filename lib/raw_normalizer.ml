open! Core

type t = Feed_normalizer.t

let venue = Market_data.Venue.Binance
let create () = Feed_normalizer.create_replay ()

let frame t ~recv_ts payload =
  Or_error.map (Feed_normalizer.frame t ~recv_ts payload) ~f:(fun outputs ->
    List.filter_map outputs ~f:(function
      | Feed_normalizer.Output.Event event -> Some event
      | Need_snapshot _ -> None))
;;

let disconnect = Feed_normalizer.reset
