open! Core

let streams = Market_data.Stream.[ Trades; Bbo; L2; Funding ]

let file (stream : Market_data.Stream.t) =
  match stream with
  | Trades -> "trades"
  | Bbo -> "bbo"
  | L2 -> "l2"
  | Funding -> "funding"
  | Private -> raise_s [%message "no private fixture"]
;;

let frames stream =
  In_channel.read_lines ("fixtures/" ^ file stream ^ ".jsonl")
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let is_snapshot frame = String.is_prefix frame ~prefix:{|{"lastUpdateId"|}
