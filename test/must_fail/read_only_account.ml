(* A key-less config cannot reach a signed endpoint. *)
let account (config : Binance_client.Config.Read_only.t) =
  Binance_client.Rest.account config
;;
