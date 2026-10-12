(* Command-line smoke tests for binance-client, e.g. against the simulator:

   binance-cli depth BTCUSDT binance-cli book-ticker BTCUSDT -network mainnet binance-cli
   watch -depth BTCUSDT -book-ticker BTCUSDT -user *)

open! Core
open Async
open Binance_protocol
open Binance_client

let config_param =
  let%map_open.Command network =
    flag
      "network"
      (optional_with_default
         `Local
         (Arg_type.of_alist_exn
            [ "local", `Local; "mainnet", `Mainnet; "testnet", `Testnet ]))
      ~doc:"NETWORK local (default, the simulator), mainnet or testnet"
  and base_url =
    flag "base-url" (optional string) ~doc:"URL REST root (overrides -network)"
  and ws_url = flag "ws-url" (optional string) ~doc:"URL websocket root"
  and routed =
    flag "routed" no_arg ~doc:" use the /public, /market and /private websocket paths"
  and debug = flag "debug" no_arg ~doc:" log at debug level" in
  if debug then Log.Global.set_level `Debug;
  Log.Global.set_output [ Log.Output.stderr () ];
  let env name = Sys.getenv name |> Option.filter ~f:(Fn.non String.is_empty) in
  let api_key = Option.value (env "BINANCE_API_KEY") ~default:"binance-cli" in
  let api_secret = env "BINANCE_API_SECRET" in
  let base : Config.t =
    (* Market data needs no secret, so a missing one is allowed everywhere. *)
    let with_secret (config : Config.t) =
      { config with keys = { config.keys with api_secret } }
    in
    match network with
    | `Local -> Config.local ?api_secret ~api_key ()
    | `Mainnet -> with_secret (Config.mainnet ~api_key ~api_secret:"" ())
    | `Testnet -> with_secret (Config.testnet ~api_key ~api_secret:"" ())
  in
  let base =
    match base_url with
    | None -> base
    | Some url ->
      { base with
        base_url = Uri.of_string url
      ; ws_url = Config.ws_url_of_base_url (Uri.of_string url)
      }
  in
  { base with
    ws_url = Option.value_map ws_url ~default:base.ws_url ~f:Uri.of_string
  ; routed_ws = base.routed_ws || routed
  }
;;

let symbol_arg =
  Command.Param.(anon ("SYMBOL" %: Command.Arg_type.map string ~f:Symbol.of_string))
;;

let print_result sexp_of = function
  | Ok value ->
    print_s (sexp_of value);
    return ()
  | Error error ->
    Print.eprint_s [%sexp (error : Error.t)];
    exit 1
;;

let depth_command =
  Command.async
    ~summary:"Print an order book snapshot (GET /fapi/v1/depth)"
    (let%map_open.Command config = config_param
     and symbol = symbol_arg
     and limit =
       flag "limit" (optional int) ~doc:"N levels (5, 10, 20, 50, 100, 500, 1000)"
     in
     fun () -> Rest.depth ?limit config symbol >>= print_result [%sexp_of: Depth.t])
;;

let book_ticker_command =
  Command.async
    ~summary:"Print the best bid and ask (GET /fapi/v1/ticker/bookTicker)"
    (let%map_open.Command config = config_param
     and symbol = symbol_arg in
     fun () -> Rest.book_ticker config symbol >>= print_result [%sexp_of: Book_ticker.t])
;;

let watch_command =
  Command.async
    ~summary:"Subscribe to streams and print every event"
    (let%map_open.Command config = config_param
     and depth = flag "depth" (listed string) ~doc:"SYMBOL diff depth (@depth@100ms)"
     and book =
       flag "book" (listed string) ~doc:"SYMBOL a local order book, printed top 5"
     and book_ticker = flag "book-ticker" (listed string) ~doc:"SYMBOL @bookTicker"
     and agg_trade = flag "agg-trade" (listed string) ~doc:"SYMBOL @aggTrade"
     and mark_price = flag "mark-price" (listed string) ~doc:"SYMBOL @markPrice@1s"
     and route =
       flag
         "route"
         (optional_with_default
            Config.Route.Public
            (Arg_type.of_alist_exn [ "public", Config.Route.Public; "market", Market ]))
         ~doc:"ROUTE websocket path when routed: public (default) or market"
     and user = flag "user" no_arg ~doc:" the user data stream (needs BINANCE_API_KEY)" in
     fun () ->
       let symbols names = List.map names ~f:Symbol.of_string in
       let streams =
         List.concat
           [ List.map
               (symbols (depth @ book))
               ~f:(fun symbol -> Stream_name.Diff_depth { symbol; speed = Some Ms_100 })
           ; List.map (symbols book_ticker) ~f:(fun s -> Stream_name.Book_ticker s)
           ; List.map (symbols agg_trade) ~f:(fun s -> Stream_name.Agg_trade s)
           ; List.map (symbols mark_price) ~f:(fun symbol ->
               Stream_name.Mark_price { symbol; every_second = true })
           ]
       in
       if List.is_empty streams && not user
       then (
         Print.eprintf "nothing to watch; pass at least one stream flag\n";
         exit 2)
       else (
         let%bind () =
           if not user
           then return ()
           else (
             let%map user =
               User_ws.connect
                 ~on_reconnect:(fun () -> Print.eprintf "user stream reconnected\n")
                 config
               >>| ok_exn
             in
             don't_wait_for
               (Pipe.iter_without_pushback (User_ws.events user) ~f:(fun event ->
                  print_s [%sexp (event : User_event.t)])))
         in
         if List.is_empty streams
         then Deferred.never ()
         else (
           let books =
             List.map (symbols book) ~f:(fun symbol ->
               ( symbol
               , Book_sync.create config symbol ~levels:5 ~on_book:(fun l2 ->
                   print_s [%sexp (l2 : Exchange_core.L2.t)]) ))
           in
           let%bind market =
             Market_ws.connect
               ~route
               ~on_reconnect:(fun () ->
                 Print.eprintf "market stream reconnected\n";
                 List.iter books ~f:(fun (_, sync) -> Book_sync.reset sync))
               config
             >>| ok_exn
           in
           let%bind () = Market_ws.subscribe market streams >>| ok_exn in
           List.iter books ~f:(fun (_, sync) -> Book_sync.start sync);
           let book_symbols = List.map books ~f:fst in
           Pipe.iter_without_pushback
             (Market_ws.events market)
             ~f:(fun ({ data; _ } as event : Market_event.t Combined.t) ->
               match data with
               | Depth_update update
                 when List.mem book_symbols update.symbol ~equal:Symbol.equal ->
                 List.iter books ~f:(fun (_, sync) -> Book_sync.handle sync update);
                 if List.mem (symbols depth) update.symbol ~equal:Symbol.equal
                 then print_s [%sexp (event : Market_event.t Combined.t)]
               | _ -> print_s [%sexp (event : Market_event.t Combined.t)]))))
;;

let feed_command =
  Command.async
    ~summary:"Print normalized market data (Live_feed) as sexps"
    (let%map_open.Command config = config_param
     and symbols =
       flag "symbol" (one_or_more_as_list string) ~doc:"SYMBOL e.g. BTCUSDT (repeatable)"
     and streams =
       flag
         "streams"
         (optional_with_default
            Market_data.Stream.[ Trades; Bbo; L2; Funding ]
            (Arg_type.comma_separated (Arg_type.create Market_data.Stream.of_string)))
         ~doc:"STREAMS comma-separated: trades, bbo, l2, funding (default: all)"
     in
     fun () ->
       let%bind feed = Live_feed.create config ~symbols ~streams in
       let module F = (val feed : Market_data.Feed.S) in
       let print pipe ~f =
         Pipe.iter_without_pushback pipe ~f:(fun x ->
           print_s ~mach:() [%sexp (f x : Market_data.Event.t)])
       in
       Deferred.all_unit
         [ print F.trades ~f:(fun x -> Trade x)
         ; print F.bbo ~f:(fun x -> Bbo x)
         ; print F.l2 ~f:(fun x -> L2 x)
         ; print F.funding ~f:(fun x -> Funding x)
         ])
;;

let () =
  Command.group
    ~summary:"Binance USD-M Futures API client"
    [ "depth", depth_command
    ; "book-ticker", book_ticker_command
    ; "watch", watch_command
    ; "feed", feed_command
    ]
  |> Command_unix.run
;;
