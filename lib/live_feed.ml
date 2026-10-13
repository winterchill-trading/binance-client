open! Core
open Async
open Binance_protocol
module Ws_client = Venue_net.Ws_client
module Stream = Market_data.Stream

let stream_names (stream : Stream.t) symbols : Config.Route.t * Stream_name.t list =
  match stream with
  | Trades -> Market, List.map symbols ~f:(fun s -> Stream_name.Agg_trade s)
  | Bbo -> Public, List.map symbols ~f:(fun s -> Stream_name.Book_ticker s)
  | L2 ->
    ( Public
    , List.map symbols ~f:(fun symbol ->
        Stream_name.Diff_depth { symbol; speed = Some Ms_100 }) )
  | Funding ->
    ( Market
    , List.map symbols ~f:(fun symbol ->
        Stream_name.Mark_price { symbol; every_second = true }) )
  | Private -> raise_s [%message "Live_feed: no public Private stream"]
;;

type t =
  { config : Config.Read_only.t
  ; time_source : Time_source.t
  ; normalizer : Feed_normalizer.t
  ; on_raw : stream:Stream.t -> recv_ts:Time_ns.t -> string -> unit
  ; fetching : Symbol.Hash_set.t
  ; synced : Symbol.Hash_set.t
  ; mutable next_id : int
  ; mutable closed : bool
  ; mutable clients : Ws_client.t list
  ; mutable frames : int
  ; mutable parse_errors : int
  ; trades : Market_data.Trade.t Pipe.Writer.t
  ; bbo : Market_data.Bbo.t Pipe.Writer.t
  ; l2 : Market_data.L2_update.t Pipe.Writer.t
  ; funding : Market_data.Funding.t Pipe.Writer.t
  ; gaps : Market_data.Gap.t Pipe.Writer.t
  ; readers :
      Market_data.Trade.t Pipe.Reader.t
      * Market_data.Bbo.t Pipe.Reader.t
      * Market_data.L2_update.t Pipe.Reader.t
      * Market_data.Funding.t Pipe.Reader.t
      * Market_data.Gap.t Pipe.Reader.t
  }

let emit t (event : Market_data.Event.t) =
  match event with
  | Trade x -> Pipe.write_without_pushback_if_open t.trades x
  | Bbo x -> Pipe.write_without_pushback_if_open t.bbo x
  | L2 x -> Pipe.write_without_pushback_if_open t.l2 x
  | Funding x -> Pipe.write_without_pushback_if_open t.funding x
;;

let gap t ?instrument ~stream () =
  if not t.closed
  then
    Pipe.write_without_pushback_if_open
      t.gaps
      { Market_data.Gap.recv_ts = Time_source.now t.time_source
      ; venue = Binance
      ; instrument
      ; stream = Some stream
      ; reason = Resync
      }
;;

let snapshot_limit = 1000
let retry_delay = Time_ns.Span.of_int_ms 200
let error_delay = Time_ns.Span.of_int_sec 1

let fetch_depth t symbol =
  match%map
    Rest.call_public
      ~params:[ "symbol", Symbol.to_string symbol; "limit", Int.to_string snapshot_limit ]
      t.config
      `GET
      "/fapi/v1/depth"
  with
  | Error _ as error -> error
  | Ok json ->
    let recv_ts = Time_source.now t.time_source in
    t.on_raw ~stream:L2 ~recv_ts (Yojson.Safe.to_string json);
    Or_error.try_with (fun () -> recv_ts, Depth.t_of_yojson json)
;;

(* One snapshot loop per symbol at a time; it stops once the book is synced. *)
let rec sync_book t symbol =
  if t.closed || Feed_normalizer.is_synced t.normalizer symbol
  then (
    Hash_set.remove t.fetching symbol;
    return ())
  else (
    match%bind fetch_depth t symbol with
    | Error error ->
      Log.Global.error_s
        [%message
          "binance feed: depth snapshot failed" (symbol : Symbol.t) (error : Error.t)];
      let%bind () = Time_source.after t.time_source error_delay in
      sync_book t symbol
    | Ok (recv_ts, depth) ->
      (match Feed_normalizer.snapshot t.normalizer ~recv_ts symbol depth with
       | `Synced update ->
         Hash_set.add t.synced symbol;
         if not t.closed then emit t (L2 update);
         sync_book t symbol
       | `Ignored -> sync_book t symbol
       | `Gap ->
         let%bind () = Time_source.after t.time_source retry_delay in
         sync_book t symbol))
;;

let need_snapshot t symbol =
  if (not t.closed) && not (Hash_set.mem t.fetching symbol)
  then (
    Hash_set.add t.fetching symbol;
    (* A book that was synced before lost its sequence; one that never was is starting. *)
    if Hash_set.mem t.synced symbol
    then (
      Hash_set.remove t.synced symbol;
      gap t ~instrument:(Feed_normalizer.instrument symbol) ~stream:L2 ());
    don't_wait_for (sync_book t symbol))
;;

let on_frame t ~stream frame =
  let recv_ts = Time_source.now t.time_source in
  t.frames <- t.frames + 1;
  t.on_raw ~stream ~recv_ts frame;
  match Feed_normalizer.frame t.normalizer ~recv_ts frame with
  | Ok outputs ->
    List.iter outputs ~f:(function
      | Event event -> emit t event
      | Need_snapshot symbol -> need_snapshot t symbol)
  | Error error ->
    t.parse_errors <- t.parse_errors + 1;
    Log.Global.debug_s
      [%message
        "binance feed: undecodable frame"
          (stream : Stream.t)
          (frame : string)
          (error : Error.t)]
;;

let subscribe t client names =
  Deferred.List.iter (List.chunks_of names ~length:50) ~how:`Sequential ~f:(fun params ->
    let id = t.next_id in
    t.next_id <- id + 1;
    Ws_client.send
      client
      (Yojson.Safe.to_string
         (Subscribe.Request.yojson_of_t { method_ = Subscribe; params; id })))
;;

let connect t ~stream symbols =
  let route, names = stream_names stream symbols in
  let on_connect client =
    (* Diff events were lost while disconnected: rebuild the books from new snapshots,
       which the first buffered events ask for. *)
    if Ws_client.reconnects client > 0
    then (
      gap t ~stream ();
      if Stream.equal stream L2
      then (
        Feed_normalizer.reset t.normalizer;
        (* The connection-wide gap covers every book. *)
        Hash_set.clear t.synced));
    subscribe t client names
  in
  (* Frames go to [on_frame] straight from the connection ([on_message]) rather than
     through [Ws_client.messages], which saves the pipe hop, about 13 words per frame.
     [on_connect] still runs before the first frame of each connection, so a
     reconnection's gap is written before any of its frames are read. *)
  let%map client =
    Ws_client.connect
      ~ping:(`Frame (Time_ns.Span.of_int_sec 30))
      ~on_connect
      ~on_message:(on_frame t ~stream)
      ~log_name:(sprintf "binance feed %s" (Stream.to_string stream))
      ~time_source:t.time_source
      ~url:(Config.combined_stream_uri t.config ~route)
      ()
    >>| ok_exn
  in
  t.clients <- client :: t.clients
;;

let close t =
  if t.closed
  then return ()
  else (
    t.closed <- true;
    let%map () = Deferred.List.iter t.clients ~how:`Parallel ~f:Ws_client.close in
    Pipe.close t.trades;
    Pipe.close t.bbo;
    Pipe.close t.l2;
    Pipe.close t.funding;
    Pipe.close t.gaps)
;;

module Stats = struct
  type t =
    { frames : int
    ; parse_errors : int
    ; reconnects : int
    }
  [@@deriving sexp_of]
end

let stats t : Stats.t =
  { frames = t.frames
  ; parse_errors = t.parse_errors
  ; reconnects = List.sum (module Int) t.clients ~f:Ws_client.reconnects
  }
;;

let start
  ?(on_raw = fun ~stream:_ ~recv_ts:_ _ -> ())
  ?(time_source = Time_source.wall_clock ())
  config
  ~symbols
  ~streams
  =
  let symbols =
    List.map symbols ~f:Symbol.of_string |> List.dedup_and_sort ~compare:Symbol.compare
  in
  let streams = List.dedup_and_sort streams ~compare:Stream.compare in
  if List.mem streams Private ~equal:Stream.equal
  then raise_s [%message "Live_feed: no public Private stream"];
  let trades_r, trades = Pipe.create () in
  let bbo_r, bbo = Pipe.create () in
  let l2_r, l2 = Pipe.create () in
  let funding_r, funding = Pipe.create () in
  let gaps_r, gaps = Pipe.create () in
  let t =
    { config = Config.Read_only.of_config config
    ; time_source
    ; normalizer =
        Feed_normalizer.create
          ~l2_symbols:(if List.mem streams L2 ~equal:Stream.equal then symbols else [])
    ; on_raw
    ; fetching = Symbol.Hash_set.create ()
    ; synced = Symbol.Hash_set.create ()
    ; next_id = 1
    ; closed = false
    ; clients = []
    ; frames = 0
    ; parse_errors = 0
    ; trades
    ; bbo
    ; l2
    ; funding
    ; gaps
    ; readers = trades_r, bbo_r, l2_r, funding_r, gaps_r
    }
  in
  let%map () =
    match%map
      Monitor.try_with (fun () ->
        Deferred.List.iter streams ~how:`Parallel ~f:(fun stream ->
          connect t ~stream symbols))
    with
    | Ok () -> ()
    | Error exn ->
      don't_wait_for (close t);
      raise exn
  in
  t
;;

let feed_with_gaps t =
  let trades_r, bbo_r, l2_r, funding_r, gaps_r = t.readers in
  (module struct
    let trades = trades_r
    let bbo = bbo_r
    let l2 = l2_r
    let funding = funding_r
    let gaps = gaps_r
    let close () = close t
  end : Market_data.Feed.With_gaps.S)
;;

let feed t = Market_data.Feed.With_gaps.to_feed (feed_with_gaps t)

let create ?on_raw ?time_source config ~symbols ~streams =
  let%map t = start ?on_raw ?time_source config ~symbols ~streams in
  feed t
;;
