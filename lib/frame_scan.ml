open! Core

module Kind = struct
  type t =
    | Agg_trade
    | Book_ticker
    | Depth_update
  [@@deriving sexp_of, equal]
end

(* Value offsets per known key: [starts.(k)] is the first byte of the value (the opening
   quote of a string), [ends.(k)] the byte after it, [-1] when the key is absent. *)
type t =
  { mutable kind : Kind.t
  ; mutable symbol_pos : int
  ; mutable symbol_len : int
  ; mutable event_time : int
  ; mutable transaction_time : int
  ; mutable agg_trade_id : int
  ; mutable price : int
  ; mutable qty : int
  ; mutable buyer_is_maker : bool
  ; mutable update_id : int
  ; mutable bid_price : int
  ; mutable bid_qty : int
  ; mutable ask_price : int
  ; mutable ask_qty : int
  ; mutable first_update_id : int
  ; mutable final_update_id : int
  ; mutable prev_final_update_id : int
  ; mutable bids : int
  ; mutable asks : int
  ; mutable level_price : int
  ; mutable level_qty : int
  ; fields : fields
  }

and fields =
  { starts : int array
  ; ends : int array
  }

(* Key ids. *)
let k_e = 0
let k_big_e = 1
let k_big_t = 2
let k_s = 3
let k_a = 4
let k_p = 5
let k_q = 6
let k_nq = 7
let k_f = 8
let k_l = 9
let k_m = 10
let k_u = 11
let k_b = 12
let k_big_b = 13
let k_big_a = 14
let k_big_u = 15
let k_pu = 16
let n_keys = 17

(* Keys that make a bare payload a subscription response or a combined frame. *)
let k_forbidden = n_keys

let create () =
  { kind = Agg_trade
  ; symbol_pos = 0
  ; symbol_len = 0
  ; event_time = 0
  ; transaction_time = 0
  ; agg_trade_id = 0
  ; price = 0
  ; qty = 0
  ; buyer_is_maker = false
  ; update_id = 0
  ; bid_price = 0
  ; bid_qty = 0
  ; ask_price = 0
  ; ask_qty = 0
  ; first_update_id = 0
  ; final_update_id = 0
  ; prev_final_update_id = 0
  ; bids = -1
  ; asks = -1
  ; level_price = 0
  ; level_qty = 0
  ; fields =
      { starts = Array.create ~len:n_keys (-1); ends = Array.create ~len:n_keys (-1) }
  }
;;

let bad = Int.min_value

(* NUL past the end: it never matches a byte the scanner expects. *)
let[@zero_alloc] [@inline] get s i =
  if i >= 0 && i < String.length s then String.unsafe_get s i else '\000'
;;

let[@zero_alloc] [@inline] is_digit c = Char.( >= ) c '0' && Char.( <= ) c '9'

let[@zero_alloc] is_lit s pos lit =
  let k = String.length lit in
  if pos < 0 || pos + k > String.length s
  then false
  else (
    let mutable j = 0 in
    while j < k && Char.equal (String.unsafe_get s (pos + j)) (String.unsafe_get lit j) do
      j <- j + 1
    done;
    j = k)
;;

let[@zero_alloc] key_id s pos len ~bare =
  if len = 1
  then (
    match String.unsafe_get s pos with
    | 'e' -> k_e
    | 'E' -> k_big_e
    | 'T' -> k_big_t
    | 's' -> k_s
    | 'a' -> k_a
    | 'p' -> k_p
    | 'q' -> k_q
    | 'f' -> k_f
    | 'l' -> k_l
    | 'm' -> k_m
    | 'u' -> k_u
    | 'b' -> k_b
    | 'B' -> k_big_b
    | 'A' -> k_big_a
    | 'U' -> k_big_u
    | _ -> -1)
  else if len = 2
  then if is_lit s pos "nq" then k_nq else if is_lit s pos "pu" then k_pu else -1
  else if bare
          && ((len = 6 && (is_lit s pos "result" || is_lit s pos "stream"))
              || (len = 4 && is_lit s pos "code"))
  then k_forbidden
  else -1
;;

(* [i] is an opening quote; the offset of the closing quote, or [-1] on an escape, a
   control character or the end of the frame. *)
let[@zero_alloc] string_end s i =
  let n = String.length s in
  let mutable j = i + 1 in
  let mutable result = -1 in
  let mutable stop = false in
  while (not stop) && j < n do
    let c = String.unsafe_get s j in
    if Char.equal c '"'
    then (
      result <- j;
      stop <- true)
    else if Char.equal c '\\' || Char.to_int c < 0x20
    then stop <- true
    else j <- j + 1
  done;
  result
;;

let[@zero_alloc] digits_end s i =
  let mutable j = i in
  while is_digit (get s j) do
    j <- j + 1
  done;
  j
;;

(* A JSON number, [true], [false] or [null] at [i]: the offset after it, or [-1]. *)
let[@zero_alloc] scalar_end s i =
  if is_lit s i "true" || is_lit s i "null"
  then i + 4
  else if is_lit s i "false"
  then i + 5
  else (
    let mutable j = i in
    if Char.equal (get s j) '-' then j <- j + 1;
    if not (is_digit (get s j))
    then -1
    else (
      if Char.equal (get s j) '0' then j <- j + 1 else j <- digits_end s j;
      let mutable ok = true in
      if Char.equal (get s j) '.'
      then (
        let k = digits_end s (j + 1) in
        if k = j + 1 then ok <- false else j <- k);
      if ok && (Char.equal (get s j) 'e' || Char.equal (get s j) 'E')
      then (
        let mutable k = j + 1 in
        if Char.equal (get s k) '+' || Char.equal (get s k) '-' then k <- k + 1;
        let d = digits_end s k in
        if d = k then ok <- false else j <- d);
      if ok then j else -1))
;;

let[@zero_alloc] decimal s ~pos ~stop =
  let mutable i = pos in
  let negative = Char.equal (get s i) '-' in
  if i < stop && (negative || Char.equal (get s i) '+') then i <- i + 1;
  let mutable whole = 0 in
  let mutable significant = 0 in
  let mutable int_digits = 0 in
  while i < stop && is_digit (get s i) do
    let d = Char.to_int (String.unsafe_get s i) - 48 in
    int_digits <- int_digits + 1;
    if significant > 0 || d <> 0
    then (
      significant <- significant + 1;
      if significant <= 11 then whole <- (whole * 10) + d);
    i <- i + 1
  done;
  let mutable frac = 0 in
  let mutable frac_digits = 0 in
  if i < stop && Char.equal (get s i) '.'
  then (
    i <- i + 1;
    while i < stop && is_digit (get s i) do
      if frac_digits < 8 then frac <- (frac * 10) + (Char.to_int (String.unsafe_get s i) - 48);
      frac_digits <- frac_digits + 1;
      i <- i + 1
    done);
  let mutable k = Int.min frac_digits 8 in
  while k < 8 do
    frac <- frac * 10;
    k <- k + 1
  done;
  if i <> stop
     || (int_digits = 0 && frac_digits = 0)
     || significant > 11
     || whole > (Int.max_value - frac) / 100_000_000
  then bad
  else (
    let units = (whole * 100_000_000) + frac in
    if negative then -units else units)
;;

(* A JSON integer that fits in an [int] (at most 18 digits), spanning [pos, stop). *)
let[@zero_alloc] int_value s ~pos ~stop =
  let mutable i = pos in
  let negative = Char.equal (get s i) '-' in
  if negative then i <- i + 1;
  let first = i in
  let mutable v = 0 in
  while i < stop && is_digit (get s i) do
    v <- (v * 10) + (Char.to_int (String.unsafe_get s i) - 48);
    i <- i + 1
  done;
  if i <> stop || i = first || i - first > 18
  then bad
  else if negative
  then -v
  else v
;;

(* [i] is the [[] of a [["price","qty"]] level: the offset after its [], or [-1]. *)
let[@zero_alloc] level_end s i =
  if not (Char.equal (get s (i + 1)) '"')
  then -1
  else (
    let e1 = string_end s (i + 1) in
    if e1 < 0
       || decimal s ~pos:(i + 2) ~stop:e1 = bad
       || (not (Char.equal (get s (e1 + 1)) ','))
       || not (Char.equal (get s (e1 + 2)) '"')
    then -1
    else (
      let e2 = string_end s (e1 + 2) in
      if e2 < 0
         || decimal s ~pos:(e1 + 3) ~stop:e2 = bad
         || not (Char.equal (get s (e2 + 1)) ']')
      then -1
      else e2 + 2))
;;

(* [i] is the [[] of an array of levels: the offset after its [], or [-1]. *)
let[@zero_alloc] levels_end s i =
  if Char.equal (get s (i + 1)) ']'
  then i + 2
  else (
    let mutable j = i + 1 in
    let mutable result = -1 in
    let mutable stop = false in
    while not stop do
      if not (Char.equal (get s j) '[')
      then stop <- true
      else (
        let e = level_end s j in
        if e < 0
        then stop <- true
        else if Char.equal (get s e) ','
        then j <- e + 1
        else (
          if Char.equal (get s e) ']' then result <- e + 1;
          stop <- true))
    done;
    result)
;;

(* [i] is the [{] of the event object: records the value offsets of the known keys and
   returns the offset after the closing [}], or [-1]. *)
let[@zero_alloc] walk t s i ~bare =
  let starts = t.fields.starts
  and ends = t.fields.ends in
  for k = 0 to n_keys - 1 do
    Array.unsafe_set starts k (-1)
  done;
  let mutable j = i + 1 in
  let mutable result = -1 in
  let mutable stop = false in
  if Char.equal (get s j) '}'
  then (
    result <- j + 1;
    stop <- true);
  while not stop do
    let kend = if Char.equal (get s j) '"' then string_end s j else -1 in
    if kend < 0 || not (Char.equal (get s (kend + 1)) ':')
    then stop <- true
    else (
      let id = key_id s (j + 1) (kend - j - 1) ~bare in
      let v = kend + 2 in
      let c = get s v in
      let vend =
        if id = k_forbidden || Char.equal c '{'
        then -1
        else if Char.equal c '"'
        then (
          let e = string_end s v in
          if e < 0 then -1 else e + 1)
        else if Char.equal c '['
        then if id = k_b || id = k_a then levels_end s v else -1
        else scalar_end s v
      in
      if vend < 0
      then stop <- true
      else if id >= 0 && starts.(id) >= 0
      then (* a duplicated key: leave it to the Yojson decoder *)
        stop <- true
      else (
        if id >= 0
        then (
          starts.(id) <- v;
          ends.(id) <- vend);
        let d = get s vend in
        if Char.equal d ','
        then j <- vend + 1
        else (
          if Char.equal d '}' then result <- vend + 1;
          stop <- true)))
  done;
  result
;;

let[@zero_alloc] int_field t s k =
  let start = t.fields.starts.(k) in
  if start < 0 then bad else int_value s ~pos:start ~stop:t.fields.ends.(k)
;;

let[@zero_alloc] decimal_field t s k =
  let start = t.fields.starts.(k) in
  if start < 0 || not (Char.equal (get s start) '"')
  then bad
  else decimal s ~pos:(start + 1) ~stop:(t.fields.ends.(k) - 1)
;;

let[@zero_alloc] is_string_field t s k =
  let start = t.fields.starts.(k) in
  start >= 0 && Char.equal (get s start) '"'
;;

let[@zero_alloc] is_array_field t s k =
  let start = t.fields.starts.(k) in
  start >= 0 && Char.equal (get s start) '['
;;

let[@zero_alloc] field_is t s k lit =
  let start = t.fields.starts.(k) in
  start >= 0 && t.fields.ends.(k) - start = String.length lit && is_lit s start lit
;;

let[@zero_alloc] read_agg_trade t s =
  t.kind <- Agg_trade;
  t.event_time <- int_field t s k_big_e;
  t.agg_trade_id <- int_field t s k_a;
  t.price <- decimal_field t s k_p;
  t.qty <- decimal_field t s k_q;
  t.transaction_time <- int_field t s k_big_t;
  t.buyer_is_maker <- field_is t s k_m "true";
  (* [nq] is optional: absent, null or a decimal string. *)
  let nq_ok =
    t.fields.starts.(k_nq) < 0
    || field_is t s k_nq "null"
    || decimal_field t s k_nq <> bad
  in
  nq_ok
  && t.event_time <> bad
  && t.agg_trade_id <> bad
  && t.price <> bad
  && t.qty <> bad
  && t.transaction_time <> bad
  && int_field t s k_f <> bad
  && int_field t s k_l <> bad
  && (t.buyer_is_maker || field_is t s k_m "false")
;;

let[@zero_alloc] read_book_ticker t s =
  t.kind <- Book_ticker;
  t.update_id <- int_field t s k_u;
  t.event_time <- int_field t s k_big_e;
  t.transaction_time <- int_field t s k_big_t;
  t.bid_price <- decimal_field t s k_b;
  t.bid_qty <- decimal_field t s k_big_b;
  t.ask_price <- decimal_field t s k_a;
  t.ask_qty <- decimal_field t s k_big_a;
  t.update_id <> bad
  && t.event_time <> bad
  && t.transaction_time <> bad
  && t.bid_price <> bad
  && t.bid_qty <> bad
  && t.ask_price <> bad
  && t.ask_qty <> bad
;;

let[@zero_alloc] read_depth_update t s =
  t.kind <- Depth_update;
  t.event_time <- int_field t s k_big_e;
  t.transaction_time <- int_field t s k_big_t;
  t.first_update_id <- int_field t s k_big_u;
  t.final_update_id <- int_field t s k_u;
  t.prev_final_update_id <- int_field t s k_pu;
  t.bids <- t.fields.starts.(k_b);
  t.asks <- t.fields.starts.(k_a);
  t.event_time <> bad
  && t.transaction_time <> bad
  && t.first_update_id <> bad
  && t.final_update_id <> bad
  && t.prev_final_update_id <> bad
  && is_array_field t s k_b
  && is_array_field t s k_a
;;

let[@zero_alloc] read_event t s =
  if not (is_string_field t s k_e && is_string_field t s k_s)
  then false
  else (
    let symbol = t.fields.starts.(k_s) in
    t.symbol_pos <- symbol + 1;
    t.symbol_len <- t.fields.ends.(k_s) - symbol - 2;
    if field_is t s k_e {|"aggTrade"|}
    then read_agg_trade t s
    else if field_is t s k_e {|"bookTicker"|}
    then read_book_ticker t s
    else if field_is t s k_e {|"depthUpdate"|}
    then read_depth_update t s
    else false)
;;

let combined_prefix = {|{"stream":"|}
let data_key = {|,"data":|}

let[@zero_alloc] scan t s =
  let n = String.length s in
  if is_lit s 0 combined_prefix
  then (
    let stream_end = string_end s (String.length combined_prefix - 1) in
    let data = stream_end + 1 + String.length data_key in
    if stream_end < 0
       || (not (is_lit s (stream_end + 1) data_key))
       || not (Char.equal (get s data) '{')
    then false
    else (
      let e = walk t s data ~bare:false in
      e > 0 && e = n - 1 && Char.equal (get s e) '}' && read_event t s))
  else if Char.equal (get s 0) '{'
  then walk t s 0 ~bare:true = n && read_event t s
  else false
;;

let[@zero_alloc] next_level t s pos =
  let c = get s pos in
  if not (Char.equal c '[' || Char.equal c ',')
  then -1
  else if not (Char.equal (get s (pos + 1)) '[')
  then -1
  else (
    let q1 = pos + 2 in
    let e1 = string_end s q1 in
    let q2 = e1 + 2 in
    let e2 = string_end s q2 in
    if e1 < 0 || e2 < 0
    then -1
    else (
      t.level_price <- decimal s ~pos:(q1 + 1) ~stop:e1;
      t.level_qty <- decimal s ~pos:(q2 + 1) ~stop:e2;
      e2 + 2))
;;

let[@zero_alloc] symbol_equal t s symbol =
  t.symbol_len = String.length symbol && is_lit s t.symbol_pos symbol
;;
