(** {!Mm_core.Venue.S} over Binance USD-M Futures (or the [binance-sim] simulator),
    converted to [exchange-core] types.

    - [connect] fetches [exchangeInfo], opens the user data stream ({!User_ws}) and one
      public combined-stream connection ({!Market_ws}).
    - Books: a full local book per symbol from [GET /fapi/v1/depth] plus
      [<symbol>@depth@100ms] diffs ({!Book_sync}, {!Local_book}), reported after every
      applied diff, top 50 levels per side, [n = 0]. This was chosen over [@depth20]
      partial snapshots, whose payload shape is unverified for USD-M and which do not
      reveal lost updates. After a reconnect the book is resynced from a new snapshot.
    - Fills: [ORDER_TRADE_UPDATE] events with execution type [TRADE] (or [CALCULATED],
      liquidations), each trade id once. The commission ([n]) is taken as USD whatever its
      asset ([N]); [closed_pnl] is [rp].
    - Order updates: [ORDER_TRADE_UPDATE] statuses. [NEW] is [New], [PARTIALLY_FILLED]
      [Partially_filled], [FILLED] [Filled]; [CANCELED], [EXPIRED] and [EXPIRED_IN_MATCH]
      are [Cancelled] (Binance reports post-only and IOC expiries this way).
    - [place] sends [batchOrders] in groups of 5 (a single order goes to [POST /order]),
      with [newOrderRespType=RESULT]. A per-order [{"code", "msg"}] becomes [Rejected];
      the reason comes from {!Binance_protocol.Api_error.to_reject_reason}, otherwise
      [Unsupported msg]. An order that expired with nothing filled is [Rejected]: post
      only ([GTX]) as [Post_only_would_cross], IOC as [No_liquidity], [EXPIRED_IN_MATCH]
      as [Self_trade]. Market orders are sent as Binance [MARKET] orders.
    - [cancel] sends [DELETE /fapi/v1/order] per id, in parallel; unknown orders ([-2011],
      [-2013]) are not errors. [cancel_all] is [DELETE /fapi/v1/allOpenOrders].
    - [account] combines [GET /fapi/v2/account] and [GET /fapi/v2/positionRisk]; the
      account id is the API key.
    - [instrument]: [exchangeInfo] filters ([PRICE_FILTER], [LOT_SIZE]); [max_leverage] is
      the account's current leverage on the symbol from [positionRisk] (Binance's maximum
      lives in the leverage brackets endpoint, which the simulator lacks), and fees are
      Binance's base tier (taker 5 bps, maker 2 bps). *)

open! Core
open! Async
open Binance_protocol
include Mm_core.Venue.S with type config = Config.t

(** Pure conversions from Binance wire types, exposed for testing. *)
module Convert : sig
  val ns_of_ms : Timestamp_ms.t -> int
  val fill : Order_trade_update.t -> Mm_core.Fill.t option
  val order_update : Order_trade_update.t -> Mm_core.Order_update.t option
  val reject_reason : Api_error.t -> Exchange_core.Reject_reason.t
  val order_status : Order.t -> Exchange_core.Order_status.t

  (** [Error] for client ids Binance does not accept. *)
  val order_request
    :  Exchange_core.Order_request.t
    -> (New_order.Request.t, Exchange_core.Reject_reason.t) Result.t

  val account
    :  account:Exchange_core.Account.t
    -> Account.t
    -> Position_risk.t list
    -> now:Time_ns.t
    -> Exchange_core.Account_view.t
end
