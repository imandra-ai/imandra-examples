(**

  Aesthetic Integration Limited
  Copyright (c) 2014 - 2018. All rights reserved.

  Visit https://www.imandra.ai for further information.

*)


type price = int;;

type mkt_data = {
  nbb : price;
  nbo : price;
  l_up : price;
  l_down : price };;

(* Three conditions for the nbbo *)

type mkt_cond = MKT_NORMAL | MKT_CROSSED | MKT_LOCKED;;

let which_mkt (mkt) =
  if mkt.nbo = mkt.nbb then MKT_LOCKED
  else if mkt.nbo < mkt.nbb then MKT_CROSSED
  else MKT_NORMAL;;

let mid_point mkt = (mkt.nbb + mkt.nbo) / 2;;

let is_price_in_luld (p, mkt) =
  mkt.l_down <= p && p <= mkt.l_up;;

(* ************************************************************************* *)
type time = int;;
let standard_expiry_time = 100;;
let time_geq (t1, t2) = t1 >= t2;;

(* ************************************************************************* *)
let dec_units = 10;;

(** Helper functions to convert between various *)
let dec_to_float (d) =
    (float_of_int (d / dec_units)) +. ((float_of_int (d mod dec_units)) /. (float_of_int dec_units));;

let float_to_dec (f) =
    int_of_float (f *. float_of_int(dec_units));;

(* ************************************************************************* *)

type static_data = { round_lot: int;
                     tick_size: price };;

(* ************************************************************************* *)
type order_side = BUY | SELL | SELL_SHORT;;
type order_peg = NEAR | MID | FAR | NO_PEG;;

type order_type =
    MARKET
  | LIMIT
  | PEGGED
  | PEGGED_CI
  | LIMIT_CI
  | FIRM_UP_PEGGED
  | FIRM_UP_LIMIT
;;

(*
Section 2 defines the order types as follows:
 Order Types:
  -- Pegged Orders (both Resident and IOC TimeInForce).
      Pegging can be to the near, midpoint, or far
      side of the NBBO. Pegged Orders may have a limit price.
  -- Limit Orders (both Resident and IOC TimeInForce)
  -- Market Orders (both Resident and IOC TimeInForce)
 Conditional Indication Types:
  -- Pegged Conditional Indications (Resident T)
  -- Limit Conditional Indications (Resident TimeInForce only) *)

(* This applies to both Orders and Conditional Indications *)

type order_attr = RESIDENT | IOC;;

type category = C_ONE | C_TWO | C_THREE | C_FOUR;;

(*
Source Categories are defined as follows:
  -- Source Category 1:
      Retail orders routed by broker - dealer clients of the
       --- Retail Market Making business.
  -- Source Category 2: Certain orders received from --- algorithms,
       where the underlying client is an institutional client of ---.
  -- Source Category 3: Orders received from Order Originators that
       are determined by --- to exhibit lowto - neutral reversion.
  -- Source Category 4: All other orders not originating from Source
       Categories 1, 2 or 3.
*)

type capacity = Principal | Agency;;

(* ID of the order source *)

type order_source = int;;

(* Home ID in the model *)
let home_id = 12;;

(* Category crossing constraints *)

type category_elig = {
  c_one_elig : bool;
  c_two_elig : bool;
  c_three_elig : bool;
  c_four_elig : bool;
};;

let default_cat_elig = {
  c_one_elig = true;
  c_two_elig = true;
  c_three_elig = true;
  c_four_elig = true;
};;

(* Type for crossing restrictions *)

type cross_restrict = {
  cr_self_cross : bool;
  cr_ubs_principal : bool;
  cr_round_lot_only : bool;
  cr_no_locked_nbbo : bool;
  cr_pegged_mid_point_mode : int;

  (* From the examples, we understand that: 0 - no constraint 1 - mid      *)
  (* constraint 2 - limit constraint                                       *)

  cr_enable_conditionals : bool;
  cr_min_qty : bool;
  cr_cat_elig : category_elig;
};;

let default_cross_restrict = {
  cr_self_cross = false;
  cr_ubs_principal = false;
  cr_round_lot_only = false;
  cr_no_locked_nbbo = false;
  cr_pegged_mid_point_mode = 0;
  cr_enable_conditionals = false;
  cr_min_qty = false;
  cr_cat_elig = default_cat_elig;
};;

(* Note: there's both the quantity of the order and the filled quantity. *)

type order =
    { id : int;                        (* Order ID *)
      peg : order_peg;                 (* Near, Mid, Far or NoPeg *)
      client_id : int;                 (* Client ID *)
      order_type : order_type;         (* Market, Limit or Pegged order + Conditional Indications *)
      qty : int;                       (* Original quantity of the order (updated after cancel/replace) *)
      min_qty : int;                   (* Minimum acceptible quantity to trade *)
      leaves_qty : int;                (* Remaining quantity of the order *)
      price : price;                   (* Limit price (Not used if the order *)
      time : time;                     (* time of order entry (reset on update)) *)
      src : order_source;              (* ID of the order source *)
      order_attr : order_attr;         (* Resident or Immediate Or Cancel (IOC) *)
      capacity : capacity;             (* Principal or agency *)
      category : category;             (* Client category *)
      cross_restrict : cross_restrict; (* Crossing restrictions *)
      locate_found : bool;             (* A sell-short order without a locate would be rejected *)
      expiry_time : int;                                                       (* When will the order expire? *)
    };;

(* Uses two orders' timestamps to determin the older price of the two *)

let older_price (o1, o2) =
  if o1.time > o2.time then o2.price else o1.price;;

(* Functions for categorisations  *)

let cat_priority (o1, o2) =
  if o1.category = C_ONE then false
  else if o1.category = C_TWO &&
            (o2.category = C_THREE || o2.category = C_FOUR)
  then false
  else if o1.category = C_THREE && (o2.category = C_FOUR) then false
  else true;;

(* ****************************************************************************** *)
(* Functions for determining how much and at what price the two orders     *)
(* will trade                                                              *)
(* ****************************************************************************** *)

let eff_min_qty (o) = min o.min_qty o.leaves_qty;;

let effective_size (o, should_round, round_lot) =
  if should_round = true then
    ( if round_lot > 0 then ( (o.leaves_qty / round_lot) * round_lot )
      else o.leaves_qty )
  else
    o.leaves_qty;;

(* The pricing functions *)

let lessAggressive (side, lim_price, far_price) =
  if lim_price <= 0 then far_price else
    (if side = BUY then min lim_price  far_price 
     else max lim_price  far_price );;

(* This function is used to calculate the priority price *)
let priority_price (side, o, mkt) =
  let calc_pegged_price =
    ( match o.peg with
      | FAR -> lessAggressive(side, o.price,
                              (if side = BUY then mkt.nbo else mkt.nbb))
      | MID -> lessAggressive(side, o.price, (mid_point mkt))
      | NEAR -> lessAggressive(side, o.price,
                               (if side = BUY then mkt.nbb else mkt.nbo))
      | NO_PEG -> o.price )
  in
  let calc_nbbo_capped_limit =
    ( if side = BUY then lessAggressive (BUY, o.price, mkt.nbo)
      else lessAggressive (SELL, o.price, mkt.nbb ) )
  in

  (** Note that we're capping the limit price as per the following phrase in the Form ATS:
  "All marketable limit orders (i.e., buy orders with limit prices at or above the
   NBO or sell orders with limit prices at or below the NBB) will be treated as
   though they are at equivalent prices for priority purposes." *)
  match o.order_type with
  | LIMIT -> calc_nbbo_capped_limit
  | MARKET -> if side = BUY then mkt.nbo else mkt.nbb
  | PEGGED -> calc_pegged_price
  | PEGGED_CI -> calc_pegged_price
  | LIMIT_CI -> calc_nbbo_capped_limit
  | FIRM_UP_PEGGED -> calc_pegged_price
  | FIRM_UP_LIMIT -> calc_nbbo_capped_limit;;

(* This is used to calculate the actual price at which the order would trade   *)

let exec_price (side, o, mkt) =
  priority_price (side, o, mkt);;

(* Need to know static data: exchange tick size will be used to do this,
   unless they're crossing at the mid
   TODO: for now, using the tick as 1.0, but need to adjust to use 'step' *)

(* Section 3.3 Crossing Restrictions
The --- ATS allows all Order Originators to use the following optional
crossing restrictions, on a per - order or configured basis:
 - No Self Cross: To prevent crossing against 'own orders'
    (orders sent with the same client ID).
 - No --- Principal: To prevent crossing against --- BD Principal Orders,
    or --- affiliate Principal orders.
 - Round Lot Only: To prevent crossing in other than round lot orders.
 - No Locked: To prevent crossing on a pegged order when the NBBO is locked.
    (Bid = Offer)
 - PeggedMidPointMode: To prevent a Mid - point Pegged Order from being executed
    at its limit price if (i) in the case of a buy order, its limit price is less
    than the mid - point of the spread between the NBB and NBO and (ii) in the
    case of a sell order, its limit price is greater than the mid - point of the
    spread between the NBB and NBO. In the event this restriction is not
    elected, a Mid - Point Pegged Order with a specified limit price may be executed
    at the limit price even if the price is not equal to the mid - point of the
    spread between the NBB and the NBBO.
 - Enable Conditionals: To enable a Resident Order to interact with Conditional Indicators.
 - Minimum Quantity: Orders may be routed to the --- ATS with a minimum quantity
    value specified. --- ATS will only cross where at least this number of shares
    is available from a single eligible contra side order. *)

(* Note that these are order-level constraints. There are members of this  *)
(* structure, like cr_min_qty and cr_round_lot_only that are used in       *)
(* calculating the effective minimum price.                                *)

let get_cat_eligibility (cat_elig, c) =
  match c with
  | C_ONE -> cat_elig.c_one_elig
  | C_TWO -> cat_elig.c_two_elig
  | C_THREE -> cat_elig.c_three_elig
  | C_FOUR -> cat_elig.c_four_elig;;

let can_orders_cross (o1, o2) =
  let o1_cr = o1.cross_restrict in
  let o2_cr = o2.cross_restrict in
  if (o1_cr.cr_self_cross || o2_cr.cr_self_cross) && (o1.client_id = o2.client_id)
  then false
  else if (o1_cr.cr_ubs_principal || o2_cr.cr_ubs_principal) &&
            (o1.client_id = home_id || o2.client_id = home_id) then false
  else if (o1_cr.cr_enable_conditionals || o2_cr.cr_enable_conditionals) &&
            (o1.order_type = LIMIT_CI || o1.order_type = PEGGED_CI) then false
  else true;;

(* *********************************************************************************
   from Msg.ml
   *********************************************************************************
 *)

(* Market best bid and offer *)

type msg_mktdata_update = { mbbo : mkt_data };;

type msg_data_update = { u_order_id : int;
                         new_price : price;
                         new_qty : int };;

type msg_data_cancel = { c_order_id : int };;

(* What goes into a message to create an order *)

type msg_data_create = {
  cr_order_type : order_type;
  cr_order_price : price;
  cr_order_qty : int;
  cr_order_attr : order_attr;
  cr_order_peg : order_peg;
  cr_order_category : category;
  cr_order_expiry_time : int;
};;

type success_data = int;;

type msg_data_success =
    { s_request_time : time;
      s_request_desc : success_data };;

(* Message to halt trading from primary exchange *)

type msg_data_halt = { halt_request_time : time };;

(* Message to pause trading from primary exchange *)

type msg_data_pause = { pause_request_time : time };;

(* Message to invite a firm-up order *)

type msg_data_invite = {
  i_request_time : time;
  i_request_ord_id : int; (* order id of the CI that is responsible for the invite *)
};;

type msg_new_order_ack = {
  ack_time : time;
  ack_ord_id : int;
  ack_qty : int;
  ack_type : order_type;
  ack_price : price;
  ack_side : order_side;
};;

type msg_order_event_ack = {
  a_request_time : time;
  a_request_ord_id : int;
};;

type msg_order_upd_ack = {
  ua_request_time : time;
  ua_request_ord_id : int;
  ua_request_qty : int;
  ua_request_price : price;
};;

(* Message to resume trading from primary exchange *)

type msg_data_resume = { resume_request_time : time };;

(* Message fill data *)

type msg_data_fill = {
  fill_data_buy_id : int;
  fill_data_sell_id : int;
  fill_data_qty : int;
  fill_data_price : price;
  fill_data_time : time;
};;

(* Note: the last 3 messages are for controlling the state of the ATS      *)
(* based on external venues (primary exchanges)                            *)

type msg_body =
  | Msg_Fill of msg_data_fill
  | Msg_Invite of msg_data_invite
  | Msg_Reject
  | Msg_Create of msg_data_create
  | Msg_Update of msg_data_update
  | Msg_Cancel of msg_data_cancel
  | Msg_Success of msg_data_success
  | Msg_MktDataUpdate of msg_mktdata_update
  | Msg_HaltTrading of msg_data_halt
  | Msg_PauseTrading of msg_data_pause
  | Msg_ResumeTrading of msg_data_resume
  | Msg_NewOrderAck of msg_new_order_ack
  | Msg_OrderCancelAck of msg_order_event_ack
  | Msg_OrderUpdateAck of msg_order_upd_ack;;

(* Note: the venue is given src/dest ID -1. So, if msg_src=-1, then the    *)
(* msg is from the server. If msg_dest=-1, then the msg is going to the    *)
(* server. All clients are given IDs i s.t. i>=0.                          *)

type msg = { msg_src : order_source;
             msg_dest : order_source;
             msg_body : msg_body;
             msg_time : time };;

type msg_buffer = msg list;;

let mk_mktdata_update_msg (src, time, nbb, nbo, l_down, l_up) =
  {
    msg_src = src;
    msg_dest = -1;
    msg_body = Msg_MktDataUpdate { mbbo = { nbb = nbb;
                                            nbo = nbo;
                                            l_down = l_down;
                                            l_up = l_up; }};
    msg_time = time
  };;

let mk_invite_msg (cur_time, order) =
  {
    msg_src = -1; (* coming from the exchange *)
    msg_dest = order.src;
    msg_body = ( Msg_Invite { i_request_time = cur_time;
                              i_request_ord_id = order.id; } );
    msg_time = cur_time;
  };;

let mk_create_msg (src, qty, ord_type, price, peg, attr, time) =
  { msg_src = src;
    msg_dest = -1;
    msg_body = (Msg_Create { cr_order_type = ord_type;
                             cr_order_price = price;
                             cr_order_qty = qty;
                             cr_order_attr = attr;
                             cr_order_peg = peg;
                             cr_order_category = C_ONE; (* Need to configure this *)
                             cr_order_expiry_time = time + standard_expiry_time;
                           });
    msg_time = time };;

let mk_modify_msg (src, order_id, price, qty, time) =
  { msg_src = src;
    msg_dest = -1;
    msg_body = (Msg_Update { u_order_id = order_id;
                             new_price = price;
                             new_qty = qty });
    msg_time = time };;

let reply_with_success (cur_time, request_src, request_time, request_desc) =
  { msg_src = -1;
    msg_dest = request_src;
    msg_body = Msg_Success { s_request_time = request_time;
                             s_request_desc = request_desc };
    msg_time = cur_time };;

let reply_with_failure (cur_time, request_src, request_time, request_desc) =
  { msg_src = -1;
    msg_dest = request_src;
    msg_body = Msg_Reject;
    msg_time = cur_time };;

(* ************************************************************************* *)

type fill_price =
  | Known of price
  | Unknown
  | TOP of price;;

(* Note, TOP above is `Theoretical Opening Price,' used in Auction mode.   *)
(* When a TOP price is used in a fill, it is a `proposal' for a TOP. Note, *)
(* such prices only ever appear in a fill_stack while in fill_stack_mode.  *)

type order_book = { buys : order list; sells : order list };;

let empty_book = { buys = []; sells = [] };;

type fill = { order_buy : order;
              order_sell : order;
              fill_qty : int;
              fill_price : fill_price;
              fill_time : time;
            };;

(* Ranking functions for orders
From Section 4.1 "Eligible Resident Orders and IOC Orders are given priority
based first on price and second on the time of their receipt by the ATS.
Eligibility is determined based on the crossing restrictions associated with
the orders on both sides of the potential cross.
Invites are sent to the Order Originators of Conditional Indications on a
priority based first on price, second on the quantity and third on the time of
receipt by ATS. For orders with the same price and time, priority is given
to Resident and IOC Orders over Conditional Indications.
All marketable limit orders (i.e., buy orders with limit prices at or above the
NBO or sell orders with limit prices at or below the NBB) will be treated as
though they are at equivalent prices for priority purposes. As such, they will
be handled based strictly on time priority, as if they were market orders. If a
marketable limit order becomes non - marketable before execution, it will be treated
as a limit order and will receive price / time priority, with time based upon the
original time of receipt of the order by the ATS." *)

let is_ci (ot) = (ot = PEGGED_CI) || (ot = LIMIT_CI);;

let order_higher_ranked (side, o1, o2, mkt) =
  let ot1 = o1.order_type in
  let ot2 = o2.order_type in

  let p_price1 = priority_price (side, o1, mkt) in
  let p_price2 = priority_price (side, o2, mkt) in

  let wins_price = (
    if side = BUY then (
      if p_price1 > p_price2 then 1
      else if p_price2 > p_price1 then -1
      else 0
    )
    else ( 
      if p_price1 < p_price2 then 1
      else if p_price2 < p_price1 then -1
      else 0)
    ) in

  let wins_time = (
    if o1.time < o2.time then 1
    else if o2.time < o1.time then -1
    else 0
  ) in

  let wins_qty = (
    if o1.qty > o2.qty then 1
    else if o2.qty > o1.qty then -1
    else 0
  ) in

  (* Note that the CI priority is price, quantity and then time *)
  if wins_price = 1 then true
  else if wins_price = -1 then false
  else (

    (* Same price level - first check to see whether we're comparing two   *)
    (* CI orders here                                                      *)
    if is_ci(ot1) && is_ci(ot2) then
    (
      (** If we're comparing two CI orders and they have the same price, then
          if they have the same quantity, then time wins, otherwise greatest quantity wins. *)
      if wins_qty = 0 then o1.time < o2.time else
      if wins_qty = 1 then true else false 
    )
    
    (** If we're here, then we're NOT comparing 2 CI orders. *)
    else ( 
      if wins_time = 1 then true
      else if wins_time = -1 then false
      else (
        
        (** Here we check whether one of the orders is CI, it will take priority then. *)
        if is_ci (ot1) then true
        else if is_ci (ot2) then false
        else o1.qty > o2.qty ) 
        (** Notice that we're including this last line (comparing qtys) here because Form ATS is not specific about a tie in price and time for non CI orders.
        We can easily replace it with 'true' or 'false' as it would not affect any transitivity issues. *)
      )
  );;

(* Let's analyse whether this order priority logic makes sense! *)

(** Function 'pretty' further constrains the counterexamples to look 'nice'. *)
let pretty (o1, o2, o3, mkt) = 
  o1.leaves_qty <= o1.qty &&
  o2.leaves_qty <= o2.qty &&
  o3.leaves_qty <= o3.qty &&
  o1.time > 0 &&
  o2.time > 0 &&
  o3.time > 0 &&
  o1.price > 0 &&
  o2.price > 0 &&
  o3.price > 0 &&
  o1.qty > 0 &&
  o2.qty > 0 &&
  o3.qty > 0 &&
  o1.leaves_qty >= 0 &&
  o2.leaves_qty >= 0 &&
  o3.leaves_qty >= 0 &&
  mkt.nbo > mkt.nbb &&
  mkt.l_down > 1000 && 
  mkt.l_down < mkt.nbb &&
  mkt.l_up > mkt.nbo
;;

(*
cx no_time_constraints (side, o1, o2, o3, mkt) =
  (order_higher_ranked(side, o1, o2, mkt) &&
   order_higher_ranked(side, o2, o3, mkt) &&
   pretty(o1, o2, o3, mkt))
  ==>
  (order_higher_ranked(side, o1, o3, mkt));;

(** The second counterexample adds a further constraint that arrival times 
  of the 3 orders are different. *)
cx time_constraints (side, o1, o2, o3, mkt) = 
  (order_higher_ranked(side, o1, o2, mkt) &&
   order_higher_ranked(side, o2, o3, mkt) &&
   pretty(o1, o2, o3, mkt) &&

   (** Make sure all of the times are different *)
   o1.time <> o2.time &&
   o2.time <> o3.time && 
   o1.time <> o3.time )
  ==>
  (order_higher_ranked(side, o1, o3, mkt));;
*)
