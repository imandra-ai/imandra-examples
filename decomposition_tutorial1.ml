(**

  Aesthetic Integration Limited
  Copyright (c) 2014 - 2018. All rights reserved.

  Visit https://www.imandra.ai for further information.

*)

type message = Add of int | Sub of int | Reset

type counter_state = {
  counter  : int;
  incoming : message option;
};;

let one_step state =
  match state.incoming with None -> state | Some msg ->
  let state = { state with incoming = None } in
  if state.counter = 1337 then state else
  match msg with
  | Reset -> { state with counter = 0 }
  | Add n -> begin
    if n + state.counter > 9000 then
      { state with counter = 0 }
    else
      { state with counter = state.counter + n }
  end
  | Sub n -> begin
    if n > state.counter then state else
    { state with counter = state.counter - n }
  end
;;

(***************** Decomposiong one_step *******************)
#install_printer Decompose.print;;
let regions = Decompose.by_simp_ctx "one_step" [@@program];;

(* Sampling the regions *)
Extract.eval ~signature:(Event.DB.fun_id_of_str "one_step") () [@@program];;
regions |> List.map (fun r -> Mex.((of_region r).state));;


(*************** Nested decomposition ****************)
let init_state = { counter = 0 ; incoming = None } ;;

let scenario e1 e2 e3 =
  let step e s = one_step { s with incoming = Some e } in
    init_state |> step e1 |> step e2 |> step e3 ;;

Extract.eval ~signature:(Event.DB.fun_id_of_str "scenario") () [@@program];;

let regions_3steps_full = Decompose.by_simp_ctx "scenario" [@@program];;

Caml.List.nth regions_3steps_full 11i;;

(* Adding constraints  *)
let is_message_valid state message = 
  match message with
  | Reset -> true
  | Add n -> n > 0 
  | Sub n -> n > state.counter ;;
;; 

let messages_valid e1 e2 e3 = 
  let step e s = one_step { s with incoming = Some e } in
  let s1 = init_state in
  let s2 = step e1 s1 in
  let s3 = step e2 s2 in
     is_message_valid s1 e1 
  && is_message_valid s2 e2 
  && is_message_valid s3 e3;; 

let regions_3steps_asm = Decompose.by_simp_ctx
    ~assuming:"messages_valid" "scenario" [@@program];;

(* Using a template *)
let template e1 e2 e3  =
  let step e s = one_step { s with incoming = Some e } in
  match (e1,e2,e3) with
  | Add _ , Sub _ , Add _ ->
    let s1 = init_state in
    let s2 = step e1 s1 in
    let s3 = step e2 s2 in
       is_message_valid s1 e1
    && is_message_valid s2 e2
    && is_message_valid s3 e3
  | _ -> false
;;

let template_regions = Decompose.by_simp_ctx
    ~assuming:"template" "scenario" [@@program];;

template_regions |> List.map Mex.of_region;;


