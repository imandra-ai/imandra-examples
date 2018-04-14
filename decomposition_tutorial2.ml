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

Genpp.eval ();;

#install_printer Decompose.print;;

module Scenario = struct
  type state = counter_state
  type event = message

  let guard_state  = id 

  let to_decompose = "Scenario.scenario"
  let global_basis = []
  let pp_state     = Pp.pp_counter_state 

  let init_state = { counter = 0 ; incoming = None } 
  let step event state =
    one_step { state with incoming = Some event }
 
  let scenario (e : event list) (state : state) : state =
    match e with e :: _ -> step e state | [] -> state 
     
  let scenario_sig (e) = scenario e init_state 
end
;;

Extract.eval ~signature:(Event.DB.fun_id_of_str "Scenario.scenario_sig") () [@@program];;

#program;;

module ScenarioMex = struct
  type event = message
  let event_of_region ~signature region =
     List.hd Mex.( (of_region ~signature region).e ) 
end;;

#logic;;

#program;;

Topfind.load_deeply ["ocamlgraph"];;
Topfind.load_deeply ["containers"];;  
Topfind.load_deeply ["containers.iter"];;

System.mod_use "idf.ml";;

module Testgen = Idf.Make(Scenario)(ScenarioMex);;

#logic;;

let is_message_valid state message = 
  match message with
  | Reset -> true
  | Add n -> n > 0 
  | Sub n -> n > state.counter ;;

type evts  = Scenario.event list;;
type state = Scenario.state;;
let scenario = Scenario.scenario;;

let template2 (e : evts) (state : state) (i : int)  : bool =
    match e with
    | Add n2 :: [] -> 
    ( is_message_valid state ( Add n2 ) )
    | _ -> false

let template1 (e : evts) (state : state) (i : int)  : bool =
    match e with
    | Sub n1 :: Add n2 :: [] ->
    ( is_message_valid state ( Sub n1 ) 
      && ( (i = 0) || (template2 ( Add n2 :: [] ) (scenario e state) (i - 1)) )
    )
    | _ -> false

let template0 (e : evts) (state : state) (i : int)  : bool =
    match e with
    |  Add n0 :: Sub n1 :: Add n2 :: [] -> 
    ( is_message_valid state ( Add n0 ) 
      && ( (i = 0) || (template1 ( Sub n1 :: Add n2 :: [] ) (scenario e state) (i - 1)) )
	)
    | _ -> false
;;

#program;;

let steps , graph =
  Testgen.decompose 
    ~asm_of_step:["template0"; "template1"; "template2"]
    Scenario.init_state
;;

steps 
  |> Testgen.L.reify_all
  |> List.map @@ List.map ( fun (a,e,b) -> 
    Testgen.(a.state.counter, e.event, b.state.counter)
  );;
