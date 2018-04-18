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

  let is_message_valid message state  =
    match message with
    | Reset -> true
    | Add n -> n > 0
    | Sub n -> n > state.counter ;;

  module Template = struct
    type t = Add | Sub | Reset

    let check _ = "Scenario.is_message_valid"

    let concrete = function
      | Add -> ["Add"]
      | Sub -> ["Sub"]
      | Reset -> ["Reset"]

  end

end
;;

Extract.eval ~signature:(Event.DB.fun_id_of_str "Scenario.scenario_sig") () [@@program];;

#program;;

Topfind.load_deeply ["ocamlgraph"];;
Topfind.load_deeply ["containers"];;
Topfind.load_deeply ["containers.iter"];;

System.mod_use "idf.iml";;

module ScenarioMex = struct
  type event = message
  let events_of_region ~signature region =
    List.hd Mex.( (of_region ~signature region).e )
    |> CCLazy_list.return
end;;

module Testgen = Idf.Make(Scenario)(ScenarioMex);;
module G = Testgen.G.Make(struct
                           let guarded_event_hash {Testgen.event;_} = Hashtbl.hash (Pp.pp_message event)
                           let indexed_state_hash {Testgen.state;_} = Hashtbl.hash (Pp.pp_counter_state state)
                           let guarded_event_label e = "event_" ^ (guarded_event_hash e |> string_of_int)
                           let indexed_state_label s = "state_" ^ (indexed_state_hash s |> string_of_int)
                         end);;

#logic;;

type evts  = Scenario.event list;;
type state = Scenario.state;;
let scenario = Scenario.scenario;;

#program;;

let steps , graph =
  Testgen.decompose
    ~template:Scenario.Template.[Add;Sub;Add]
    Scenario.init_state
;;

steps
  |> Testgen.L.reify_all
  |> List.map @@ List.map ( fun (a,e,b) ->
    Testgen.(a.state.counter, e.event, b.state.counter)
  );;

G.output_graph graph "graph.dot";;

