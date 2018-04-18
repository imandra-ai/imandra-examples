#program;;

Topfind.load_deeply ["yojson"];;
System.mod_use "../../imandra_tools/regions_json/rjson.ml";;

let pp_region r = 
  let open Decompose in
  let c2s c = Expr_trans.to_term ~strict:false c |> Term.to_string in
  (r.constraints |> List.map c2s) @ [c2s r.invariant]
;;

let regions_to_file fname regions = 
  Rjson.regions_to_json pp_region regions 
  |> Yojson.Basic.to_file fname  
;;

regions_to_file "temp.json" regions;;



