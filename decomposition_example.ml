(**

  Aesthetic Integration Limited
  Copyright (c) 2014 - 2018. All rights reserved.

  Visit https://www.imandra.ai for further information.

*)


(* A simple ImandraML model, illustrating decomposition regions   *)
let f x y =
  match x with
    Some n ->
    if n > 20 then
      n + y + 2
    else if n < -5 then
      88
    else if n > 10 then
      99
    else
      100
  | None ->
     if y > 50 then
       11
     else
       99
;;

#install_printer Decompose.print;;

(* Decomosiong the function *)
let regions = Decompose.by_simp_ctx "f" [@@program];;

(* Now, we'll add a simple side condition. *)
let side_cond_1 (x : int option) (y : int) = (x = None);;

let conditioned_regions = Decompose.by_simp_ctx
    ~assuming:"side_cond_1" "f" [@@program];;

