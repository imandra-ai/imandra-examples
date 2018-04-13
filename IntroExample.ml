(**

  Aesthetic Integration Limited
  Copyright (c) 2014 - 2018. All rights reserved.

  Visit https://www.imandra.ai for further information.

*)


let f (x,y) =
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

:decompose f

(* Now, we'll add a simple side condition. *)

let side_cond_1 (x,y : int option * _) =
  x = None;;

:decompose f assuming side_cond_1
