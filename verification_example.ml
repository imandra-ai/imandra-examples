(**

  Aesthetic Integration Limited
  Copyright (c) 2014 - 2018. All rights reserved.

  Visit https://www.imandra.ai for further information.

*)

type currency = GBP of float | USD of float | EUR of float;;

let f x =
  match x with
    Some n -> n + 1
  | None -> 0;;

let foo  x y  =
  match x, y with
    Some n, 100 ->
    n + 25
  | Some n, m ->
     76
  | _ -> 25;;

(* Trying to verify a statement about foo *)
let verify_foo1 x y = (foo x y) <> 76;;
Verify.top "verify_foo1";;

(* Counterexample values  can be obtained from CX module: *)
( CX.x , CX.y ) ;;

(* Another statement about foo *)
let verify_foo2 x y = (foo x y) <> 97;;
Verify.top "verify_foo2";;



let bar x y z  =
  match x,y,z with
    Some n, Some m, Some k ->
    n + m + k - 2
  | None, Some n, Some n' ->
     if n = 2*n' + 28
     then 956
     else foo (Some (43*n))  n' 
  | _ -> 99;;

let verify_bar x y z  = ( bar x y z  <> 101883 );;

Verify.top "verify_bar";;


let g x =
  if x > 0 then Some x else None;;

let verify_g x = g x <> None;;

Verify.top "verify_g";;


let c_add  x y  =
  match x,y with
    GBP n, GBP m -> Some (GBP (n +. m))
  | USD n, USD m -> Some (USD (n +. m))
  | EUR n, EUR m -> Some (EUR (n +. m))
  | _ -> None;;

let same_currency  x y  =
  match x,y with
    USD _, USD _ -> true
  | GBP _, GBP _ -> true
  | _ -> false;;

let verify_c_add_safe  x y  =
  (same_currency x y )
    ==>
  (c_add x y  <> None);;
Verify.top "verify_c_add_safe";;

let verify_same_currency x y = same_currency x y ;;
Verify.top "verify_same_currency";;


(* Check that if same currency, then addition result is not Note *)
let verify_same_currency_adds x y =
  (same_currency x y )
    ==>
  (c_add x y  <> None);;
Verify.top "verify_same_currency_adds";;

let verify_add_result x y  =
  (same_currency x y )
    ==>
  (c_add x y  <> Some (GBP 169.0));;
Verify.top "verify_add_result";;

let verify_add_positive x y  =
  (same_currency x y  &&
     match x,y with
     | GBP n, GBP m -> n>0. && m>0.
     | USD n, USD m -> n>0. && m>0.
     | _ -> false)
    ==>
  (c_add x y  <> Some (GBP 169.0));;
Verify.top "verify_add_positive";;
