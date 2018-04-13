type currency = GBP of float | USD of float | EUR of float;;

let f x =
  match x with
    Some n -> n + 1
  | None -> 0;;

let foo (x,y) =
  match x, y with
    Some n, 100 ->
    n + 25
  | Some n, m ->
     76
  | _ -> 25;;

let bar (x,y,z) =
  match x,y,z with
    Some n, Some m, Some k ->
    n + m + k - 2
  | None, Some n, Some n' ->
     if n = 2*n' + 28
     then 956
     else foo (Some (43*n), n')
  | _ -> 99;;

verify _ (x,y) = foo(x,y) <> 76;;

verify _ (x,y) = foo(x,y) <> 97;;

verify _ (x,y,z) = bar(x,y,z) <> 101883;;

let g x =
  if x > 0 then Some x else None;;

verify _ x = g x <> None;;

let c_add (x,y) =
  match x,y with
    GBP n, GBP m -> Some (GBP (n +. m))
  | USD n, USD m -> Some (USD (n +. m))
  | EUR n, EUR m -> Some (EUR (n +. m))
  | _ -> None;;

let same_currency (x,y) =
  match x,y with
    USD _, USD _ -> true
  | GBP _, GBP _ -> true
  | _ -> false;;

verify c_add_safe (x,y) =
  (same_currency(x,y))
    ==>
  (c_add(x,y) <> None);;

verify _ (x,y) = same_currency(x,y);;

(* Needs disambiguation on polymorphic ADT constructors *)

verify _ (x,y) =
  (same_currency(x,y))
    ==>
  (c_add(x,y) <> None);;

verify _ (x,y) =
  (same_currency(x,y))
    ==>
  (c_add(x,y) <> Some (GBP 169.0));;

verify _ (x,y) =
  (same_currency(x,y) &&
     match x,y with
       GBP n, GBP m -> n>0. && m>0.
     | USD n, USD m -> n>0. && m>0.)
    ==>
  (c_add(x,y) <> Some (GBP 169.0));;