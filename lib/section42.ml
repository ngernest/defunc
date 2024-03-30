(** 4.2 The CBV lambda-calculus *)

type term = 
  | Val of value 
  | Comp of comp 
and value = 
  | Var of string 
  | Lam of string * term 
and comp = App of term * term 

(** Evaluation contexts*)
type ectx = 
  | Empty 
  | App1 of ectx * term 
  | App2 of ectx * value

let subst term value str : term = 
  failwith "Substitution function not in paper"

let rec plug (ectx : ectx) (e : term) : term = 
  match ectx with 
  | Empty -> e 
  | App1 (ec, e') -> plug ectx @@ Comp (App (e, e'))
  | App2 (x, v) -> plug ectx @@ Comp (App (Val v, e))

(** Single-step reduction *)  
let rec reduce1 (comp : comp) (ectx : ectx) : term option = 
  match comp with 
  | App (Val (Lam (x, e)), Val v) -> Some (plug ectx @@ subst e v x)
  | App (Val (Var x), Val v) -> None
  | App (Val v, Comp s) -> reduce1 s @@ App2 (ectx, v)
  | App (Comp s, e) -> reduce1 s @@ App1 (ectx, e)

(** Evaluation is multi-step reduction *)  
let rec eval (tm : term) : value option = 
  match tm with 
  | Val v -> Some v 
  | Comp s -> 
    match reduce1 s Empty with 
    | Some e -> eval e 
    | None -> None

(** 
    
- Defcuntionalize the CPS counterpart of a recursive descent over the BNF 
  (e.g. a single-step reduction function)

*)    