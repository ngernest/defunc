open Core 

(** Arithmetic expressions: values & computations *)
type aexp = 
  | Val of int      
  | Comp of comp    

(** Computations: additions + conditionals *)
and comp = 
  | Add of aexp * aexp 
  | Ifz of aexp * aexp * aexp

(** Evaluation contexts *)  
type ectx = 
  | Empty 
  | Add1 of ectx * aexp          (* E[[] + e] *)
  | Add2 of ectx * int           (* E[v + []] *)  
  | Ifz0 of ectx * aexp * aexp   (* E[ifz [] e e] *)

(** [plug ectx e] puts the expression [e] in the evaluation context [ectx] 
    - Note that [plug] is the apply function for [ectx]! *)    
let rec plug (ectx : ectx) (e : aexp) : aexp = 
  match ectx with 
  | Empty -> e 

  (* E[[] + e'][e] = E[e + e'] *)
  | Add1 (ec, e2) -> plug ec @@ Comp (Add (e, e2))   
  
  (* E[v + []][e] = E[v + e] *)
  | Add2 (ec, v) -> plug ec @@ Comp (Add (Val v, e))

  (* E[ifz [] e1 e2][e] = E[ifz e e1 e2] *)
  | Ifz0 (ec, e1, e2) -> plug ec @@ Comp (Ifz (e, e1, e2))

(** Single-step reduction: A computation undergoes a reduction step when:
    1. it is decomposed into a redex & its context
    2. the redex is contracted
    3. the result is plguged into the context *)  
let rec reduce1 (comp : comp) (ectx : ectx) : aexp = 
  match comp with 
  | Add (Val v1, Val v2) -> plug ectx @@ Val (v1 + v2)
  | Add (Val v1, Comp s2) -> reduce1 s2 @@ Add2 (ectx, v1)
  | Add (Comp s1, e2) -> reduce1 s1 @@ Add1 (ectx, e2)
  
  (* E[ifz 0 e1 e2] -> E[e1] *)
  | Ifz (Val 0, e1, e2) -> plug ectx e1 
  
  (* E[ifz v e1 e2] -> E[e2] if v != 0 *)
  | Ifz (Val v, e1, e2) -> plug ectx e2 

  | Ifz (Comp s0, e1, e2) -> reduce1 s0 @@ Ifz0 (ectx, e1, e2)

(** Evaluation entails repeatedly reduce until we reach a value *)  
let rec eval (e : aexp) : int =
  match e with
  | Val v -> v 
  | Comp s -> eval (reduce1 s Empty)
  
(** Refunctionalized version of [reduce1] *)
let rec reduce1' (comp : comp) (ec : aexp -> int) : int = 
  match comp with 
  | Add (Val v1, Val v2) -> ec @@ Val (v1 + v2)
  | Add (Val v1, Comp s2) -> 
    reduce1' s2 (fun e2 -> ec @@ Comp (Add (Val v1, e2)))
  | Add (Comp s1, e2) -> 
    reduce1' s1 (fun e1 -> ec @@ Comp (Add (e1, e2)))
  | Ifz (Val 0, e1, e2) -> ec e1 
  | Ifz (Val v, e1, e2) -> ec e2 
  | Ifz (Comp s0, e1, e2) -> 
    reduce1' s0 (fun e0 -> ec @@ Comp (Ifz (e0, e1, e2)))

