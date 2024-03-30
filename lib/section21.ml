(** 2.1 Flattening a binary tree into a list *)

open Core

(** A datatype for binary trees *)
type 'a bt = Leaf of 'a | Node of 'a bt * 'a bt

let cons x xs = x :: xs

(** Flattens a binary tree into a list of its leaves 
    - [Leaf] maps homomorphically to a curried list constructor 
    - [Node]'s maps homomorphically to function composition 
    i.e. "we map a list into the monoid of functions from lists to lists" *)
let flatten (t : 'a bt) : 'a list =
  let rec walk (tree : 'a bt) : 'a list -> 'a list =
    match tree with
    | Leaf x -> cons x
    | Node (t1, t2) -> Fn.compose (walk t1) (walk t2) in
  walk t []

(** Eta-expanded version of [flatten], with [cons] & [Fn.compose] inlined 
    - The paper calls this "a curried version of the fast flatten function 
    with an accumulator" *)
let flatten_ee (t : 'a bt) : 'a list =
  let rec walk (tree : 'a bt) (xs : 'a list) : 'a list =
    match tree with
    | Leaf x -> x :: xs
    | Node (t1, t2) -> walk t1 (walk t2 xs) in
  walk t []

(** Auxiliary datatype representing defunctionalized lambdas 
    - ['a lam] is isomorphic to ['a bt] *)
type 'a lam = Lam1 of 'a | Lam2 of 'a lam * 'a lam

let rec apply ((lam, xs) : 'a lam * 'a list) : 'a list =
  match lam with
  | Lam1 x -> x :: xs
  | Lam2 (f1, f2) -> apply (f1, apply (f2, xs))

let cons_def x = Lam1 x

(** Defunctionalized version of function composition *)
let o_def (f1, f2) = Lam2 (f1, f2)

(** Defunctionalized version of [flatten] *)
let flatten_def (t : 'a bt) : 'a list =
  let rec walk (tree : 'a bt) : 'a lam =
    match tree with
    | Leaf x -> cons_def x
    | Node (t1, t2) -> o_def (walk t1, walk t2) in
  apply (walk t, [])
