(** 2.2 Higher-order representations of lists *)

(** Hughes' insight: represent intermediate lists as partialy applied 
    concatenation functions (aka difference lists):
    - Instead of constructing a list [xs], instantiate the lambda 
      [fun ys -> xs @ ys]
    - This allows lists to be concatenated in constant time! *)

open Core

(** Alias for [@] *)
let append xs ys = xs @ ys

(** Reverses a list in linear (as opposed to quadratic time), 
    taking advantage of the higher-order representation of lists *)
let reverse (xs : 'a list) : 'a list =
  let rec walk (ys : 'a list) : 'a list -> 'a list =
    match ys with
    | [] -> append []
    | x :: xs -> Fn.compose (walk xs) (append [ x ]) in
  walk xs []

let cons x xs = x :: xs

(** Variant of [reverse], with the following rewrite rules applied :
    - Appending the empty list is the same as the identity function
    - Appending a single element is the same as [cons]-ing it *)
let reverse' (xs : 'a list) : 'a list =
  let rec walk (ys : 'a list) : 'a list -> 'a list =
    match ys with
    | [] -> Fn.id
    | x :: xs -> Fn.compose (walk xs) (cons x) in
  walk xs []

(** Three constructors, each corresponding to a lambda in [reverse'] above
    - [Lam0] corresponds to [Fn.id]
    - [Lam1] corresponds to [cons x] 
    - [Lam2] corresponds to function composition 
    - "In Hughes's monoid of intermediate lists, 
      concatenation is done in constant time via [Lam2]" *)
type 'a lam = Lam0 | Lam1 of 'a | Lam2 of 'a lam * 'a lam

let rec apply ((lam, ys) : 'a lam * 'a list) : 'a list =
  match lam with
  | Lam0 -> ys
  | Lam1 x -> x :: ys
  | Lam2 (f, g) -> apply (f, apply (g, ys))

let id_def = Lam0
let cons_def x = Lam1 x
let o_def (f, g) = Lam2 (f, g)

(** Defunctionalized version of [reverse'] *)
let reverse_def (xs : 'a list) : 'a list =
  let rec walk (ys : 'a list) : 'a lam =
    match ys with
    | [] -> id_def
    | x :: xs -> o_def (walk xs, cons_def x) in
  apply (walk xs, [])

(** Since [Lam1] & [Lam2] are always used together,
    we can fuse them into a single constructor [Lam3] *)
type 'a lam_alt = Lam0 | Lam3 of 'a lam_alt * 'a

(** [apply] function for ['a lam_alt] 
    - The paper calls [apply_lam_alt] "an uncurried version of the fast 
    reverse function with an accumulator" *)
let rec apply_lam_alt ((lam, ys) : 'a lam_alt * 'a list) : 'a list =
  match lam with
  | Lam0 -> ys
  | Lam3 (f, x) -> apply_lam_alt (f, x :: ys)

(** Embeds [xs] homomorphically into the datatype ['a lam_alt] *)
let reverse_def_alt (xs : 'a list) : 'a list =
  let rec walk (ys : 'a list) : 'a lam_alt =
    match ys with
    | [] -> Lam0
    | x :: xs -> Lam3 (walk xs, x) in
  apply_lam_alt (walk xs, [])
