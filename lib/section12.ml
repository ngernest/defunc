(** 1.2 A sample higher-order program with a dynamic number of closures *)

let aux ((i, f) : int * (int -> int)) : int = f i

(** [main i js] adds [i] to each element of [js] 
    - Note that the lambda [fun i -> i + j] 
    is instantiated [n] times, where [n = length js] *)
let main ((i, js) : int * int list) : int list =
  let rec walk : int list -> int list = function
    | [] -> []
    | j :: js -> aux (i, fun i -> i + j) :: walk js in
  walk js

(** We only have one lambda, so there is only one constructor *)
type lam = Lam of int

(** Eliminates values of the [lam] datatype *)
let apply ((Lam j, i) : lam * int) : int = i + j

(** Passes the [lam] to [apply], which eliminates 
    it by dispatching over the constructor using a pattern match *)
let aux_def ((i, f) : int * lam) : int = apply (f, i)

(** Note that the lambda is introduced using the [Lam] constructor *)
let main_def ((i, js) : int * int list) : int list =
  let rec walk : int list -> int list = function
    | [] -> []
    | j :: js -> aux_def (i, Lam j) :: walk js in
  walk js
