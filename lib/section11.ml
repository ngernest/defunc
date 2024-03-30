(** 1.1 A sample higher-order program with a static number of closures *)

let aux (f : int -> int) : int = f 1 + f 10

(** Calls [aux] twice & multiplies the results *)
let main ((x, y, b) : int * int * bool) : int =
  aux (fun z -> x + z) * aux (fun z -> if b then y + z else y - z)

(** To defunctionalize a program: 
    1. Define a new datatype (eg [lam] below), and 
      create a constructor for each lambda, with each free variable in the 
      lambda corresponding to a constructor argument. 
    2. Define the associated [apply] function below which eliminates 
       values of the new datatype. *)
type lam =
  | Lam1 of int (* 1 free variable [x : int] *)
  | Lam2 of int * bool (* 2 free variables [y : int], [b : bool] *)

let apply : lam * int -> int = function
  | Lam1 x, z -> x + z
  | Lam2 (y, b), z -> if b then y + z else y - z

(** Defunctionalized version of [aux] *)
let aux_def (f : lam) : int = apply (f, 1) + apply (f, 10)

(** Defunctionalized version of [main] *)
let main_def ((x, y, b) : int * int * bool) : int =
  aux_def (Lam1 x) * aux_def (Lam2 (y, b))
