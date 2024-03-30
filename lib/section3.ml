(** Section 3 *)
open Core

exception Not

(** A recognizaer for the language {0^n 1^n} *)
let recognizer0 (xs : int list) : bool = 
  (** Traverses [xs]: 
      - Whenever we encounter a 0, [walk] calls itself recursively 
      - Whenever it meets a non-zero value, it returns the rest of the list, 
        and expects to see 1s as the remaining elements
      - In the event of a mismatch, an exception is raised *)
  let rec walk : int list -> int list = function
    | 0 :: xs' -> 
      (match walk xs' with 
      | 1 :: xss -> xss 
      | _ -> raise Not)
    | ys -> ys in 
  try List.is_empty (walk xs) with 
    Not -> false

(** CPS version of [recognizer0] *)    
let rec recognizer1 (xs : int list) : bool = 
  (** Traverses [xs] tail-recursively using a continuation [k] 
      - If it meets a non-zero value, it passes the current list to [k] 
      - If it encounters 0, it walks down the list with a new continuation 
      - If the new continuation is given a list beginning with 1, 
        it sends the tail of that list to [k], otherwise it returns [false] *)
  let rec walk (ys : int list) (k : int list -> bool) : bool = 
    match ys with 
    | 0 :: xs' -> 
      walk xs 
        (fun zs -> match zs with 
          | 1 :: zss -> k zss 
          | _ -> false)
    | _ -> k ys in 
  walk xs List.is_empty

(** [Cont0] is the initial continuation, 
    [Cont1] is for intermediate continuations *)
type cont = 
  | Cont0     
  | Cont1 of cont

(** The apply function for [cont] *)  
let rec apply (cont : cont) (xs : int list) : bool = 
  match cont with 
  | Cont0 -> List.is_empty xs 
  | Cont1 k -> 
    (match xs with 
    | 1 :: ys -> apply k ys 
    | _ -> false) 
      
let rec recognizer2 (xs : int list) : bool = 
  let rec walk (ys : int list) (k : cont) : bool = 
    match ys with 
    | 0 :: zs -> walk zs (Cont1 k)
    | _ -> apply k ys in 
  walk xs Cont0
     
(**
- We've implemented a push-down automaton that has two states and one element 
  in the stack alphabet. 
- The two states are represented by the functions [walk] and [apply]
- The stack is implemented using the [cont] datatype
- The transitions are the tail recursive calls 
- This automaton accepts an input if processing the input 
*)  