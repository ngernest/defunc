(** 2.3 Defunctionalizing Church-encoded non-recursive data structures *)

(** A Church pair is a lambda that expects an argument that is a selector 
    that projects the appropriate element *)
let church_pair (x1, x2) s = s (x1, x2)

let church_fst p = p (fun (x1, x2) -> x1)
let church_snd p = p (fun (x1, x2) -> x2)

(** Selectors are closed terms, i.e. the corresponding constructors have 
    arity 0 *)
type sel = Fst | Snd

let apply_sel ((sel, (x1, x2)) : sel * ('a * 'a)) : 'a =
  match sel with
  | Fst -> x1
  | Snd -> x2

type 'a pair = Pair of 'a * 'a

let apply_pair ((Pair (x1, x2), s) : 'a pair * sel) : 'a =
  apply_sel (s, (x1, x2))
