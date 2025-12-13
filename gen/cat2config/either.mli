(* This whole module is purely for compatibility with OCaml <= 4.12. *)

type ('a, 'b) t = Left of 'a | Right of 'b

val find_left : ('a, 'b) t -> 'a option
val find_right : ('a, 'b) t -> 'b option
