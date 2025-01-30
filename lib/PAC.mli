
(* Symbolic implementation of `FEAT_Pauth2` and `FEAT_CONSTPACFIELD`
 * Also add a type of solver state to reason about hash collisions
 *)

(* Symbolic type for Pointer Authentication Codes *)
type t

(* Generate a canonical PAC field *)
val canonical : t

(* Check if a virtual address is canonical *)
val is_canonical : t -> bool

(* Symbolically add a new pac field to a pointer with an exclusive OR using
 * the key, the modifier and the current offset of the pointer.
 * If `x` is a canonical virtual address and `p` is a PAC field:
 *     `add modifier key offset p = pac(x+offset, key, modifier) eor p`
 *)
val add : string -> string -> int -> t -> t

val pp : t -> string -> string

val compare : t -> t -> int


val equal : t -> t -> bool
