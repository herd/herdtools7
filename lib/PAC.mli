(* Symbolic implementation of `FEAT_Pauth2` and `FEAT_CONSTPACFIELD`
 * Also add a type of solver state to reason about hash collisions
 *)

(* All the supported Pointer Authentication Code keys *)
type key = DA | DB | IA | IB

(* Parse a PAC key and raise an error if the input is invalid *)
val parse_key : string -> key

(* Pretty print a PAC key in lower case *)
val pp_lower_key : key -> string

(* Pretty print a PAC key in upper case *)
val pp_upper_key : key -> string

(* Symbolic type for Pointer Authentication Codes *)
type t

(* pretty print a PAC field, but only the PAC field, as example
 * `pp (add "x" DA "42" 7 empty) "x+4" := "pacda(x+4,42,7)"`
 *)
val pp : t -> string -> int -> string

(* Ordering relation over PAC fields *)
val compare : t -> t -> int

(* Equality relation over PAC fields *)
val equal : t -> t -> bool

(* Generate a canonical PAC field *)
val canonical : t

(* Check if a virtual address is canonical *)
val is_canonical : t -> bool

(* Symbolically add a new pac field to a pointer with an exclusive OR using
 * the key, the modifier and the current offset of the pointer.
 * If `x` is a canonical virtual address and `p` is a PAC field:
 *     `add name key modifier offset p = pac(x+offset, key, modifier) eor p`
 *)
val add : string -> key -> string -> int -> t -> t

(* Return a pac field representing an error core in case of an authentication
 * failure using `FEAT_PAuth` without `FEAT_PAuth2` *)
val error : string -> key -> t

(* A type of solver to reason about equality constraints on the PAC fields of
 * virtual addresses*)
type solver_state

val compare_solver_state : solver_state -> solver_state -> int

val pp_solver : solver_state -> string

(* A solver state without constraints *)
val empty_solver : solver_state

(* Add an equality constraint to the solver state, return None if this
 * equality introduce a contradiction *)
val add_equality : t -> t -> solver_state -> solver_state option

(* Add an inequality constraint to the solver state, return None if this
 * inequality introduce a contradiction *)
val add_inequality : t -> t -> solver_state -> solver_state option

val conjunction : solver_state -> solver_state -> solver_state option

(* Normalize a PAC field in the current solver state *)
val normalize : t -> solver_state -> t
