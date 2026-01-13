(* The Yojson library provides several types for representing JSON values,
   with different use cases.
   Each of these different types have their own module (Basic, Safe, Raw).
   This file re-exports one of these modules so that we don't have to commit
   to any particular representation, or to Yojson itself.
*)

include module type of Yojson.Basic

(* Constructors as functions *)
val string : string -> t
val int : int -> t
val assoc : (string * t) list -> t

(* Helpers for rendering records and variants. *)
val ctor : label:string -> (string * t) list -> t
