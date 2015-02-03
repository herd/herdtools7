(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Model identifiers *)

type cav12_opt =  { cord : bool ; strongst : bool ; }

type jade_opt = { jstrongst : bool;}

type t =
  | File of string (* To convey model filename *)
  | Minimal of bool    (* true <=> check uniproc *)
  | CAV12 of cav12_opt 
  | Jade of jade_opt
  | X86TSO
  | Generic of AST.pp_t

val tags : string list
val parse : string -> t option
val pp : t -> string

(* What to let through *)

type through =
  | ThroughAll       (* Do not retain anything *)
  | ThroughInvalid   (* Let invalid go through (ie retain uniproc violations) *)
  | ThroughNone      (* Standard behaviour *)

val tags_through : string list
val parse_through : string -> through option
val pp_through : through -> string

(* Common configuration *)
module type Config = sig
  val through : through
  val debug : bool
  val verbose : int
  val skipchecks : StringSet.t
  val strictskip : bool
  val optace : bool
end

(* Defaults *)
val get_default_model : Archs.t -> t
