(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Model identifiers *)

type cav12_opt =  { cord : bool ; strongst : bool ; }

type jade_opt = { jstrongst : bool;}

type t =
  | File of string (* To convey model filename *)
  | CAV12 of cav12_opt
  | Generic of string * AST.t (* Filename X ast *)

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
  val showsome : bool
  val through : through
  val debug : bool
  val debug_files : bool
  val profile : bool
  val verbose : int
  val skipchecks : StringSet.t
  val strictskip : bool
  val cycles : StringSet.t
  val optace : OptAce.t
  val libfind : string -> string
  val variant : Variant.t -> bool
  val dirty : DirtyBit.t option
end

(* Defaults *)
val get_default_model : (Variant.t -> bool) -> Archs.t -> t
