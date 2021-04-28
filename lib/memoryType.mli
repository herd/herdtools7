(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Memory types of global variables *)
module type S = sig
  type t
  val default : t (* Default value *)
  val pp : t -> string (* Pretty print *)
  val emit : t -> string (* Emit in code *)
  val fold : (t -> 'a -> 'a) -> 'a -> 'a (* Fold over all values of t *)
  val equal : t -> t -> bool

  (* Parse info, notice that default memory type is discarded. *)
  val parse : MiscParser.info -> t Misc.Simple.bds
  (* Cache flush needed after initialisation. *)
  val need_flush : t Misc.Simple.bds -> bool
end

module X86_64 : S

module No : S
