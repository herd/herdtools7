(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type instr = {
  instruction : AArch64Base.instruction;
  proc : int;
  static_poi : int;
  label : Label.t option;
}

val render_instruction : latex:bool -> instr -> string
val equal_proc_poi : instr -> instr -> bool
val compare_proc_poi : instr -> instr -> int

val from_string : string -> AArch64Base.pseudo MiscParser.t
(** Parse a AArch64 litmus test from a string.

    @raise Invalid_argument
      on inputs targeting an architecture that is not AArch64. *)

val collect_instructions : AArch64Base.pseudo MiscParser.t -> instr list
