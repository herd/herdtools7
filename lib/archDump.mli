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

(** Arch definition for dumper, very minimal *)

module type S = sig

  (* Who am I ? *)
  val arch : Archs.t

  (***********************************************)
  (* Basic arch types and their basic operations *)
  (***********************************************)

  type reg

  val pp_reg : reg -> string

  type parsedInstruction
  type instruction

  val dump_instruction : instruction -> string
  val dump_instruction_hash : instruction -> string

  include Pseudo.Types
   with type ins = instruction
   and type pins = parsedInstruction
   and type reg_arg = reg

end
