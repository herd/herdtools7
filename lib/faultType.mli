(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  type t

  val sets : (string * t list) list

  val pp : t -> string (* Pretty print *)
  val parse : MiscParser.fault_type -> t
  val compare : t -> t -> int
end

module type AArch64Sig = sig
  type mmu_t =
    | Translation (* valid: 0 *)
    | AccessFlag  (* af: 0 *)
    | Permission  (* db: 0 *)
    | Exclusive   (* memattr <> sharedWB *)

  type t =
    | MMU of mmu_t
    | TagCheck
    | UndefinedInstruction
    | SupervisorCall
    | PacCheck of PAC.key

  include S with type t := t
end

module AArch64 : AArch64Sig

module No : S

(* For parse disambiguation  *)
val is : string -> bool
