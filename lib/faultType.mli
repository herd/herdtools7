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

module type S = sig
  type t

  val sets : (string * t list) list

  val pp : t -> string (* Pretty print *)
end

module type AArch64Sig = sig
  type mmu_t =
    | Translation (* valid: 0 *)
    | AccessFlag  (* af: 0 *)
    | Permission  (* db: 0 *)

  type t =
    | MMU of mmu_t
    | TagCheck

  include S with type t := t
end

module AArch64 : AArch64Sig

module No : S
