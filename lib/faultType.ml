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

  val pp : t -> string
  val parse : MiscParser.fault_type -> t
  val compare : t -> t -> int
end

module type AArch64Sig = sig
  type mmu_t =
    | Translation
    | AccessFlag
    | Permission

  type key_t = DA | DB | IA | IB

  type t =
    | MMU of mmu_t
    | TagCheck
    | UndefinedInstruction
    | PacCheck of key_t

  include S with type t := t
end

module AArch64 = struct
  type mmu_t =
    | Translation
    | AccessFlag
    | Permission

  let pp_mmu_t = function
    | Translation -> "Translation"
    | AccessFlag -> "AccessFlag"
    | Permission -> "Permission"

  type key_t = DA | DB | IA | IB

  let pp_key_t = function
    | DA -> "DA"
    | DB -> "DB"
    | IA -> "IA"
    | IB -> "IB"

  type t =
    | MMU of mmu_t
    | TagCheck
    | UndefinedInstruction
    | PacCheck of key_t

  let sets = [
      "MMU", [MMU Translation;
              MMU AccessFlag;
              MMU Permission];
      "Translation", [MMU Translation];
      "AccessFlag", [MMU AccessFlag];
      "Permission", [MMU Permission];
      "TagCheck", [TagCheck];
      "PacCheck", [PacCheck DA;
                   PacCheck DB;
                   PacCheck IA;
                   PacCheck IB];
      "UndefinedInstruction",[UndefinedInstruction];
    ]

  let pp = function
    | MMU m -> Printf.sprintf "MMU:%s" (pp_mmu_t m)
    | TagCheck -> "TagCheck"
    | UndefinedInstruction -> "UndefinedInstruction"
    | PacCheck k -> Printf.sprintf "PacCheck:%s" (pp_key_t k)

  let parse = function
    | "MMU:Translation" -> MMU Translation
    | "MMU:AccessFlag" -> MMU AccessFlag
    | "MMU:Permission" -> MMU Permission
    | "TagCheck" -> TagCheck
    | "PacCheck:DA" -> PacCheck DA
    | "PacCheck:DB" -> PacCheck DB
    | "PacCheck:IA" -> PacCheck IA
    | "PacCheck:IB" -> PacCheck IB
    | "UndefinedInstruction" -> UndefinedInstruction
    | _ as s -> Warn.user_error "%s not a valid fault type" s

  let is s = try ignore (parse s); true  with  _ -> false

  let compare ft1 ft2 = Misc.polymorphic_compare ft1 ft2
end

module No = struct

  type t = unit

  let sets = []

  let pp () = "Default"
  let parse _ = Warn.user_error "Fault types not supported"
  let compare _ _ = Warn.user_error "Fault types not supported"
end

let is = AArch64.is
