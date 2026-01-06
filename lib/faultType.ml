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
  val matches : t -> t -> bool
end

module type AArch64Sig = sig
  type mmu_t =
    | Translation
    | AccessFlag
    | Permission
    | Exclusive

  type t =
    | MMU of DISide.t * mmu_t
    | TagCheck
    | UndefinedInstruction
    | SupervisorCall
    | PacCheck of PAC.key

  include S with type t := t
end

module AArch64 = struct
  type mmu_t =
    | Translation
    | AccessFlag
    | Permission
    | Exclusive

  let pp_mmu_t = function
    | Translation -> "Translation"
    | AccessFlag -> "AccessFlag"
    | Permission -> "Permission"
    | Exclusive -> "Exclusive"

  type t =
    | MMU of DISide.t * mmu_t
    | TagCheck
    | UndefinedInstruction
    | SupervisorCall
    | PacCheck of PAC.key

  let sets =
    let open DISide in
    [
      "MMU", [MMU (Data, Translation);
              MMU (Data, AccessFlag);
              MMU (Data, Permission);
              MMU (Data, Exclusive);
              MMU (Instr, Translation);
              MMU (Instr, AccessFlag);
              MMU (Instr, Permission);
              MMU (Instr, Exclusive)];

      "D-MMU", [MMU (Data, Translation);
                MMU (Data, AccessFlag);
                MMU (Data, Permission);
                MMU (Data, Exclusive)];

      "Translation", [MMU (Data, Translation);
                      MMU (Instr, Translation)];

      "AccessFlag", [MMU (Data, AccessFlag);
                     MMU (Instr, AccessFlag)];

      "Permission", [MMU (Data, Permission);
                     MMU (Instr, Permission)];

      "Exclusive", [MMU (Data, Exclusive);
                    MMU (Instr, Exclusive)];

      "TagCheck", [TagCheck];
      "PacCheck", [PacCheck PAC.DA;
                   PacCheck PAC.DB;
                   PacCheck PAC.IA;
                   PacCheck PAC.IB];
      "UndefinedInstruction",[UndefinedInstruction];
      "SVC", [SupervisorCall];
    ]

  let pp = function
    | MMU (DISide.Any, m) ->
        Printf.sprintf "MMU:%s" (pp_mmu_t m)
    | MMU (d, m) ->
        Printf.sprintf "%s-MMU:%s" (DISide.pp d) (pp_mmu_t m)
    | TagCheck -> "TagCheck"
    | UndefinedInstruction -> "UndefinedInstruction"
    | SupervisorCall -> "SupervisorCall"
    | PacCheck k -> Printf.sprintf "PacCheck:%s" (PAC.pp_upper_key k)

  let parse =
    let open DISide in
    function
      | "MMU:Translation" -> MMU (Any, Translation)
      | "MMU:AccessFlag"  -> MMU (Any, AccessFlag)
      | "MMU:Permission"  -> MMU (Any, Permission)
      | "MMU:Exclusive"   -> MMU (Any, Exclusive)
      | "D-MMU:Translation" -> MMU (Data, Translation)
      | "D-MMU:AccessFlag"  -> MMU (Data, AccessFlag)
      | "D-MMU:Permission"  -> MMU (Data, Permission)
      | "D-MMU:Exclusive"   -> MMU (Data, Exclusive)
      | "I-MMU:Translation" -> MMU (Instr, Translation)
      | "I-MMU:AccessFlag"  -> MMU (Instr, AccessFlag)
      | "I-MMU:Permission"  -> MMU (Instr, Permission)
      | "I-MMU:Exclusive"   -> MMU (Instr, Exclusive)
      | "TagCheck" -> TagCheck
      | "PacCheck:DA" -> PacCheck PAC.DA
      | "PacCheck:DB" -> PacCheck PAC.DB
      | "PacCheck:IA" -> PacCheck PAC.IA
      | "PacCheck:IB" -> PacCheck PAC.IB
      | "UndefinedInstruction" -> UndefinedInstruction
      | "SupervisorCall" -> SupervisorCall
      | _ as s -> Warn.user_error "%s not a valid fault type" s

  let is s = try ignore (parse s); true  with  _ -> false

  let compare ft1 ft2 = Misc.polymorphic_compare ft1 ft2

  let matches actual expected =
    match actual, expected with
    | MMU (_, m_actual), MMU (DISide.Any, m_expected) ->
        m_actual = m_expected
    | _ -> actual = expected
end

module No = struct

  type t = unit

  let sets = []

  let pp () = "Default"
  let parse _ = Warn.user_error "Fault types not supported"
  let compare _ _ = Warn.user_error "Fault types not supported"
  let matches () () = true
end

let is = AArch64.is
