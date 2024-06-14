(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let comment = "#"

module Make(O:Arch_litmus.Config)(V:Constant.S) = struct

  include RISCVBase
  module V = V
  module FaultType = FaultType.No

  let reg_to_string r = match r with
  | Symbolic_reg _|RESADDR -> assert false
  | Ireg _ -> pp_reg r


  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `RISCV
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init _r _v = None
        let reg_class _ = "=&r"
        let reg_class_stable _ = "=&r"
        let comment = comment

        let error t1 t2 =
          let open CType in
(*          Printf.eprintf "Error %s and %s\n" (debug t1) (debug t2) ; *)
          match t1,t2 with
          | (Base
               ("int"|"ins_t"|"int16_t"|"uint16_t"|"int8_t"|"uint8_t"),
             Pointer _)
          | (Pointer _,
             Base ("int"|"ins_t"|"int16_t"|"uint16_t"|"int8_t"|"uint8_t"))  ->
              true

          | _ -> false

        let warn t1 t2 =
          let open CType in
          match t1,t2 with
          | Base ("ins_t"|"int"|"int32_t"|"uint32_t"
                  |"int16_t"|"uint16_t"
                  |"int8_t"|"uint8_t"),
            Base ("ins_t"|"int"|"int32_t"|"uint32_t") -> false
          | (Base "int",_)|(_,Base "int") -> true
          | _ -> false

      end)
  let features = []
  let nop = INop

  include HardwareExtra.No

  module GetInstr = GetInstr.No(struct type instr = instruction end)

end
