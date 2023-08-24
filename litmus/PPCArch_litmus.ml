(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let comment = "#"

module Make (O:Arch_litmus.Config)(V:Constant.S) = struct
  include PPCBase
  module V = V
  module FaultType = FaultType.No

  let ireg_to_string r = match r with
  | GPR0 -> "r0"
  | GPR1 -> "r1"
  | GPR2 -> "r2"
  | GPR3 -> "r3"
  | GPR4 -> "r4"
  | GPR5 -> "r5"
  | GPR6 -> "r6"
  | GPR7 -> "r7"
  | GPR8 -> "r8"
  | GPR9 -> "r9"
  | GPR10 -> "r10"
  | GPR11 -> "r11"
  | GPR12 -> "r12"
  | GPR13 -> "r13"
  | GPR14 -> "r14"
  | GPR15 -> "r15"
  | GPR16 -> "r16"
  | GPR17 -> "r17"
  | GPR18 -> "r18"
  | GPR19 -> "r19"
  | GPR20 -> "r20"
  | GPR21 -> "r21"
  | GPR22 -> "r22"
  | GPR23 -> "r23"
  | GPR24 -> "r24"
  | GPR25 -> "r25"
  | GPR26 -> "r26"
  | GPR27 -> "r27"
  | GPR28 -> "r28"
  | GPR29 -> "r29"
  | GPR30 -> "r30"
  | GPR31 -> "r31"

  let reg_to_string r = match r with
  | Ireg r -> ireg_to_string r
  | Internal i -> sprintf "i%i" i
  | LR -> "LR" (* Needed for BLR *)
  | Symbolic_reg _ -> assert false
  | _ -> assert false

  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `PPC
        let forbidden_regs = [Ireg GPR0]
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r =
          if reg_compare r base = 0 then Some ("_a->_scratch","int")
          else if reg_compare r max_idx = 0 then Some ("_a->_p->max_idx","int")
          else if reg_compare r loop_idx = 0 then Some ("_a->_p->max_loop","int")
          else if reg_compare r signal = 0 then Some ("_a->_p->sig_addr","int")
          else if reg_compare r tb0 = 0 then Some ("_tb0","tb_t")
          else if reg_compare r tb1 = 0 then Some ("_tb1","tb_t")
          else if reg_compare r tb_addr0 = 0 then Some ("&_tb0","tb_t *")
          else if reg_compare r tb_addr1 = 0 then Some ("&_tb1","tb_t *")
          else None
        let reg_class _ = "=&r"
        let reg_class_stable _ = "=&r"
        let comment = comment
        let error _ _ = false
        let warn _ _ = false
      end)

  let features = []
  let nop = Pnop

  include HardwareExtra.No

  module GetInstr = struct

      type t = instruction

      let self_instrs = [Pnop; ]

      let lower_instr i = Misc.lowercase (dump_instruction i)

      let instr_name i =
        MyName.name_as_symbol (Misc.skip_spaces (lower_instr i))

      let fun_name i = Printf.sprintf "get%s" (instr_name i)

      let dump_instr dump = function
        | Constant.Instruction i -> instr_name i
        | v -> dump v

      module Make(O:Indent.S) = struct
      let dump i =
        O.f "static ins_t %s(void) {" (fun_name i) ;
        O.oi "ins_t r = 0;" ;
        O.oi "asm __volatile__ (" ;
        O.fii "%S" "nop\n\t" ;
        O.oii ":" ;
        O.oii ":" ;
        O.oii ": \"cc\", \"memory\"" ;
        O.o ");" ;
        O.oi "return r;" ;
        O.o "}" ;
        O.o ""
    end
  end

end
