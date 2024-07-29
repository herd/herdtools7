(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
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
  include MIPSBase
  module V = V
  module FaultType = FaultType.No

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | _ -> pp_reg r


  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `MIPS
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init _r _v = None
(*
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
*)
        let reg_class _ = "=&r"
        let reg_class_stable _ = "=&r"
        let comment = comment
        let error _ _ = false
        let warn _ _ = false
      end)
  let features = []
  let nop = NOP

  include HardwareExtra.No

  module GetInstr = struct

      type t = instruction

      let self_instrs = [NOP; ]

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
