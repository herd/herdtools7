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

(* let comment = '@' *)
let comment = "#"

module Make(O:Arch_litmus.Config)(V:Constant.S) = struct
  include ARMBase
  module V = V

  module FaultType = FaultType.No

  let tab = Hashtbl.create 17
  let () =
    List.iter (fun (r,s) -> Hashtbl.add tab r s) regs

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | Internal i -> sprintf "i%i" i
  | _ ->
      try Misc.lowercase (Hashtbl.find tab r) with Not_found -> assert false

  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `ARM
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r _ =
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
          else None
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
  let nop = I_NOP

  include HardwareExtra.No

  module GetInstr = struct

      type t = instruction

      let self_instrs = [I_NOP; ]

      let lower_instr i = Misc.lowercase (dump_instruction i)

      let instr_name i =
        MyName.name_as_symbol (Misc.skip_spaces (lower_instr i))

      let fun_name i = sprintf "get%s" (instr_name i)

      let dump_instr dump = function
        | Constant.Instruction i -> instr_name i
        | v -> dump v

      module Make(O:Indent.S) = struct
      let dump i =
        O.f "static ins_t %s(void) {" (fun_name i) ;
        O.oi "ins_t *x1;" ;
        O.oi "ins_t r;" ;
        O.oi "asm __volatile__ (" ;
        O.fii "%S" "adr %[x1],0f\n\t" ;
        O.fii "%S" "ldr %[x2],[%[x1]]\n\t" ;
        O.fii "%S" "b 1f\n" ;
        O.fii "%S" "0:\n\t" ;
        O.fii "\"%s\\n\"" (lower_instr i) ;
        O.fii "%S" "1:\n" ;
        O.oii ":[x1] \"=&r\" (x1),[x2] \"=&r\" (r)" ;
        O.oii ":" ;
        O.oii ": \"cc\", \"memory\"" ;
        O.o ");" ;
        O.oi "return r;" ;
        O.o "}" ;
        O.o ""
    end
  end

end
