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

module Make(O:Arch_litmus.Config)(V:Constant.S) = struct
  include X86_64Base
  module V = V

  let reg_to_string r = match r with
    | RIP -> "%rip"
(* We always want the 64 bits version of the register because the smaller version are created by GCC*)
    | Ireg (r, t) -> "%" ^ X86_64Base.reg64_string r
    | Internal i -> sprintf "i%i" i
    | _ -> assert false

  include
    ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `X86_64
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r = None

        let reg_class r=
          match r with
          (* as some instructions have eax as implicit argument,
             we must allocate our EAX to machine %eax
          | Ireg RAX -> "=&a"
          (* esi and edi implicit for MOVSD *)
          | Ireg RSI -> "=&S"
          | Ireg RDI -> "=&D" *)
          | Ireg (AX, _) -> "=&a"
          | _ -> "=&Q"
        let reg_class_stable r = reg_class r
        let comment = comment
        let error _ _ = false
      end)
  let nop =  I_NOP
end
