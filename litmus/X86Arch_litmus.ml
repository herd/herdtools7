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
  include X86Base
  module V = V

  let reg_to_string r = match r with
  | EAX -> "%eax"
  | EBX -> "%ebx"
  | ECX -> "%ecx"
  | EDX -> "%edx"
  | ESI -> "%esi"
  | EDI -> "%edi"
  | EBP -> "%ebp"
  | Internal i -> sprintf "i%i" i
  | _ -> assert false

  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `X86
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r =
          if reg_compare r loop_idx = 0 then Some ("max_loop","int")
          else None

        let reg_class = function
          (* as some instructions have eax as implicit argument,
             we must allocate our EAX to machine %eax *)
          | EAX -> "=&a"
          (* esi and edi implicit for MOVSD *)
          | ESI -> "=&S"
          | EDI -> "=&D"
          | _ -> "=&r"
        let reg_class_stable r = reg_class r
        let comment = comment
        let error _ _ = false
        let warn _ _ = false
      end)
  let nop =  I_NOP
end
