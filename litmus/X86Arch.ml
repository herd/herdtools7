(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

let comment = "#"

module Make(O:Arch.Config)(V:Constant.S) = struct
  include X86Base
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

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
      ArchExtra.Make(O)
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

        let comment = comment
      end)

end
