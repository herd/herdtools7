(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

let comment = '#'

module Make(O:Arch.Config)(V:Constant.S) = struct
  include MIPSBase
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | _ -> pp_reg r


  include
      ArchExtra.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `MIPS
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r = None
(*
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
*)
        let reg_class _ = "=&r"
        let comment = comment
      end)

end
