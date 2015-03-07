(**************************************************************************)
(*                                  DIY                                   *)
(*                                                                        *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.          *)
(* Shaked Flur, Susmit Sarkar, Peter Sewell, University of Cambridge, UK. *)
(*                                                                        *)
(*  Copyright 2015 Institut National de Recherche en Informatique et en   *)
(*  Automatique and the authors. All rights reserved.                     *)
(*  This file is distributed  under the terms of the Lesser GNU General   *)
(*  Public License.                                                       *)
(**************************************************************************)


let comment = '#' (* AArch64.comment ??? *)

module Make(O:Arch.Config)(V:Constant.S) = struct
  include AArch64GenBase
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

  let reg_to_string = AArch64GenBase.pp_reg

  include
      ArchExtra.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `AArch64
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r =
          if reg_compare r base = 0 then Some ("_a->_scratch","int")
          else if reg_compare r max_idx = 0 then Some ("_a->_p->max_idx","int")
          else if reg_compare r loop_idx = 0 then Some ("_a->_p->max_loop","int")
          else None
        let reg_class _ = "=&r"
        let comment = comment
      end)

end
