(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

let comment = "#"

module Make(O:Arch.Config)(V:Constant.S) = struct

  include AArch64Base
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

  let tab = Hashtbl.create 17
  let () = List.iter (fun (r,s) -> Hashtbl.add tab r s) regs

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | Internal i -> sprintf "i%i" i
  | _ ->
      try String.lowercase (Hashtbl.find tab r) with Not_found -> assert false

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
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
          else None
        let reg_class _ = "=&r"
        let comment = comment
      end)

end
