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

open Printf

let comment = "//"

module Make(V:Constant.S) = struct
  include BellBase
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

  let reg_to_string r = match r with
  | GPRreg _ -> pp_reg r
  | Symbolic_reg _ -> assert false

  include
      ArchExtra_litmus.Make
      (struct
        include Template.DefaultConfig
        let asmcomment = None
      end)
       (struct
        module V = V

        type arch_reg = reg
        let arch = `LISA
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init _r = None
        let reg_class _ = assert false
        let comment = comment
        let error _ _ = false
      end)

end
