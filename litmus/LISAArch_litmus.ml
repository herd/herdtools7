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

let comment = "//"

module Make(V:Constant.S) = struct
  include BellBase
  module V = V

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
        let reg_class _ = ""
        let reg_class_stable _ = ""
        let comment = comment
        let error t1 t2 =
          let open CType in
          match t1,t2 with
          | (Base "int",Pointer _)
          | (Pointer _,Base "int")  ->
              true
          | _ -> false
        let warn _t1 _t2 = false
      end)
  let nop = Pnop
end
