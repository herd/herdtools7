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


module
  Make
    (O:sig val hexa : bool end)
    (A:ArchBase.S)(Pte:PteVal.S)(Intid:IntidVal.S)
  = struct
  include A

  module RegSet =
    MySet.Make
      (struct
        type t = reg
        let compare = reg_compare
      end)

  module ProcMap =
    MyMap.Make
      (struct
        type t = int
        let compare = Misc.int_compare
      end)

  type v = ParsedConstant.v
  let zero = ParsedConstant.zero
  let one = ParsedConstant.one
  let symbToV =  ParsedConstant.nameToV
  let maybevToV c = c
  let pp_v = ParsedConstant.pp_norm O.hexa Pte.pp_norm Intid.pp_norm

  type global = ParsedConstant.v
  let maybevToGlobal c = c

  include Location.Make
      (struct
        type arch_reg = A.reg
        let pp_reg = A.pp_reg
        let reg_compare = A.reg_compare

        type arch_global = global
        let pp_global = pp_v
        let global_compare = ParsedConstant.compare
      end)
(*
  type location =
    | Location_global of global
    | Location_reg of int * A.reg

  let pp_location = function
    | Location_global c -> ParsedConstant.pp O.hexa c
    | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)

  let pp_rval = function
    | Location_global c -> sprintf "*%s" (ParsedConstant.pp O.hexa c)
    | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)

 *)
  module FaultType = FaultType.No
  type fault_type = FaultType.t

  type test = (location,v,pseudo,fault_type) MiscParser.r3
  type prop = (location,v,fault_type) ConstrGen.prop
  type constr = prop ConstrGen.constr
end
