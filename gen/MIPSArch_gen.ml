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

module Config = struct
  let moreedges = false
end

module Make(C:sig val moreedges : bool end) = struct
include MIPSBase
module ScopeGen = ScopeGen.NoGen

let tr_endian = Misc.identity
include MachAtom.Make
    (struct
      let naturalsize=None
      let endian = endian
      let fullmixed = C.moreedges
    end)

module PteVal = PteVal_gen.No(struct type arch_atom = atom end)

(**********)
(* Fences *)
(**********)
type fence = barrier

let is_isync _ = false

let compare_fence = barrier_compare

let default = Sync
let strong = default

let pp_fence = pp_barrier

let fold_all_fences f r = f Sync r
let fold_cumul_fences f r = f Sync r
let fold_some_fences f r = f Sync r

let orders f d1 d2 = match f,d1,d2 with
| Sync,_,_ -> true

let var_fence f r = f default r

(********)
(* Deps *)
(********)

type dp = ADDR | DATA | CTRL

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"

let fold_dpr f r =  f ADDR (f CTRL r)
let fold_dpw f r =  f ADDR (f DATA (f CTRL r))

let ddr_default = Some ADDR
let ddw_default = Some DATA
let ctrlr_default = Some CTRL
let ctrlw_default = Some CTRL

let is_ctrlr = function
  | CTRL -> true
  | _ -> false

let is_addr = function
  | ADDR -> true
  | _ -> false

let fst_dp = function
  | CTRL -> [CTRL]
  | ADDR|DATA -> []

let sequence_dp d1 d2 = match d1 with
| ADDR -> [d2]
| DATA|CTRL -> []

include OneRMW
include NoEdge

include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg
      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false
      let pp_reg = pp_reg
      let free_registers = allowed_for_symb
      include NoSpecial
    end)
end
