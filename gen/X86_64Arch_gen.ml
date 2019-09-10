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

open Code
include X86_64Base
let tr_endian = Misc.identity

module ScopeGen = ScopeGen.NoGen

let bellatom = false

type atom = Atomic

let default_atom = Atomic

let applies_atom a d = assert false

let compare_atom = compare

let merge_atoms Atomic Atomic = Some Atomic

let pp_plain = Code.plain

let pp_as_a = None

let pp_atom = function
  | Atomic -> "A"

let fold_non_mixed f k = f Atomic k

let fold_atom f k =  fold_non_mixed f k

let worth_final _ = true

let varatom_dir _d f = f None

include NoMixed

(**********)
(* Fences *)
(**********)

type fence = MFence

let is_isync _ = false

let compare_fence = compare

let default = MFence
let strong = default

let pp_fence = function
  | MFence -> "MFence"

let fold_all_fences f r = f MFence r
let fold_cumul_fences f r = f MFence r
let fold_some_fences f r =  f MFence r

let orders f d1 d2 = match f,d1,d2 with
| MFence,_,_ -> true

let var_fence f r = f default r

(********)
(* Deps *)
(********)

type dp

let pp_dp _ = assert false

let fold_dpr _f r =  r
let fold_dpw _f r =  r

let ddr_default = None
let ddw_default = None
let ctrlr_default = None
let ctrlw_default = None

let is_ctrlr _ = assert false
let fst_dp _ = assert false
let sequence_dp _ _ = assert false

(*******)
(* RWM *)
(*******)

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
    end)
