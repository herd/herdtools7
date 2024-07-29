(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
module Config = struct
  let moreedges = false
end

module Make(C:sig val moreedges : bool end) = struct
include ARMBase

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

type fence = DMB of barrier_option | DSB of barrier_option | ISB

let is_isync = function
  | ISB -> true
  | _ -> false

let compare_fence = compare

let default = DMB SY
let strong = default

let pp_fence = function
  | DMB SY -> "DMB"
  | DMB o -> sprintf "DMB.%s" (pp_option o)
  | DSB SY -> "DSB"
  | DSB o -> sprintf "DSB.%s" (pp_option o)
  | ISB -> "ISB"

let fo f r =  f SY (f ST r)
let fold_cumul_fences f r =
  fo (fun o -> f (DMB o)) (fo (fun o -> f (DSB o)) r)
let fold_all_fences f r = f ISB (fold_cumul_fences f r)
(* Do not include DSB in 'some fences' at the moment *)
let fold_some_fences f r = f ISB (fo (fun o -> f (DMB o)) r)


let orders _f _d1 _d2 =true

let var_fence f r = f default r

(********)
(* Deps *)
(********)

include Dep

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlIsb"

(*******)
(* RWM *)
(*******)
include Exch.LxSx(struct type arch_atom = atom end)

include NoEdge

  include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg

      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false

      let pp_reg = pp_reg
      let pp_i _ = assert false
      let free_registers = allowed_for_symb
      include NoSpecial
    end)

end
