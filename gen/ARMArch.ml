(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
include ARMBase
include MachAtom

(**********)
(* Fences *)
(**********)

type fence = DMB of barrier_option | DSB of barrier_option | ISB

let is_isync = function
  | ISB -> true
  | _ -> false

let compare_fence = compare

let strong = DMB SY

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

(********)
(* Deps *)
(********)

include Dep

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlIsb"

include
    ArchExtra.Make
    (struct
      type arch_reg = reg

      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false

      let pp_reg = pp_reg
      let free_registers = allowed_for_symb
    end)

