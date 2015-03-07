(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*                 Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


module Make(V:Constant.S) =
  struct
    include MIPSBase
    include MachAtom
    module V = V

(**********)
(* Fences *)
(**********)
    type fence = barrier

    let is_isync _ = false

    let compare_fence = barrier_compare

    let strong = Sync

    let pp_fence = pp_barrier

    let fold_all_fences f r = f Sync r
    let fold_cumul_fences f r = f Sync r
    let fold_some_fences f r = f Sync r

    let orders f d1 d2 = match f,d1,d2 with
    | Sync,_,_ -> true

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

    let fst_dp = function
      | CTRL -> [CTRL]
      | ADDR|DATA -> []

    let sequence_dp d1 d2 = match d1 with
    | ADDR -> [d2]
    | DATA|CTRL -> []

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
  end
