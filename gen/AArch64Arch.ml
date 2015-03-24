(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code
open Printf

include AArch64Base

(* AArch64 has more atoms that others *)
type atom_rw =  PP | PL | AP | AL
type atom = Acq | Rel | Atomic of atom_rw

let default_atom = Atomic PP

let applies_atom a d = match a,d with
| Acq,R
| Rel,W
| Atomic _,(R|W)
  -> true
| _ -> false

let applies_atom_rmw ar aw = match ar,aw with
| (Some Acq|None),(Some Rel|None) -> true
| _ -> false

let pp_plain = "P"
(* Annotation A is taken by load aquire *)
let pp_as_a = None

let pp_atom_rw = function
  | PP -> ""
  | PL -> "L"
  | AP -> "A"
  | AL -> "AL"

let pp_atom = function
  | Atomic rw -> sprintf "X%s" (pp_atom_rw rw)
  | Rel -> "L"
  | Acq -> "A"

let compare_atom = Pervasives.compare

let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))
let fold_atom f r = 
  f Acq (f Rel (fold_atom_rw (fun rw -> f (Atomic rw)) r))

let worth_final = function
  | Atomic _ -> true
  | Acq|Rel -> false

(* End of atoms *)

(**********)
(* Fences *)
(**********)

type fence = barrier

let is_isync = function
  | ISB -> true
  | _ -> false

let compare_fence = barrier_compare

let strong = DMB (SY,FULL)

let pp_fence f = do_pp_barrier "." f

let fold_cumul_fences f k = do_fold_dmb_dsb f k

let fold_all_fences f k = fold_barrier f k

let fold_some_fences f k =
  let k = f ISB k  in
  let k = f (DMB (SY,FULL)) k in
  let k = f (DMB (SY,ST)) k in
  let k = f (DMB (SY,LD)) k in
  k

let orders f d1 d2 = match f,d1,d2 with
| ISB,_,_ -> false
| (DSB (_,FULL)|DMB (_,FULL)),_,_ -> true
| (DSB (_,ST)|DMB (_,ST)),W,W -> true
| (DSB (_,ST)|DMB (_,ST)),_,_ -> false
| (DSB (_,LD)|DMB (_,LD)),Code.R,(W|Code.R) -> true
| (DSB (_,LD)|DMB (_,LD)),_,_ -> false


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

