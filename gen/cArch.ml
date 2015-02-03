(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
(* Memory order *)
open MemOrder  

let sig_of_mem_order = function
  | Acq -> 'D'
  | Rel -> 'E'
  | Acq_Rel -> 'F'
  | SC -> 'G'
  | Rlx -> 'H'
  | Con -> 'I'

(* Atoms *)
open Code
type atom = MemOrder.t
let default_atom = SC
let applies_atom a d = match a,d with
| (Acq|Acq_Rel|Con),W -> false
| (Rel|Acq_Rel),R -> false
| _,_ -> true

let applies_atom_rmw = function
  | Some Con|None -> false
  | Some _ -> true

let compare_atom = Pervasives.compare
let sig_of_atom = sig_of_mem_order
let pp_as_a = Some SC
let pp_atom = pp_mem_order_short

let fold_atom f k =
  let k = f Acq k in
  let k = f Rel k in
  let k = f Acq_Rel k in
  let k = f SC k in
  let k = f Rlx k in
  let k = f Con k in
  k

let worth_final _ = false

(* Fences, to be completed *)
type fence = MemOrder.t

let is_isync _ = false

let compare_fence = MemOrder.compare

let strong = SC

let pp_fence f = sprintf "Fence%s" (pp_mem_order_short f)


let sig_of_fence f = sig_of_mem_order f

let do_fold_fence f k =
  let k = f Acq k in
  let k = f Rel k in
  let k = f Acq_Rel k in
  let k = f SC k in
  let k = f Rlx k in
  let k = f Con k in
  k

let fold_cumul_fences = do_fold_fence
let fold_all_fences =  do_fold_fence
let fold_some_fences =  do_fold_fence

let orders _f _d1 _d2 = true

(* Basic C arch *)
type arch_reg = { id:int }

let dump_reg r = sprintf "r%i" r.id

type location =
  | Loc of Code.loc
  | Reg of Code.proc * arch_reg

let dump_loc = function
  | Loc loc -> loc
  | Reg (p,r) -> sprintf "%i:%s" p (dump_reg r)

let pp_location = dump_loc

let location_compare = Pervasives.compare

let of_reg p r = Reg (p,r)
let of_loc loc = Loc loc

type tbase = TypBase.t

let dump_tbase t =
  let open TypBase in
  match t with
  | LongLong -> "long long"
  | Long -> "long"
  | Int -> "int"
  | Short -> "short"
  | Char -> "char"

type typ = Plain of tbase | Atomic of tbase

let dump_typ = function
  | Plain t -> dump_tbase t
  | Atomic TypBase.LongLong -> "atomic_llong"
  | Atomic t -> sprintf "atomic_%s" (dump_tbase t)

type exp =
  | Load of location
  | AtomicLoad of MemOrder.t * location
  | AtomicExcl of MemOrder.t * location * Code.v
  | Deref of exp
  | Const of Code.v
  | AssertVal of exp * Code.v

let addrs_of_location = function
  | Reg _ -> StringSet.empty
  | Loc loc -> StringSet.singleton loc

let rec addrs_of_exp = function
  | Const _ -> StringSet.empty
  | AtomicLoad (_,loc)|AtomicExcl (_,loc,_)|Load loc -> addrs_of_location loc
  | Deref e|AssertVal (e,_) -> addrs_of_exp e

type cond = Eq | Ne

type condexp = exp * cond * exp
type ins =
  | Seq of ins * ins
  | Decl of typ * arch_reg * exp option
  | Store of Code.loc * exp
  | SetReg of arch_reg * exp
  | AtomicStore of MemOrder.t * Code.loc * exp
  | Fence of fence
  | Loop of ins
  | If of condexp * ins * ins option
  | Break
  | Decr of arch_reg
  | Nop

let addrs_ofcondexp (e1,_,e2) =
  StringSet.union (addrs_of_exp e1) (addrs_of_exp e2)

let rec addrs_of = function
  | Break | Fence _ | Decr _ | Nop | Decl (_,_,None) -> StringSet.empty
  | Seq (i1,i2) -> StringSet.union (addrs_of i1) (addrs_of i2)
  | Decl (_,_,Some e)
  | SetReg (_,e) -> addrs_of_exp e
  | Store (loc,e)|AtomicStore (_,loc,e) -> StringSet.add loc (addrs_of_exp e)
  | Loop i -> addrs_of i
  | If (ce,itrue,ifalse) ->
      StringSet.union (addrs_ofcondexp ce)
        (StringSet.union (addrs_of itrue) (addrs_of_opt ifalse))
and addrs_of_opt = function
  | None -> StringSet.empty
  | Some i -> addrs_of i

let seq i1 i2 = match i1,i2 with
| (Nop,i)|(i,Nop) -> i
| _,_ -> Seq (i1,i2)

let seqs is = List.fold_right seq is Nop

let rec is_nop = function
  | Nop|Decl (_,_,None) -> true
  | Seq (i1,i2) -> is_nop i1 && is_nop i2
  | _ -> false

(* No dependencies *)
type dp

let pp_dp _ = assert false
let sig_of_dp _ = assert false
let fold_dpr  _f k = k
let fold_dpw _f k = k

let ddr_default = None
let ddw_default = None
let ctrlr_default = None
let ctrlw_default = None

let is_ctrlr _ = assert false
let fst_dp _ = assert false
let sequence_dp _ _ = assert false

