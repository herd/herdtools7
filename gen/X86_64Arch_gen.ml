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

module Config = struct
  let naturalsize = MachSize.Word
  let fullmixed = true
end

module Make
    (C:sig
      val naturalsize : MachSize.sz
      val fullmixed : bool
    end) = struct

      open Printf

      include X86_64Base
      let tr_endian = Misc.identity

      module ScopeGen = ScopeGen.NoGen
      module Mixed =
        MachMixed.Make
          (struct
            let naturalsize = Some C.naturalsize
            let fullmixed = C.fullmixed
          end)

      let bellatom = false

      module SIMD = NoSIMD

      type atom_acc = Plain | Atomic | NonTemporal

      type atom = atom_acc * MachMixed.t option

      let default_atom = Atomic,None

      let applies_atom a d = match a,d with
      | ((Atomic,_),Code.W)
      | (((Plain|NonTemporal),_),(Code.W|Code.R)) -> true
      | ((Atomic,_),Code.R)
      | (_,Code.J)|(_,Code.I)|(_,Code.D)-> false

      let compare_atom = compare

      include
        MachMixed.Util
          (struct
            type at = atom_acc
            let plain = Plain
          end)

      let pp_plain = Code.plain

      let pp_as_a = None

      let pp_atom_acc = function
        | Atomic -> "A" | Plain -> ""
        | NonTemporal -> "NT"

      let pp_atom = function
        | a,None -> pp_atom_acc a
        | a,Some m -> sprintf "%s%s" (pp_atom_acc a) (Mixed.pp_mixed m)

      let fold_mixed f r =
        Mixed.fold_mixed (fun mix r -> f (Plain,Some mix) r) r

      let fold_acc f k = f Atomic (f NonTemporal k)

      let fold_non_mixed f r =
        fold_acc (fun acc r -> f (acc,None) r) r

      let apply_mix f acc m r = match acc,m with
      | (NonTemporal,(None|Some ((MachSize.Quad|MachSize.Word),_)))
      | ((Plain|Atomic),_) ->
          f (acc,m) r
      |  (NonTemporal,Some ((MachSize.Short|MachSize.Byte),_))
        -> r
      | (NonTemporal,Some (MachSize.S128,_)) -> assert false

      let fold_atom f r =
        fold_acc
          (fun acc r ->
            Mixed.fold_mixed
              (fun m r -> apply_mix f acc (Some m) r)
              (f (acc,None) r))
          (fold_mixed f r)

      let worth_final (a,_) = match a with
        | NonTemporal|Plain -> false
        | Atomic  -> true

      let varatom_dir _d f = f None

      let check_nt a sz =
        apply_mix (fun c _ -> Some c) a sz None

      let merge_atoms a1 a2 = match a1,a2 with
        | ((Plain,sz),(a,None))
        | ((a,None), (Plain,sz)) -> check_nt a sz
        | ((a1,None),(a2,sz))
        | ((a1,sz),(a2,None)) when a1=a2 ->
            check_nt a1 sz
        | ((Plain,sz1),(a,sz2))
        | ((a,sz1),(Plain,sz2)) when sz1=sz2 ->
            check_nt a sz1
        | _,_ -> if a1=a2 then Some a1 else None

      let overlap_atoms a1 a2 = match a1,a2 with
        | ((_,None),_)|(_,(_,None)) -> true
        | (_,Some m1),(_,Some m2) -> MachMixed.overlap m1 m2

      let atom_to_bank _ = Code.Ord

(**************)
(* Mixed size *)
(**************)

      let tr_value ao v = match ao with
      | None | Some ((NonTemporal|Plain|Atomic),None) -> v
      | Some ((NonTemporal|Plain|Atomic), Some (sz, _)) ->
         Mixed.tr_value sz v

      module ValsMixed =
        MachMixed.Vals
          (struct
            let naturalsize () = C.naturalsize
            let endian = endian
          end)

      let overwrite_value v ao w = match ao with
      | None | Some ((Plain|Atomic|NonTemporal),None) -> w
      | Some ((Plain|Atomic|NonTemporal),Some (sz, o)) ->
         ValsMixed.overwrite_value v sz o w

      let extract_value v ao = match ao with
      | None | Some ((Plain|Atomic|NonTemporal),None) -> v
      | Some ((Plain|Atomic|NonTemporal),Some (sz, o)) ->
         ValsMixed.extract_value v sz o

      include NoWide

      module PteVal = PteVal_gen.No(struct type arch_atom = atom end)

      (**********)
      (* Fences *)
      (**********)

      type flushline = Prev|Next|Other

      type fence = Fence of barrier | ClFlush of opt * flushline

      let is_isync _ = false

      let compare_fence f1 f2 = match f1,f2 with
        | Fence b1,Fence b2 -> barrier_compare b1 b2
        | ClFlush (o1,l1),ClFlush (o2,l2) ->
           begin match compare o1 o2 with
           | 0 -> compare l1 l2
           | r -> r
           end
        | ClFlush _,Fence _ -> -1
        | Fence _,ClFlush _ -> 1

      let default = Fence MFENCE
      let strong = default

      let pp_barrier = function
        | MFENCE -> "MFence"
        | SFENCE -> "SFence"
        | LFENCE -> "LFence"

      let pp_fence = function
        | Fence b -> pp_barrier b
        | ClFlush (NoOpt,Prev) -> "ClFlush"
        | ClFlush (Opt,Prev) -> "ClFlushOpt"
        | ClFlush (NoOpt,Next) -> "ClFlushNext"
        | ClFlush (Opt,Next) -> "ClFlushOptNext"
        | ClFlush (NoOpt,Other) -> "ClFlushOther"
        | ClFlush (Opt,Other) -> "ClFlushOptOther"

      let fold_all_barriers f r = f MFENCE (f SFENCE (f LFENCE r))
      let fold_all_fences f r =
        let r = f (ClFlush (NoOpt,Prev)) (f (ClFlush (Opt,Prev)) r) in
        let r = f (ClFlush (NoOpt,Next)) (f (ClFlush (Opt,Next)) r) in
        let r = f (ClFlush (NoOpt,Other)) (f (ClFlush (Opt,Other)) r) in
        fold_all_barriers (fun b -> f (Fence b)) r
      let fold_cumul_fences = fold_all_fences
      let fold_some_fences f r =  f default r

      let orders f d1 d2 =
        let open Code in match f,d1,d2 with
        | (Fence MFENCE,_,_)
        | (Fence SFENCE,W,W)
        | (Fence LFENCE,R,R)
        | (ClFlush _,_,_) (* Do not know exactly yet *)
          -> true
        | (_,_,_)
          -> false

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
      let is_addr _ = assert false
      let fst_dp _ = assert false
      let sequence_dp _ _ = assert false

      (*******)
      (* RWM *)
      (*******)

      include Exch.Exch(struct type arch_atom = atom end)
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
            type special = xmm
            let specials = xmms
          end)
    end
