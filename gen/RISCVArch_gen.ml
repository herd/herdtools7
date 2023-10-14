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

module Config = struct
  let naturalsize = MachSize.Word
  let moreedges = false
end

module Make
 (C:sig val naturalsize : MachSize.sz val moreedges : bool end) = struct

   include RISCVBase

(* Little endian, as far as I know *)
   let tr_endian = Misc.identity

(* No Scope *)
   module ScopeGen = ScopeGen.NoGen

(* Mixed size *)
   module Mixed =
     MachMixed.Make
       (struct
         let naturalsize = Some C.naturalsize
         let fullmixed = C.moreedges
       end)

   include NoWide

(*********)
(* Atoms *)
(*********)

   let bellatom = false

   module SIMD = NoSIMD

   type atom = MO of mo | Atomic of mo * mo | Mixed of MachMixed.t

   let default_atom = Atomic (Rlx,Rlx)

   let applies_atom a d = match a with
   | MO mo ->
       begin match mo,d with
       | (Acq,Code.W)|(Rel,Code.R) -> false
       | (Rel, Code.W)|(Acq, Code.R)
       | ((Rlx|AcqRel), _) -> true
       | _,Code.J|_,Code.D|_,Code.I -> assert false
       | Sc,_ -> assert false
       end
   | Atomic _|Mixed _ -> true

   let pp_plain = "P"

   let pp_as_a = None

   let pp_mo = function
     | Rlx -> "P"
     | Acq -> "Aq"
     | Rel -> "Rl"
     | AcqRel -> "AR"
     | Sc -> assert false

   let pp_mo2 m1 m2 = match m1,m2 with
   | Rlx,Rlx -> ""
   | _,_ -> pp_mo m1 ^ pp_mo m2

   let pp_atom = function
     | MO mo -> pp_mo mo
     | Atomic (m1,m2) -> "X" ^ pp_mo2 m1 m2
     | Mixed m -> Mixed.pp_mixed m

   let compare_atom = compare

   let get_access_atom = function
     | None
     | Some (MO _|Atomic _) -> None
     | Some (Mixed m) -> Some m

   let set_access_atom a sz =
     Some
       (match a with
        | None|Some (Mixed _) -> Mixed sz
        | Some (MO _|Atomic _ as a) -> a)

   let fold_mixed f k = Mixed.fold_mixed (fun  mix r -> f (Mixed mix) r) k

   let fold_mo f k =
     let k = f Acq k in
     let k = f Rel k in
     let k = f AcqRel k in
     k

   let fold_rmw f k =
     let fold1 f k = fold_mo f (f Rlx k) in
     fold1
       (fun m1 k -> fold1 (fun m2 k -> f (Atomic (m1,m2)) k) k)
       k

   let fold_non_mixed f k =
     let k = fold_mo (fun mo k -> f (MO mo) k) k in
     fold_rmw f k

   let fold_atom f k =
     let k = fold_mixed f k in
     fold_non_mixed f k

   let worth_final = function
     | Atomic _ -> true
     | MO _|Mixed _ -> false

   let varatom_dir _ f k = f None k

   let merge_atoms a1 a2 = if a1=a2 then Some a1 else None

   let overlap_atoms a1 a2 =
     match a1,a2 with
     | ((MO _|Atomic _),_)|(_,(MO _|Atomic _)) -> true
     | Mixed m1,Mixed m2 -> MachMixed.overlap m1 m2

   let atom_to_bank _ = Code.Ord

   let tr_value ao v = match ao with
   | None| Some (MO _|Atomic _) -> v
   | Some (Mixed (sz,_)) -> Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)

   let overwrite_value v ao w = match ao with
   | None| Some (MO _|Atomic _) -> w (* total overwrite *)
   | Some (Mixed (sz,o)) ->
       ValsMixed.overwrite_value v sz o w

   let extract_value v ao = match ao with
   | None| Some (MO _|Atomic _) -> v
   | Some (Mixed (sz,o)) ->
       ValsMixed.extract_value v sz o

   module PteVal = PteVal_gen.No(struct type arch_atom = atom end)

(* End of atoms *)

   type fence = barrier

   let is_isync = function
  | FenceI  -> true
  | _ -> false

   let compare_fence = barrier_compare

   let default = Fence (IORW,IORW)
   let strong = default

   let pp_fence f = Misc.capitalize (pp_barrier_dot f)

   let add_iorw fold f k = fold f k |> f (Fence (IORW,IORW))

   let do_fold_fence f = add_iorw do_fold_fence f
   and fold_barrier f = add_iorw fold_barrier f
   
   let fold_cumul_fences f k = do_fold_fence f k
   let fold_all_fences f k = fold_barrier f k
   let fold_some_fences = fold_all_fences

   let applies r d = match r,d with
   | (IORW|RW),_
   | (IR|R),Code.R
   | (OW|W),Code.W
     -> true
   | (W|OW),Code.R
   | (R|IR),Code.W
     -> false
   | _ -> assert false

   let orders f d1 d2 = match f with
   | FenceI -> false
   | FenceTSO ->
       begin match d1,d2 with
       | Code.W,Code.R -> false
       | _,_ -> true
       end
   | Fence (r1,r2) -> applies r1 d1 && applies r2 d2

   let var_fence f r = f default r

(********)
(* Deps *)
(********)
include Dep

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlFenceI"

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
      include NoSpecial
    end)

 end
