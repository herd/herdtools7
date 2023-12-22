(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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
  let fullmixed = false
  let variant _ = false
end

module Make
    (C:sig
      val naturalsize : MachSize.sz
      val moreedges : bool
      val fullmixed : bool
      val variant : Variant_gen.t -> bool
    end) = struct

let do_self = C.variant Variant_gen.Self
let do_tag = C.variant Variant_gen.MemTag
let do_morello = C.variant Variant_gen.Morello
let do_fullkvm = C.variant Variant_gen.FullKVM
let do_kvm = do_fullkvm || C.variant Variant_gen.KVM
let do_neon = C.variant Variant_gen.Neon
let do_mixed = Variant_gen.is_mixed  C.variant
let do_cu = C.variant Variant_gen.ConstrainedUnpredictable

open Code
open Printf

include MakeAArch64Base.Make(struct let is_morello = do_morello end)

(* Little endian *)
let tr_endian = Misc.identity

module ScopeGen = ScopeGen.NoGen

(* Mixed size *)
module Mixed =
  MachMixed.Make
    (struct
      let naturalsize = Some C.naturalsize
      let fullmixed = C.fullmixed
    end)

(* AArch64 has more atoms that others *)
let bellatom = false
module SIMD = struct

  type atom = NeP|NeAcqPc|NeRel|Ne1|Ne2|Ne3|Ne4|Ne2i|Ne3i|Ne4i|NePa|NePaN

  let fold_neon f r = r |>
    f NeAcqPc |> f NeRel |>
    f NeP |>
    f NePa |> f NePaN |>
    f Ne1 |> f Ne2 |> f Ne3 |> f Ne4 |>
    f Ne2i |> f Ne3i |> f Ne4i

  let nregs = function
    | Ne1 -> 1
    | Ne2 | Ne2i -> 2
    | Ne3 | Ne3i -> 3
    | Ne4 | Ne4i -> 4
    | _ -> 1

  let nelements = function
    | Ne1|Ne2|Ne2i|Ne3|Ne3i|Ne4|Ne4i -> 4
    | NePa|NePaN -> 2
    | NeP | NeAcqPc | NeRel -> 1

  let pp_opt = function
    | Ne2i | Ne3i | Ne4i -> "i"
    | _ -> ""

  let pp n =
    match n with
    | Ne1 | Ne2 | Ne3 | Ne4 | Ne2i | Ne3i | Ne4i ->
       Printf.sprintf "Ne%i%s" (nregs n) (pp_opt n)
    | NePa -> "NePa"
    | NePaN -> "NePaN"
    | NeP -> "NeP"
    | NeAcqPc -> "NeQ"
    | NeRel -> "NeL"

  let initial sz =
    let sz = if sz <= 0 then 1 else sz in
    Array.make sz 0

  let step n start v =
    let start = start+1 in
    let el = nelements n in
    let sz = nregs n in
    let v = Array.copy v in
    for k = 0 to sz-1 do
      for i=0 to el-1 do
        let j = match n with
          | Ne2i | Ne3i | Ne4i -> k+i*sz
          | NeP | NeAcqPc | NeRel | NePa | NePaN
          | Ne1 | Ne2 | Ne3 | Ne4 -> i+k*el
        in
       v.(j) <- start+k
      done
    done ;
    v

  let read n v =
    let el = nelements n in
    let sz = nregs n in
    let access r k = match n with
      | Ne2i | Ne3i | Ne4i -> sz*k + r
      | NeP | NeAcqPc | NeRel | NePa | NePaN
      | Ne1 | Ne2 | Ne3 | Ne4 -> el*r + k
    in
    let rec reg r k =
      if k >= el then []
      else v.(access r k)::reg r (k+1) in
    let rec regs r =
      if r >= sz then []
      else reg r 0::regs (r+1) in
    regs 0

  let reduce vec =
    List.fold_right (+) (List.flatten vec) 0
end

type atom_rw =  PP | PL | AP | AL
type capa = Capability
type capa_opt = capa option

module WPTE = struct

  type t = AF | DB | OA | DBM | VALID

  let all= [AF; DB; OA; DBM; VALID;]

  let compare w1 w2 = match w1,w2 with
  | (AF,AF) | (DB,DB) | (OA,OA) | (DBM,DBM) | (VALID,VALID)
    -> 0
  | (AF,(DB|OA|DBM|VALID))
  | (DB,(OA|DBM|VALID))
  | (OA,(DBM|VALID))
  | (DBM,VALID)
    -> -1
  | ((DB|OA|DBM|VALID),AF)
  | ((OA|DBM|VALID),DB)
  | ((DBM|VALID),OA)
  | (VALID,DBM)
      -> 1

   let pp = function
     | AF -> "AF"
     | DB -> "DB"
     | DBM -> "DBM"
     | VALID -> "VA"
     | OA -> "OA"
end

module WPTESet = MySet.Make(WPTE)

type atom_pte =
  | Read|ReadAcq|ReadAcqPc
  | Set of  WPTESet.t
  | SetRel of  WPTESet.t

type neon_opt = SIMD.atom

type pair_idx = Both

type atom_acc =
  | Plain of capa_opt | Acq of capa_opt | AcqPc of capa_opt | Rel of capa_opt
  | Atomic of atom_rw | Tag | CapaTag | CapaSeal | Pte of atom_pte | Neon of neon_opt
  | Pair of pair_opt * pair_idx | Instr

let  plain = Plain None

type atom = atom_acc * MachMixed.t option

let default_atom = Atomic PP,None
let instr_atom = Some (Instr,None)

let applies_atom (a,_) d = match a,d with
| Neon SIMD.NeAcqPc,W
| Neon SIMD.NeRel,R -> false
| Acq _,R
| AcqPc _,R
| Rel _,W
| Pte (Read|ReadAcq|ReadAcqPc),R
| Pte (Set _|SetRel _),W
| (Plain _|Atomic _|Tag|CapaTag|CapaSeal|Neon _|Pair _|Instr),(R|W)
  -> true
| _ -> false

let is_ifetch a = match a with
| Some (Instr,_) -> true
| _ -> false

   let pp_plain = "P"
(* Annotation A is taken by load aquire *)
   let pp_as_a = None

   let pp_atom_rw = function
     | PP -> ""
     | PL -> "L"
     | AP -> "A"
     | AL -> "AL"

   let pp_opt = function
     | None -> ""
     | Some Capability -> "c"

   let pp_w_pte ws = WPTESet.pp_str "." WPTE.pp ws

   let pp_atom_pte = function
     | Read -> ""
     | ReadAcq -> "A"
     | ReadAcqPc -> "Q"
     | Set set -> pp_w_pte set
     | SetRel set -> pp_w_pte set ^"L"

   let pp_pair_opt = function
     | Pa -> ""
     | PaN -> "N"
     | PaI -> "I"

   and pp_pair_idx = function
     | Both -> ""

   let pp_atom_acc = function
     | Atomic rw -> sprintf "X%s" (pp_atom_rw rw)
     | Rel o -> sprintf "L%s" (pp_opt o)
     | Acq o -> sprintf "A%s" (pp_opt o)
     | AcqPc o -> sprintf "Q%s" (pp_opt o)
     | Plain o -> sprintf "P%s" (pp_opt o)
     | Tag -> "T"
     | CapaTag -> "Ct"
     | CapaSeal -> "Cs"
     | Pte p -> sprintf "Pte%s" (pp_atom_pte p)
     | Neon n -> SIMD.pp n
     | Pair (opt,idx)
       -> sprintf "Pa%s%s" (pp_pair_opt opt) (pp_pair_idx idx)
     | Instr -> "I"

   let pp_atom (a,m) = match a with
   | Plain o ->
      let prefix = match o with
      | None -> ""
      | Some Capability -> "Pc" in
       begin
         match m with
         | None -> prefix
         | Some m ->
            if String.length prefix > 0
            then sprintf "%s.%s" prefix (Mixed.pp_mixed m)
            else Mixed.pp_mixed m
       end
   | _ ->
     let pp_acc = pp_atom_acc a in
     match m with
     | None -> pp_acc
     | Some m -> sprintf "%s.%s" pp_acc  (Mixed.pp_mixed m)

   let compare_atom = compare

   let equal_atom a1 a2 = a1 = a2

   include
     MachMixed.Util
       (struct
         type at = atom_acc
         let plain = plain
       end)

   let fold_mixed f r =
     if do_mixed then
       Mixed.fold_mixed
         (fun m r -> f (Plain None,Some m) r)
         r
     else
       r

   let fold_all_subsets f =
     let rec fold_rec xs k r = match xs with
       | [] -> if WPTESet.is_empty k then r else f k r
       | x::xs ->
          let r = fold_rec xs (WPTESet.add x k) r in
          fold_rec xs k r in
     fold_rec WPTE.all WPTESet.empty

   let fold_small_subsets f =
     let rec fold_rec xs r =
       match xs with
       | [] -> r
       | x::xs ->
          let sx = WPTESet.singleton x in
          List.fold_right
            (fun y r -> f (WPTESet.add y sx) r)
            xs (f sx (fold_rec xs r)) in
     fold_rec WPTE.all

   let fold_subsets = if do_fullkvm then  fold_all_subsets else fold_small_subsets

   let fold_pte f r =
     if do_kvm then
       let g fs r = f (Set fs) (f (SetRel fs) r) in
       let r = fold_subsets g r in
       f Read (f ReadAcq (f ReadAcqPc r))
     else r

   let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))

   let fold_tag =
     if do_tag then fun f r -> f Tag r
     else fun _f r -> r

   let fold_morello =
     if do_morello then fun f r -> f CapaSeal (f CapaTag r)
     else fun _f r -> r

   let fold_neon =
     if do_neon then
       fun f -> SIMD.fold_neon (fun n -> f (Neon n))
     else
       fun _ r -> r

      let fold_pair f r =
        if do_mixed then r
        else
          let f opt idx r =
            f (Pair (opt,idx)) r in
          r |>
          f Pa Both |>
          f PaN Both |>
          f PaI Both

      let fold_acc_opt o f r =
        let r = f (Acq o) r in
        let r = f (AcqPc o) r in
        let r = f (Rel o) r in
        let r = f Instr r in
        r

   let fold_acc mixed f r =
     let r = if mixed then r else fold_pte (fun p r -> f (Pte p) r) r in
     let r = fold_morello f r in
     let r = fold_tag f r in
     let r = fold_neon f r in
     let r = fold_pair f r in
     let r = fold_acc_opt None f r in
     let r =
       if do_morello then
         let r = f (Plain (Some Capability)) r in
         let r = fold_acc_opt (Some Capability) f r in
         r
       else r in
     let r = fold_atom_rw (fun rw -> f (Atomic rw)) r in
     r

   let fold_non_mixed f r = fold_acc false (fun acc r -> f (acc,None) r) r

   let fold_atom f r =
     let r = fold_non_mixed f r in
     if do_mixed then
       fold_acc true
         (fun acc r -> Mixed.fold_mixed (fun m r -> f (acc,Some m) r) r)
         (Mixed.fold_mixed
            (fun m r -> f (Plain None,Some m) r)
            r)
     else r

   let worth_final (a,_) = match a with
     | Atomic _ -> true
     | Acq _|AcqPc _|Rel _|Plain _|Tag|Instr
     | CapaTag|CapaSeal
     | Pte _|Neon _
     | Pair _
       -> false



   let varatom_dir _d f r = f None r

   let merge_atoms a1 a2 = match a1,a2 with
(* Eat Plain *)
   | ((Plain None,None),a)
   | (a,(Plain None,None)) ->
       Some a
(* Add size to ordinary annotations *)
   | ((Plain None,(Some _ as sz)),
      ((Acq None|AcqPc None|Rel None|Atomic _ as a),None))
   | (((Acq None|AcqPc None|Rel None|Atomic _ as a),None),
      (Plain None,(Some _ as sz)))
     -> Some (a,sz)
(* No sizes for Pte and tags *)
   | (((Pte _|Tag),_),(_,Some _))
   | ((_,Some _),((Pte _|Tag),_)) ->
       None
(* Merge Pte *)
   | ((Pte (Read|ReadAcq),None),((Pte ReadAcq|Acq None),None))
   | (((Acq None|Pte ReadAcq),None),(Pte (Read|ReadAcq),None))
       -> Some (Pte ReadAcq,None)
   | ((Pte (Read|ReadAcqPc),None),((Pte ReadAcqPc|AcqPc None),None))
   | (((Pte ReadAcqPc|AcqPc None),None),(Pte (Read|ReadAcqPc),None))
       -> Some (Pte ReadAcqPc,None)
   | ((Pte (Set set|SetRel set),None),(Rel None,None))
   | ((Rel None,None),(Pte (Set set|SetRel set),None))
       -> Some (Pte (SetRel set),None)
   | (Pte (Set set1),None),(Pte (Set set2),None)
       -> Some (Pte (Set (WPTESet.union  set1 set2)),None)
   | ((Pte (Set set1),None),(Pte (SetRel set2),None))
   | ((Pte (SetRel set1),None),(Pte (Set set2),None))
   | ((Pte (SetRel set1),None),(Pte (SetRel set2),None))
       ->
         Some (Pte (SetRel (WPTESet.union set1 set2)),None)
(* Add size when (ordinary) annotation equal *)
   | ((Acq None as a,None),(Acq None,(Some _ as sz)))
   | ((Acq None as a,(Some _ as sz)),(Acq None,None))
   | ((AcqPc None as a,None),(AcqPc None,(Some _ as sz)))
   | ((AcqPc None as a,(Some _ as sz)),(AcqPc None,None))
   | ((Rel None as a,None),(Rel None,(Some _ as sz)))
   | ((Rel None as a,(Some _ as sz)),(Rel None,None))
   | ((Atomic PP as a,None),(Atomic PP,(Some _ as sz)))
   | ((Atomic PP as a,(Some _ as sz)),(Atomic PP,None))
   | ((Atomic AP as a,None),(Atomic AP,(Some _ as sz)))
   | ((Atomic AP as a,(Some _ as sz)),(Atomic AP,None))
   | ((Atomic PL as a,None),(Atomic PL,(Some _ as sz)))
   | ((Atomic PL as a,(Some _ as sz)),(Atomic PL,None))
   | ((Atomic AL as a,None),(Atomic AL,(Some _ as sz)))
   | ((Atomic AL as a,(Some _ as sz)),(Atomic AL,None))
     -> Some (a,sz)
(* Remove plain when size equal *)
   | ((Plain None,sz1),(a,sz2))
   | ((a,sz1),(Plain None,sz2)) when sz1=sz2 -> Some (a,sz1)
   | _,_ ->
       if equal_atom a1 a2 then Some a1 else None

   let overlap_atoms a1 a2 = match a1,a2 with
     | ((_,None),(_,_))|((_,_),(_,None)) -> true
     | ((_,Some sz1),(_,Some sz2)) ->
         MachMixed.overlap  sz1 sz2

   let neon_as_integers =
     let open SIMD in
     function
     | NeP | NeAcqPc | NeRel -> 1
     | NePa | NePaN -> 2
     | Ne1 -> 4
     | Ne2 | Ne2i -> 8
     | Ne3 | Ne3i -> 12
     | Ne4 | Ne4i -> 16

   let atom_to_bank = function
   | Tag,None -> Code.Tag
   | Pte _,None -> Code.Pte
   | CapaTag,None -> Code.CapaTag
   | CapaSeal,None -> Code.CapaSeal
   | Neon n,None -> Code.VecReg n
   | Pair (_,Both),_ -> Code.Pair
   | Instr,_ -> Code.Instr
   | (Tag|CapaTag|CapaSeal|Pte _|Neon _),Some _ -> assert false
   | (Plain _|Acq _|AcqPc _|Rel _|Atomic _),_
     -> Code.Ord


(**************)
(* Mixed size *)
(**************)

   let tr_value ao v = match ao with
   | None| Some (_,None) -> v
   | Some (_,Some (sz,_)) ->
      Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)

let overwrite_value v ao w = match ao with
| None
| Some
    ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|
    Tag|CapaTag|CapaSeal|Pte _|Neon _|Pair _|Instr),None)
  -> w (* total overwrite *)
| Some ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|Neon _|Instr),Some (sz,o)) ->
   ValsMixed.overwrite_value v sz o w
| Some ((Tag|CapaTag|CapaSeal|Pte _|Pair _),Some _) ->
    assert false

 let extract_value v ao = match ao with
  | None
  | Some
      ((Atomic _|Acq _|AcqPc _|Rel _|Plain _
        |Tag|CapaTag|CapaSeal|Pte _|Neon _|Pair _|Instr),None) -> v
  | Some ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|Tag|CapaTag|CapaSeal|Neon _),Some (sz,o)) ->
     ValsMixed.extract_value v sz o
  | Some ((Pte _|Pair _|Instr),Some _) -> assert false

(* Page table entries *)
  module PteVal = struct

    type pte_atom = atom

    type t = AArch64PteVal.t

    let pp = AArch64PteVal.pp_v

    let default = AArch64PteVal.default

    let compare = AArch64PteVal.compare

    let do_setpteval a f p loc =
      let open AArch64PteVal in
      let fs = match f with
        | Set f|SetRel f -> f
        | Read|ReadAcq|ReadAcqPc ->
           Warn.user_error "Atom %s is not a pteval write" (pp_atom a) in
      WPTESet.fold
        (fun f p ->
          let open WPTE in
          match f with
          | AF -> { p with af = 1-p.af; }
          | DB -> { p with db = 1-p.db; }
          | DBM -> { p with dbm = 1-p.dbm; }
          | VALID -> { p with valid = 1-p.valid; }
          | OA -> { p with oa=OutputAddress.PHY (loc ()); })
      fs p

    let set_pteval a p =
      match a with
      | Pte f,None -> do_setpteval a f p
      | _ -> Warn.user_error "Atom %s is not a pteval write" (pp_atom a)
  end

(* Wide accesses *)

   let as_integers a =
     Misc.seq_opt
       (function
        | Neon n,_ -> (match neon_as_integers n with
                       | 1 -> None
                       | n -> Some n)
        | Pair _,_ -> Some 2
        | _ -> None)
       a

   let is_pair a =
     match a with
     | Some (Pair _,_) -> true
     | Some _|None -> false

(* End of atoms *)

(**********)
(* Fences *)
(**********)

type strength = Strong | Weak
let fold_strength f r = f Strong (f Weak r)
let fold_dirloc f r = f Next (f Prev r)
type sync = Sync | NoSync
type fence = | Barrier of barrier | CacheSync of strength * bool |
               Shootdown of mBReqDomain * TLBI.op * sync | CMO of syncType * dirloc

let is_isync = function
  | Barrier ISB -> true
  | _ -> false

let compare_fence b1 b2 = match b1,b2 with
| (Barrier _,(CacheSync _|Shootdown _))
| (CacheSync _,Shootdown _)
  -> -1
| Barrier b1,Barrier b2 -> barrier_compare b1 b2
| CacheSync (s1,b1) ,CacheSync (s2,b2)->
    begin match compare b1 b2 with
   | 0 -> compare s1 s2
   | r -> r
    end
| Shootdown (dom1,op1,sync1),Shootdown (dom2,op2,sync2) ->
    begin match compare dom1 dom2 with
    | 0 ->
       begin
         match compare op1 op2 with
         | 0 -> compare sync1 sync2
         | r -> r
       end
   | r -> r
    end
| (Shootdown _,(Barrier _|CacheSync _))
| (CacheSync _,Barrier _)
| (CMO _,_) | (_, CMO _)
 -> +1


let default = Barrier (DMB (SY,FULL))
let strong = default

let add_dot f x = match f x with
| "" -> ""
| s -> "." ^ s

let pp_sync = function
  | NoSync -> ""
  | Sync -> "-sync"

let pp_fence f = match f with
| Barrier f -> do_pp_barrier "." f
| CacheSync (s,isb) ->
   sprintf "CacheSync%s%s"
     (match s with Strong -> "Strong" | Weak -> "")
     (if isb then "Isb" else "")
| Shootdown (d,op,sync) ->
   let tlbi = "TLBI" ^ pp_sync sync in
   sprintf "%s%s%s" tlbi
     (add_dot TLBI.short_pp_op op)
     (match sync with
      | NoSync -> ""
      | Sync -> add_dot pp_domain d)
| CMO (t,loc) ->
  sprintf "%s%s"
    (match t with DC_CVAU -> "DC.CVAU" | IC_IVAU -> "IC.IVAU")
    (match loc with Prev -> "p"| Next -> "n")
  

let fold_cumul_fences f k =
   do_fold_dmb_dsb do_kvm C.moreedges (fun b k -> f (Barrier b) k) k

let fold_shootdown =
  if do_kvm then
    let fold_domain =
      if C.moreedges then fold_domain
      else fun f k -> f ISH k
    and fold_op =
      if C.moreedges then TLBI.full_fold_op
      else TLBI.fold_op in
    fun f k ->
      fold_op
        (fun op k ->
          fold_domain
            (fun d k ->
              f (Shootdown(d,op,Sync))
                (f (Shootdown(d,op,NoSync)) k)) k)
        k
  else fun _f k -> k

let fold_cachesync =
  if do_self then
    fun f ->
      Misc.fold_bool
        (fun b k -> fold_strength (fun s k -> f (CacheSync (s,b)) k) k)
  else fun _ k -> k


let fold_cmo f k = fold_dirloc (fun d k -> f (CMO (DC_CVAU,d)) (f (CMO (IC_IVAU,d)) k)) k
  

let fold_all_fences f k =
  let k = fold_shootdown f k in
  let k = fold_cachesync f k in
  let k = fold_cmo f k in
  fold_barrier do_kvm C.moreedges (fun b k -> f (Barrier b) k) k


let fold_some_fences f k =
  let f = fun b k -> f (Barrier b) k in
  let k = f ISB k  in
  let k = f (DMB (SY,FULL)) k in
  let k = f (DMB (SY,ST)) k in
  let k = f (DMB (SY,LD)) k in
  k

let orders f d1 d2 = match f,d1,d2 with
| Barrier ISB,_,_ -> false
| Barrier (DSB (_,FULL)|DMB (_,FULL)),_,_ -> true
| Barrier (DSB (_,ST)|DMB (_,ST)),W,W -> true
| Barrier (DSB (_,ST)|DMB (_,ST)),_,_ -> false
| Barrier (DSB (_,LD)|DMB (_,LD)),Code.R,(W|Code.R) -> true
| Barrier (DSB (_,LD)|DMB (_,LD)),_,_ -> false
| CacheSync _,_,_ -> true
| Shootdown _,_,_ -> false
| CMO _,_,_ -> true

let var_fence f r = f default r

(********)
(* Deps *)
(********)

module D = Dep

type csel = OkCsel|NoCsel

type dp = D.dp * csel

let fold_dpr f r =
  D.fold_dpr
    (fun d r -> f (d,NoCsel) (f (d,OkCsel) r))
    r
let fold_dpw f r =
  D.fold_dpw
    (fun d r -> f (d,NoCsel) (f (d,OkCsel) r))
    r

let pp_ddp =
  let open D in
  function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlIsb"

let pp_dp (d,c) = match c with
  | NoCsel ->  pp_ddp d
  | OkCsel -> pp_ddp d^"Csel"

let lift_dd = Misc.app_opt (fun d -> d,NoCsel)
let ddr_default = lift_dd D.ddr_default
let ddw_default = lift_dd D.ddw_default
let ctrlr_default = lift_dd  D.ctrlr_default
let ctrlw_default = lift_dd  D.ctrlw_default

let lift_pred p (d,_) = p d
let is_ctrlr dc = lift_pred D.is_ctrlr dc
let is_addr dc = lift_pred D.is_addr dc

let fst_dp (d,c) = match c with
  | NoCsel -> List.map (fun d -> (d,NoCsel)) (D.fst_dp d)
  | OkCsel -> []

let sequence_dp (d1,c1) (d2,c2) = match c1 with
  | NoCsel -> List.map (fun d -> d,c2) (D.sequence_dp d1 d2)
  | OkCsel -> []

(* Read-Modify-Write *)
type rmw =  LrSc | LdOp of atomic_op | StOp of atomic_op | Swp | Cas

type rmw_atom = atom (* Enforced by Rmw.S signature *)

let pp_aop op =  Misc.capitalize (Misc.lowercase (pp_aop op))

let pp_rmw compat = function
  | LrSc -> if compat then "Rmw" else "LxSx"
  | Swp -> "Amo.Swp"
  | Cas -> "Amo.Cas"
  | LdOp op -> sprintf "Amo.Ld%s" (pp_aop op)
  | StOp op -> sprintf "Amo.St%s" (pp_aop op)

let is_one_instruction = function
  | LrSc -> false
  | LdOp _ | StOp _ | Swp | Cas -> true

let fold_aop f r =
  let r = f A_ADD r in
  let r = f A_EOR r in
  let r = f A_SET r in
  let r = f A_CLR r in
  r

let fold_rmw f r =
  let r = f LrSc r in
  let r = f Swp r in
  let r = f Cas r in
  let r = fold_aop (fun op r -> f (LdOp op) r) r in
  let r = fold_aop (fun op r -> f (StOp op) r) r in
  r

let fold_rmw_compat f r = f LrSc r

(* Check legal anotation for AMO instructions and LxSx pairs *)

let ok_rw ar aw =
  match ar,aw with
  | (Some ((Acq _|Plain _),_)|None),(Some ((Rel _|Plain _),_)|None)
    -> true
  | _ -> false

let ok_w  ar aw =
  match ar,aw with
  | (Some (Plain _,_)|None),(Some ((Rel _|Plain _),_)|None)
    -> true
  | _ -> false

let same_mixed (a1:atom option) (a2:atom option) =
  let a1 = get_access_atom a1
  and a2 = get_access_atom a2 in
  Misc.opt_eq MachMixed.equal a1 a2

let applies_atom_rmw rmw ar aw = match rmw with
  | LrSc ->
     ok_rw ar aw && (do_cu || same_mixed ar aw)
  | Swp|Cas|LdOp _ ->
     ok_rw ar aw && same_mixed ar aw
  | StOp _ ->
     ok_w ar aw && same_mixed ar aw

let show_rmw_reg = function
| StOp _ -> false
| LdOp _|Cas|Swp|LrSc -> true

type arch_edge = IFF of ie | FIF of ie

let pp_arch_edge = function
  | IFF ie -> sprintf "Iff%s" (pp_ie ie)
  | FIF ie -> sprintf "Fif%s" (pp_ie ie)


let dir_tgt = function
| IFF _ -> R
| FIF _ -> W

let dir_src = function
| IFF _ -> W
| FIF _ -> R

let loc_sd (IFF _|FIF _) = Code.Same

let get_ie e = match e with
| IFF ie|FIF ie -> ie

let fold_edge f r = Code.fold_ie (fun ie r -> f (IFF ie) (f (FIF ie) r)) r


let compute_rmw r old co = match r with
    | LdOp op | StOp op ->
      begin match op with
        | A_ADD -> old + co
        | A_SMAX -> if old > co then old else co
        | A_SMIN -> if old < co then old else co
        | A_EOR -> old lxor co
        | A_SET -> old lor co
        | A_CLR -> old land (lnot co)
    end
    | LrSc | Swp | Cas  -> co

include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg

      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false

      let pp_reg = pp_reg
      let pp_i = pp_i
      let free_registers = allowed_for_symb

      type special = reg
      let specials = vregs
    end)

end
