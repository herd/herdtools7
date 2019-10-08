(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
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
open Code
open Printf

include AArch64Base

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
type atom_rw =  PP | PL | AP | AL
type atom_acc = Plain | Acq | AcqPc | Rel | Atomic of atom_rw | Tag
type atom = atom_acc * MachMixed.t option

let default_atom = Atomic PP,None

let applies_atom (a,_) d = match a,d with
| Acq,R
| AcqPc,R
| Rel,W
| (Plain|Atomic _|Tag),(R|W)
  -> true
| _ -> false

   let pp_plain = "P"
(* Annotation A is taken by load aquire *)
   let pp_as_a = None

   let pp_atom_rw = function
     | PP -> ""
     | PL -> "L"
     | AP -> "A"
     | AL -> "AL"

   let pp_atom_acc = function
     | Atomic rw -> sprintf "X%s" (pp_atom_rw rw)
     | Rel -> "L"
     | Acq -> "A"
     | AcqPc -> "Q"
     | Plain -> "P"
     | Tag -> "T"

   let pp_atom (a,m) = match a with
   | Plain ->
       begin
         match m with
         | None -> ""
         | Some m -> Mixed.pp_mixed m
       end
   | _ ->
     let pp_acc = pp_atom_acc a in
     match m with
     | None -> pp_acc
     | Some m -> sprintf "%s.%s" pp_acc  (Mixed.pp_mixed m)

   let compare_atom = compare
   let equal_atom a1 a2 = a1 = a2

   let fold_mixed f r =
     Mixed.fold_mixed
       (fun m r -> f (Plain,Some m) r)
       r

   let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))

   let fold_acc f r =
     f Tag (f Acq (f AcqPc (f Rel (fold_atom_rw (fun rw -> f (Atomic rw)) r))))

   let fold_non_mixed f r = fold_acc (fun acc r -> f (acc,None) r) r

   let fold_atom f r =
     fold_acc
       (fun acc r ->
         Mixed.fold_mixed
           (fun m r -> f (acc,Some m) r)
           (f (acc,None) r))
       (fold_mixed f r)

   let worth_final (a,_) = match a with
     | Atomic _ -> true
     | Acq|AcqPc|Rel|Plain|Tag -> false


   let varatom_dir _d f r = f None r

   let merge_atoms a1 a2 = match a1,a2 with
   | ((Plain,sz),(a,None))
   | ((a,None),(Plain,sz)) -> Some (a,sz)
   | ((a1,None),(a2,sz))
   | ((a1,sz),(a2,None)) when a1=a2 -> Some (a1,sz)
   | ((Plain,sz1),(a,sz2))
   | ((a,sz1),(Plain,sz2)) when sz1=sz2 -> Some (a,sz1)
   | _,_ ->
       if equal_atom a1 a2 then Some a1 else None

   let tr_value ao v = match ao with
   | None| Some (_,None) -> v
   | Some (_,Some (sz,_)) -> Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)

let overwrite_value v ao w = match ao with
| None| Some ((Atomic _|Acq|AcqPc|Rel|Plain|Tag),None)
  -> w (* total overwrite *)
| Some ((Atomic _|Acq|AcqPc|Rel|Plain|Tag),Some (sz,o)) ->
    ValsMixed.overwrite_value v sz o w

 let extract_value v ao = match ao with
  | None| Some ((Atomic _|Acq|AcqPc|Rel|Plain|Tag),None) -> v
  | Some ((Atomic _|Acq|AcqPc|Rel|Plain|Tag),Some (sz,o)) ->
      ValsMixed.extract_value v sz o

(* End of atoms *)

(**********)
(* Fences *)
(**********)
type strength = Strong | Weak
let fold_strength f r = f Strong (f Weak r)

type fence = | Barrier of barrier | CacheSync of strength * bool

let is_isync = function
  | Barrier ISB -> true
  | _ -> false

let compare_fence b1 b2 = match b1,b2 with
| Barrier _,CacheSync _ -> -1
| CacheSync (s1,b1) ,CacheSync (s2,b2)->
    begin match compare b1 b2 with
   | 0 -> compare s1 s2
   | r -> r
    end
| Barrier b1,Barrier b2 -> barrier_compare b1 b2
| CacheSync _,Barrier _ -> +1


let default = Barrier (DMB (SY,FULL))
let strong = default

let pp_fence f = match f with
| Barrier f -> do_pp_barrier "." f
| CacheSync (s,isb) -> sprintf "CacheSync%s%s"
      (match s with Strong -> "Strong" | Weak -> "")
      (if isb then "Isb" else "")

let fold_cumul_fences f k =
   do_fold_dmb_dsb C.moreedges (fun b k -> f (Barrier b) k) k

let fold_all_fences f k =
  fold_barrier  C.moreedges (fun b k -> f (Barrier b) k)
    (if do_self then
      Misc.fold_bool
        (fun b k ->
          fold_strength
            (fun s k -> f (CacheSync (s,b)) k)
            k)
        k
    else k)

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

(* Read-Modify-Write *)
type rmw =  LrSc | LdOp of atomic_op | StOp of atomic_op | Swp | Cas

let pp_aop op =  Misc.capitalize (Misc.lowercase (pp_aop op))

let pp_rmw = function
  | LrSc -> ""
  | Swp -> "Swp"
  | Cas -> "Cas"
  | LdOp op -> sprintf "Ld%s" (pp_aop op)
  | StOp op -> sprintf "St%s" (pp_aop op)

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

let applies_atom_rmw rmw ar aw = match rmw,ar,aw with
| (LrSc|Swp|Cas|LdOp _),(Some (Acq,_)|None),(Some (Rel,_)|None)
| (StOp _),None,(Some (Rel,_)|None)
  -> true
| _ -> false

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

end
