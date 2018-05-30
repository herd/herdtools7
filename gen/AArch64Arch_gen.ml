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
end

module Make
 (C:sig val naturalsize : MachSize.sz val moreedges : bool end) = struct
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
    end)

(* AArch64 has more atoms that others *)
let bellatom = false
type atom_rw =  PP | PL | AP | AL
type atom = Acq | AcqPc | Rel | Atomic of atom_rw | Mixed of MachMixed.t

let default_atom = Atomic PP

let applies_atom a d = match a,d with
| Acq,R
| AcqPc,R
| Rel,W
| Atomic _,(R|W)
| Mixed _,(R|W)
  -> true
| _ -> false

let applies_atom_rmw ar aw = match ar,aw with
| (Some (Acq|AcqPc)|None),(Some Rel|None) -> true
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
     | AcqPc -> "Q"
     | Mixed mix -> Mixed.pp_mixed mix

   let compare_atom = Pervasives.compare

   let fold_mixed f r = Mixed.fold_mixed (fun mix r -> f (Mixed mix) r) r

   let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))

   let fold_non_mixed f r =
     f Acq (f AcqPc (f Rel (fold_atom_rw (fun rw -> f (Atomic rw)) r)))

   let fold_atom f r =
     let r = fold_mixed f r in
     fold_non_mixed f r

   let worth_final = function
     | Atomic _ -> true
     | Acq|AcqPc|Rel|Mixed _ -> false


   let varatom_dir _d f r = f None r

   let tr_value ao v = match ao with
   | None| Some (Acq|AcqPc|Rel|Atomic _) -> v
   | Some (Mixed (sz,_)) -> Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)

let overwrite_value v ao w = match ao with
  | None| Some (Atomic _|Acq|AcqPc|Rel) -> w (* total overwrite *)
  | Some (Mixed (sz,o)) ->
      ValsMixed.overwrite_value v sz o w

 let extract_value v ao = match ao with
  | None| Some (Atomic _|Acq|AcqPc|Rel) -> v
  | Some (Mixed (sz,o)) ->
      ValsMixed.extract_value v sz o

(* End of atoms *)

(**********)
(* Fences *)
(**********)

type fence = barrier

let is_isync = function
  | ISB -> true
  | _ -> false

let compare_fence = barrier_compare

let default = DMB (SY,FULL)
let strong = default

let pp_fence f = do_pp_barrier "." f

let fold_cumul_fences f k = do_fold_dmb_dsb C.moreedges f k

let fold_all_fences f k = fold_barrier  C.moreedges f k

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
