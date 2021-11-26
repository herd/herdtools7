(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
 (* RISCV: tagged accesses as amo's with x0 as arg (load) or result (store) *)
  | AsAmo
  | ConstsInInit
(* Mixed size (diy only, see alt.ml) *)
  | Mixed
(* Lift the default restriction of mixed-size annotation to depth one *)
  | FullMixed
(* Allow non-overlapping mixed accesses *)
  | MixedDisjoint
(* Require strict overlap *)
  | MixedStrictOverlap
(* Self-modifying code *)
  | Self
(* MTE = Memory tagging *)
  | MemTag
(* C: Prevents the use of Volatile to capture bugs in compilation *)
  | NoVolatile
(* Morello C64 instruction set *)
  | Morello
(* Explicit virtual memory *)
  | KVM
(* Neon AArch64 extension *)
  | Neon
(* Constrained Unpredictable *)
  | ConstrainedUnpredictable

let tags =
  ["AsAmo";"ConstsInInit";
   "Mixed";"FullMixed";"MixedDisjoint"; "MixedStrictOverlap";
   "Self"; "MemTag";
   "NoVolatile"; "Morello"; "kvm"; "Neon"; "ConstrainedUnpredictable"; ]

let parse tag = match Misc.lowercase tag with
| "asamo" -> Some AsAmo
| "constsininit" -> Some ConstsInInit
| "mixed" -> Some Mixed
| "fullmixed" -> Some FullMixed
| "mixeddisjoint"|"disjoint" -> Some MixedDisjoint
| "mixedstrictoverlap"|"strictoverlap" -> Some MixedStrictOverlap
| "self" -> Some Self
| "memtag" -> Some MemTag
| "novolatile" -> Some NoVolatile
| "morello" -> Some Morello
| "kvm" -> Some KVM
| "neon" -> Some Neon
| "constrainedunpredictable"|"cu" -> Some ConstrainedUnpredictable
| _ -> None

let pp = function
  | AsAmo -> "AsAmo"
  | ConstsInInit -> "ConstsInInit"
  | Mixed -> "Mixed"
  | FullMixed -> "FullMixed"
  | MixedDisjoint -> "MixedDisjoint"
  | MixedStrictOverlap -> "MixedStrictOverlap"
  | Self -> "Self"
  | MemTag -> "MemTag"
  | NoVolatile -> "NoVolatile"
  | Morello -> "Morello"
  | KVM -> "kvm"
  | Neon -> "Neon"
  | ConstrainedUnpredictable -> "ConstrainedUnpredictable"
