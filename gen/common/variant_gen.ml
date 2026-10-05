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
  | KVM | NoFault
(* Synchronisation mode *)
  | Sync | Async | Asym
(* Store-only mode *)
  | StoreOnly
(* Neon AArch64 extension *)
  | Neon
(* Scalable Vector extension (AArch64) *)
  | SVE
(* Scalable Matrix extension (AArch64) *)
  | SME
(* Constrained Unpredictable *)
  | ConstrainedUnpredictable

module Set =
  Set.Make
    (struct
      type elt = t
      type t = elt
      let compare = compare
    end)

type set = Set.t

let empty = Set.empty
let add = Set.add
let remove = Set.remove
let has = Set.mem

let tags =
  ["AsAmo";"ConstsInInit";
   "Mixed";"FullMixed";"MixedDisjoint"; "MixedStrictOverlap";
   "Ifetch(Self)"; "MemTag";
   "NoVolatile"; "Morello"; "VMSA(KVM)"; "NoFault";
   "Sync"; "Async"; "Asym"; "StoreOnly"; "Neon"; "ConstrainedUnpredictable"; ]

let all_t =
  [ AsAmo ; ConstsInInit ; Mixed ; FullMixed ; MixedDisjoint ; MixedStrictOverlap ;
    Self ; MemTag ; NoVolatile ; Morello ; KVM ; NoFault ;
    Sync ; Async ; Asym ; StoreOnly ; Neon ; SVE ; SME ; ConstrainedUnpredictable ]

let parse tag = match Misc.lowercase tag with
| "asamo" -> Some AsAmo
| "constsininit" -> Some ConstsInInit
| "mixed" -> Some Mixed
| "fullmixed" -> Some FullMixed
| "mixeddisjoint"|"disjoint" -> Some MixedDisjoint
| "mixedstrictoverlap"|"strictoverlap" -> Some MixedStrictOverlap
| "ifetch" | "self" -> Some Self
| "memtag" -> Some MemTag
| "novolatile" -> Some NoVolatile
| "morello" -> Some Morello
| "kvm" | "vmsa" -> Some KVM
| "nofault" -> Some NoFault
| "sync" -> Some Sync
| "async" -> Some Async
| "asym" | "asymmetric" -> Some Asym
| "storeonly" | "store-only" -> Some StoreOnly
| "neon" -> Some Neon
| "sve" -> Some SVE
| "sme" -> Some SME
| "constrainedunpredictable"|"cu" -> Some ConstrainedUnpredictable
| _ -> None

let pp = function
  | AsAmo -> "AsAmo"
  | ConstsInInit -> "ConstsInInit"
  | Mixed -> "Mixed"
  | FullMixed -> "FullMixed"
  | MixedDisjoint -> "MixedDisjoint"
  | MixedStrictOverlap -> "MixedStrictOverlap"
  | Self -> "Ifetch"
  | MemTag -> "MemTag"
  | NoVolatile -> "NoVolatile"
  | Morello -> "Morello"
  | KVM -> "VMSA"
  | NoFault -> "NoFault"
  | Sync -> "Sync"
  | Async -> "Async"
  | Asym -> "Asym"
  | StoreOnly -> "StoreOnly"
  | Neon -> "Neon"
  | SVE -> "sve"
  | SME -> "sme"
  | ConstrainedUnpredictable -> "ConstrainedUnpredictable"

let pp_herd_variant = function
  | AsAmo | ConstsInInit | NoVolatile | NoFault -> None
  | Neon -> Some "neon"
  | SVE -> Some "sve"
  | SME -> Some "sme"
  | Mixed | FullMixed | MixedDisjoint | MixedStrictOverlap -> Some "mixed"
  | Self -> Some "ifetch"
  | MemTag -> Some "memtag"
  | Sync -> Some "sync"
  | Async -> Some "async"
  | Asym -> Some "asym"
  | StoreOnly -> Some "store-only"
  | Morello -> Some "morello"
  | KVM  -> Some "vmsa"
  | ConstrainedUnpredictable -> Some "ConstrainedUnpredictable"

let is_mixed variants = has Mixed variants || has FullMixed variants
let is_kvm variants = has KVM variants

let validate variants =
  if List.length
       (List.filter (fun variant -> has variant variants) [Sync; Async; Asym]) > 1
  then
    Warn.user_error
      "variants `Sync`, `Async` and `Asym` are mutually exclusive" ;
  if (has Sync variants || has Async variants || has Asym variants ||
      has StoreOnly variants) && not (has MemTag variants) then
    Warn.user_error
      "variants `Sync`, `Async`, `Asym` and `StoreOnly` require `MemTag`"
