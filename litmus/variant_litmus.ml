(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
  | Self (* Self modifying code *)
  | FaultHandling of Fault.Handling.t
  | S128 (* 128 bit signed ints*)
  | Mixed (* Ignored *)
  | Vmsa  (* Checked *)
  | Telechat (* Telechat idiosyncrasies *)
  | SVE (* Do nothing *)
  | SME (* Do nothing *)
  | MemTag           (* Memory Tagging, synonym of MTE *)
  | MTEPrecision of Precision.t (* MTE tag mismatch handling *)
  | NoInit (* Do not initialise variables *)
  | Pac (* Pointer authentication instructions *)
  | FPac (* Fault on pointer authentication *)
  | ConstPacField (* Bit 55 is used to compute the VA-range in ComputePAC *)

let (mode_variants, arch_variants) : t list * t list =
  let f = function
  | Self -> Self
  | FaultHandling p -> FaultHandling p 
  | S128 -> S128
  | Mixed -> Mixed
  | Vmsa -> Vmsa
  | Telechat -> Telechat
  | SVE -> SVE
  | SME -> SME
  | MemTag -> MemTag
  | MTEPrecision p -> MTEPrecision p
  | NoInit -> NoInit
  | Pac -> Pac
  | FPac -> FPac
  | ConstPacField -> ConstPacField
  in
  let base_modes =
    List.map f [NoInit; S128; Telechat]
  and archs =
    List.map f [SVE; SME; Self; Mixed; Vmsa; Pac; FPac; ConstPacField; MemTag;]
  and mte_precisions =
    List.map (fun precision -> f (MTEPrecision precision)) Precision.all
  and fault_modes =
    List.filter_map
      (fun tag ->
        match Fault.Handling.parse (Misc.lowercase tag) with
        | Some fh -> Some (f (FaultHandling fh))
        | None -> None)
      Fault.Handling.tags
  in
  (base_modes, archs @ mte_precisions @ fault_modes)

let compare = compare

let parse s = match Misc.lowercase s with
| "noinit" -> Some NoInit
| "s128" -> Some S128
| "self" -> Some Self
| "mixed" -> Some Mixed
| "vmsa"|"kvm" -> Some Vmsa
| "telechat" -> Some Telechat
| "sve" -> Some SVE
| "sme" -> Some SME
| "pac" -> Some Pac
| "fpac" -> Some FPac
| "const-pac-field" -> Some ConstPacField
| "memtag" | "mte" -> Some MemTag
| tag when Misc.is_some (Fault.Handling.parse tag) ->
  let fh = Misc.as_some (Fault.Handling.parse tag) in
  Some (FaultHandling fh)
| tag when  Misc.is_some (Precision.parse tag) ->
  let p = Misc.as_some (Precision.parse tag) in
  Some (MTEPrecision p)
| tag ->
  let len = String.length tag in
  if len > 4 then
    let sve = String.sub tag 0 4 = "sve:" in
    let sme = String.sub tag 0 4 = "sme:" in
    if sve || sme then
      Warn.warn_always "Ignoring vector length setting %s" tag ;
    if sve then
      Some SVE
    else if sme then
      Some SME
    else None
  else None

let pp = function
  | NoInit -> "noinit"
  | Self -> "self"
  | Mixed -> "mixed"
  | S128 -> "s128"
  | Vmsa -> "vmsa"
  | Telechat -> "telechat"
  | FaultHandling p -> Fault.Handling.pp p
  | SVE -> "sve"
  | SME -> "sme"
  | Pac -> "pac"
  | FPac -> "fpac"
  | ConstPacField -> "const-pac-field"
  | MemTag -> "memtag"
  | MTEPrecision p -> Precision.pp p

let ok v a = match v,a with
| Self,`AArch64 -> true
| _,_ -> false

let set_fault_handling r = function
| FaultHandling p -> r := p ; true
| _ -> false

let set_mte_precision r = function
  | MTEPrecision p -> r := p; true
  | _ -> false

let set_mte_store_only _ _ = false

let set_sve_length _ _ = None
let set_sme_length _ _ = None
let check_tag tag = [tag]

let mode_tags =
  List.map pp mode_variants

let arch_tags =
  List.concat
    (List.map
       (function
         | MTEPrecision p -> [Precision.pp p; Precision.alias p]
         | v -> [pp v])
       arch_variants)

let tags = 
  mode_tags @ arch_tags


let helper_message =
  Printf.sprintf
    "<tags> mode tags={%s}; arch tags={%s}"
    (String.concat "," mode_tags)
    (String.concat "," arch_tags)
