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

let compare = compare

let tags = "s128"::"self"::"mixed"::"vmsa"::"telechat"::Fault.Handling.tags

let parse s = match Misc.lowercase s with
| "s128" -> Some S128
| "self" -> Some Self
| "mixed" -> Some Mixed
| "vmsa"|"kvm" -> Some Vmsa
| "telechat" -> Some Telechat
| tag ->
   Misc.app_opt (fun p -> FaultHandling p) (Fault.Handling.parse tag)

let pp = function
  | Self -> "self"
  | Mixed -> "mixed"
  | S128 -> "s128"
  | Vmsa -> "vmsa"
  | Telechat -> "telechat"

  | FaultHandling p -> Fault.Handling.pp p

let ok v a = match v,a with
| Self,`AArch64 -> true
| _,_ -> false

let set_fault_handling r = function
| FaultHandling p -> r := p ; true
| _ -> false

let set_mte_precision _ _ = assert false
