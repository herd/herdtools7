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
  | NoInit (* Do not initialise variables *)
  | Pac (* Pointer authentication instructions *)
  | FPac (* Fault on pointer authentication *)
  | ConstPacField (* Bit 55 is used to compute the VA-range in ComputePAC *)

val tags : string list
val parse : string -> t option
val pp : t -> string
val ok : t -> Archs.t -> bool
val compare : t -> t -> int

val set_fault_handling : Fault.Handling.t ref -> t -> bool
val set_mte_precision : Precision.t ref -> t -> bool
val set_mte_store_only : bool ref -> t -> bool

val set_sve_length : int ref -> t -> t option
val set_sme_length : int ref -> t -> t option
val check_tag : t -> t list
