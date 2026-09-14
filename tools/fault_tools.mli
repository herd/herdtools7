(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = (Proc.t * string option) * string option * string option

(* Returns true when the argument is prefixed by "D-" or "I-". *)
val has_diprefix : string -> bool

(* Returns argument with the "D-" or "I-" prefix stripped packed in
   [Some], when prefixed. Otherwise, returns [None] *)
val strip_diprefix : string -> string option

(* [match ft0 ft] return true when [f] "matches" [f0] seen as a fault
   specification. *)
val match_fault_type : string option -> string option -> bool

val pp : t -> string
