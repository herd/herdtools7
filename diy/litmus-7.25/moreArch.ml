(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Additional arch specification, not so nice... *)

type t = No | ARMv6K | Cheri

let tags = ["armv6k";"cheri"; "none"]

let parse tag = match Misc.lowercase tag with
| "none" -> Some No
| "armv6k" -> Some ARMv6K
| "cheri" -> Some Cheri
| _ -> None

let pp = function
  | No -> "none"
  | ARMv6K -> "ARMv6K"
  | Cheri -> "cheri"
