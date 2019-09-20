(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = Std | PreSi | Kvm

let tags = ["std"; "presi"; "kvm";]

let parse tag = match Misc.lowercase tag with
| "std" -> Some Std
| "presi" -> Some PreSi
| "kvm" -> Some Kvm
| _ -> None

let pp = function
  | Std -> "std"
  | PreSi -> "presi"
  | Kvm -> "kvm"

let exe = function
  | Std|PreSi -> ".exe"
  | Kvm -> ".flat"
