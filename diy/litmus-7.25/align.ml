(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Align option *)

type t = All | No | Not of StringSet.t


let tags = ["all"; "none"; "<var list>";]

let parse tag = match tag with
| "all" -> Some All
| "none"  -> Some No
|  _ ->
    try
      Some (Not (StringSet.of_list (LexSplit.strings tag)))
    with LexSplit.Error -> None

let pp = function
  | All -> "all"
  | No -> "none"
  | Not s -> StringSet.pp_str "," Misc.identity s
