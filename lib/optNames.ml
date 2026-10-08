(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let rename = ref []
and select = ref []
and names = ref []
and oknames = ref StringSet.empty
and excl = ref []
and nonames = ref StringSet.empty


let parse_noselect =
  let open CheckName in
  [
    parse_names names;
    parse_oknames oknames;
    parse_excl excl;
    parse_nonames nonames;
  ]

let parse_noselect_rename = parse_noselect @ [CheckName.parse_rename rename]

let parse_withselect = CheckName.parse_select select::parse_noselect

let parse_withselect_rename = parse_withselect @ [CheckName.parse_rename rename]
