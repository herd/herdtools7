(****************************************************************************)
(*                           the diy toolsuite                              *)
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

(** Command line parsing for selection of tests by name *)

(* Destinaion of options arguments *)
val rename : string list ref
val select : string list ref
val names : string list ref
val oknames : StringSet.t ref
val excl : string list ref
val nonames : StringSet.t ref

(* Command line options specifications *)
val parse_noselect : (Arg.key * Arg.spec * Arg.doc) list
val parse_withselect : (Arg.key * Arg.spec * Arg.doc) list
