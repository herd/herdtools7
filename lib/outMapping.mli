(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Key of mapping in info list *)
val key : string

(* From info list to translation function, if no mapping returns identity *)
val info_to_tr : (string * string) list -> (string -> string)

(* Inverse explicit map *)
val inverse : string StringMap.t -> string StringMap.t

(* Build location map *)
val locmap_inverse :
    string StringMap.t -> MiscParser.location MiscParser.LocMap.t
