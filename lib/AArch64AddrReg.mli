(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = { oa : OutputAddress.t; f : int; }

val eq : t -> t -> bool
val compare : t -> t -> int

val default : t

val tr : ParsedAddrReg.t -> t
val pp_norm : ParsedAddrReg.t -> string

val pp : 'a -> t -> string
val pp_v : t -> string

val dump_pack : (string -> string) -> t -> string
val fields : string list
val default_fields : string list