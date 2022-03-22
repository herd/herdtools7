(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
  { p_oa : OutputAddress.t option;
    p_kv : string StringMap.t;
    p_attrs : StringSet.t; }

(* Buld values *)
val empty : t
val add_oa : OutputAddress.t -> t -> t
val add_oa_if_none : OutputAddress.t -> t -> t
val add_kv : string -> string -> t -> t
val add_attr : string -> t -> t
val add_attrs : string list -> t -> t

val compare : t -> t -> int
val eq : t -> t -> bool

val pp_old : t -> string
val pp : t -> string
val pp_norm : (string StringMap.t -> string StringMap.t) -> t -> string
