(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Constants as they are parsed, scalars still are strings, as present in file *)

type v = (string,ParsedPteVal.t,InstrLit.t) Constant.t

val zero : v
val one : v
val intToV : int -> v

(* Comparison is used by locations, which should contain symbols only,
   They fails on scalars *)
val compare : v -> v -> int
val eq : v -> v -> bool

val nameToV : string -> v

(* New and old style, id format differ *)
val pp_v : v -> string
val pp_v_old : v -> string

(* Hexa parameter ignored... *)
val pp : bool (* hexa *) -> v -> string

(* Pass specific printer for pteval's *)
val pp_norm : bool -> (ParsedPteVal.t -> string) -> v -> string
