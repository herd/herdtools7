(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
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

type 'c t = Constr of 'c | And of 'c t list | Or of 'c t list

val map_constr : ('a -> 'b t) -> 'a t -> 'b t
val fold_map : ('acc -> 'a -> 'acc * 'a) -> 'acc -> 'a t -> 'acc * 'a t
val constr : 'c -> 'c t
val constraints : 'c t -> 'c list
val of_conj_list : 'c list -> 'c t
val pp : (Format.formatter -> 'c -> unit) -> Format.formatter -> 'c t -> unit
