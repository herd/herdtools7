(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Model options:
  - arch: enforce arch matching, or not.
  - co: co is computed externaly and defined in initial environment.
  - init: add initial write events (default)
  - catdep: cat fill computes dependencies
*)

type t = { arch : Archs.t option ; co : bool ; init : bool ; catdep : bool; }

val pp : t -> string
val default : t
val compat : t

val set_enumco : bool -> t -> t
val set_init : bool -> t -> t
val set_catdep : bool -> t -> t
val set_arch : string -> t -> t
