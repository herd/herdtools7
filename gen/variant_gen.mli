(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
 (* RISCV: tagged accesses as amo's with x0 as arg (load) or result (store) *)
  | AsAmo
  | ConstsInInit
(* Mixed size -> diy specific *)
  | Mixed
(* Lift the default restriction of mixed-size annotation to depth one *)
  | FullMixed
(* Self-modifying code *)
  | Self
(* MTE = Memory tagging *)
  | MemTag

val tags : string list

val parse : string -> t option

val pp : t -> string
