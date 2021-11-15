(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
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

(** Output Address, ie address in page table entries. *)

(* A distinction is made between physical and PTE addresses *)

type t = PTE of string | PHY of string

(* For hashcode backward compatibility *)
val pp_old : t -> string

(* Standard *)
val pp : t -> string
val parse : string -> t

val compare : t -> t -> int
val eq : t -> t -> bool

val as_physical : t -> string option
val as_pte : t -> string option
val refers_virtual : t -> string option
