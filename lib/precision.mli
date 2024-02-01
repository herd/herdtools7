(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Reaction to tag mismatches common to litmus and herd *)

type t =
  | Synchronous  (** The fault raises a synchronous exception. (MTE) *)
  | Asynchronous (** The fault doesn't raise a synchronous exception. (MTE) *)
  | Asymmetric
  (** An attempt to read from a location with a mismatched tag raises
      a synchronous exception and an attempt to write from a location
      with a mismatched tag, records the mismatch and allows the write
      to happen. (MTE) *)

val default : t
val tags : string list
val parse : string -> t option
val pp : t -> string
