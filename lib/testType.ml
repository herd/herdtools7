(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Type as specified in tests *)

open Printf

type t =
  | TyDef | TyDefPointer
  | Ty of string | Pointer of string
  | TyArray of string * int
  | Atomic of string

let pp = function
  | TyDef -> "TyDef"
  | TyDefPointer -> "TyDefPointer"
  | Ty s -> sprintf "Ty<%s>" s
  | Atomic s -> sprintf "Atomic<%s>" s
  | Pointer s -> sprintf "Pointer<%s>" s
  | TyArray (s,sz) -> sprintf "TyArray<%s,%i>" s sz
