(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type pc_specific_symb =
  | CodeEnd
  | FaultHandlerEnd
  | FaultInHandler

type 'v t =
  | PCSymb of pc_specific_symb * Proc.t
  | Addr of Proc.t * int
  | Value of 'v

val ignored_pc_specific_symbs : pc_specific_symb list
val pp_prefix : pc_specific_symb -> string
val pp : pc_specific_symb -> Proc.t -> string
val prefixes : string list
