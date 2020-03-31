(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  type arch_reg
  module RegMap : MyMap.S with type key = arch_reg
  type t

(* Function dump *)
  val dump_fun :
    out_channel ->
    CType.t RegMap.t ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit

  val dump_call :
    string ->
    (CType.t -> string -> string) ->
    out_channel ->
    string ->
    CType.t RegMap.t ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit

(* Inline dump *)
  val dump :
    out_channel ->
    string ->
    CType.t RegMap.t ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit
end
