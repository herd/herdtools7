(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Abstract signature of target *)
module type S = sig
  module V : Constant.S
  type arch_reg
  type t

  val get_nrets : t -> int
  val get_nnops : t -> int
  val get_addrs_only : t -> string list
  val get_addrs : t -> string list * string list
  val dump_out_reg : int -> arch_reg -> string
  val addr_cpy_name : string -> int -> string
  val dump_v : V.v -> string
  val dump_init_val : V.v -> string
end
