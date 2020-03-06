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

(* C target, a simplified template *)
type arch_reg = string
module RegMap = StringMap
type ins = unit
type code = string

type t =
  { inputs : (string * CType.t) list ;
    finals : arch_reg list ;
    code : code ; }


let fmt_reg x = x

let dump_out_reg p x =  OutUtils.fmt_out_reg p x

let compile_out_reg proc reg = OutUtils.fmt_index (dump_out_reg proc reg)

let compile_presi_out_reg proc reg =
  OutUtils.fmt_presi_index (dump_out_reg proc reg)

let compile_presi_out_ptr_reg proc reg =
  OutUtils.fmt_presi_ptr_index (dump_out_reg proc reg)

let get_nrets _ = 0

let get_addrs_only t = List.map fst t.inputs
let get_addrs t = get_addrs_only t,[]

let out_code chan code = Printf.fprintf chan "%s\n" code
