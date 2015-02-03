(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* C target, a simplified template *)
type arch_reg = string

type t =
  { inputs : (string * CType.t) list ;
    finals : arch_reg list ;
    code : string ; }

   
let fmt_reg x = x

let dump_out_reg p x =  OutUtils.fmt_out_reg p x

let compile_out_reg proc reg = OutUtils.fmt_index (dump_out_reg proc reg)

let compile_presi_out_reg proc reg =
  OutUtils.fmt_presi_index (dump_out_reg proc reg)

let compile_presi_out_ptr_reg proc reg =
  OutUtils.fmt_presi_ptr_index (dump_out_reg proc reg)

let get_addrs t = List.map fst t.inputs
