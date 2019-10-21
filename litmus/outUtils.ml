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

(* Utilities for targets (Out module) *)

(* Some formating stuff *)
open Printf

let fmt_out_reg proc str = sprintf "out_%i_%s" proc str
let fmt_index name = sprintf "%s[_i]" name
let fmt_presi_index name = sprintf "_log->%s" name
let fmt_presi_ptr_index name = sprintf "_log_ptr->%s" name
let fmt_code p = sprintf "code%i" p
let fmt_prelude p = sprintf "prelude%i" p
let fmt_code_size p = sprintf "code%i_sz" p
let fmt_lbl_offset p lbl  = sprintf "off_P%i_%s" p lbl
let fmt_lbl_var p lbl = sprintf "P%i_%s" p lbl

(* Value (address) output *)
module type Config = sig
  val memory : Memory.t
  val hexa : bool
end

module DefaultConfig = struct
  let memory = Memory.Direct
  let hexa = false
end

module Make(O:Config)(V:Constant.S) = struct
  open Memory
  open Constant

  let dump_addr a = match O.memory with
  | Direct -> sprintf "&_a->%s[_i]" a
  | Indirect -> sprintf "_a->%s[_i]" a

  let dump_v v = match v with
  | Concrete _ -> V.pp O.hexa v
  | Symbolic ((a,None),0) -> dump_addr a
  | Tag _
  | Symbolic _
  | Label _ -> assert false 

  let addr_cpy_name s p = sprintf "_addr_%s_%i" s p
end
