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

(* Value (address) output *)
module type Config = sig
  val memory : Memory.t
end

module Make(O:Config) = struct
  open Memory
  open Constant

  let dump_addr a = match O.memory with
  | Direct -> sprintf "&_a->%s[_i]" a
  | Indirect -> sprintf "_a->%s[_i]" a
                
  let dump_v v = match v with
  | Concrete i -> sprintf "%i" i
  | Symbolic a -> dump_addr a

  let addr_cpy_name s p = sprintf "_addr_%s_%i" s p
end
