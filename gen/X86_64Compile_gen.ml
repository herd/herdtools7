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

open Code

module Make(C:CompileCommon.Config) : XXXCompile_gen.S =

struct

  module X86_64 = X86_64Arch_gen
  include CompileCommon.Make(C)(X86_64)

(******)
  let ppo _f k = k
(******)

  open X86_64

  let next_reg x = alloc_reg x

  let emit_store addr v = assert false

  let emit_sta addr r v = assert false

  let emit_load_ins addr r = Warn.fatal "Loop observers not implemented for X86_64"

  and emit_cmp_zero_ins r =
    Warn.fatal "Loop observers not implemented for X86_64"

  and emit_cmp_one_ins r =
    Warn.fatal "Loop observers not implemented for X86_64"

  and emit_cmp_int_ins r i =
    Warn.fatal "Loop observers not implemented for X86_64"

  and emit_je_ins lab =
    Warn.fatal "Loop observers not implemented for X86_64"

  and emit_jne_ins lab =
    Warn.fatal "Loop observers not implemented for X86_64"

  let pseudo = List.map (fun i -> X86_64.Instruction i)

  let emit_load st _p init x =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_load_not_zero st _p init x =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_load_one st _p init x =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_load_not  _st _p _init _x _cmp =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_load_not_eq  st =  emit_load_not st
  let emit_load_not_value  st = emit_load_not st

let emit_joker st init =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_access st _p init e =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_access_dep _st _p _init _e _r1 =
    Warn.fatal "Dependent access is irrelevant for X86_64"

  let emit_exch_dep _st =
    Warn.fatal "Dependent access is irrelevant for X86_64"

  let emit_rmw () st p init er ew  =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_rmw_dep () =
    Warn.fatal "Loop observers not implemented for X86_64"

  let emit_fence _ _ _ =
    Warn.fatal "Loop observers not implemented for X86_64"

  let full_emit_fence = GenUtils.to_full emit_fence

  let stronger_fence = MFence

(* Check load *)
  let do_check_load p st r e =
    Warn.fatal "Loop observers not implemented for X86_64"

  let check_load  p r e init st =
    Warn.fatal "Loop observers not implemented for X86_64"

(* Postlude *)

  let does_jump lab cs =
    Warn.fatal "Loop observers not implemented for X86_64"

  let does_exit p cs st =
    Warn.fatal "Loop observers not implemented for X86_64"

    let list_of_exit_labels p st =
    Warn.fatal "Loop observers not implemented for X86_64"

  let postlude st p init cs =
    Warn.fatal "Loop observers not implemented for X86_64"

  let get_xstore_results _ =
    Warn.fatal "Loop observers not implemented for X86_64"

end
