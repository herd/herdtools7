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

(** Basic arch, ie with no definition of what a global location is *)

module type Config = ArchExtra_herd.Config


module type S =
  sig

    include ArchBase.S

    module V : Value.S with type Cst.Instr.exec = instruction

    val is_amo : instruction -> bool
    val pp_barrier_short : barrier -> string
    val reject_mixed : bool (* perform a check that rejects mixed-size tests *)
    val mem_access_size : instruction -> MachSize.sz option

    val opt_env : bool (* environemnt optimisation is available *)
    val killed : instruction -> reg list
    val get_lx_sz : instruction -> MachSize.lr_sc
    (* Those register are to be initialised to the default value explicitly *)
    val reg_defaults : reg list


    include ArchExtra_herd.S with module I.V = V
    and type I.arch_reg = reg
    and type I.instr = instruction

(* Levels are abstract, for AArch64, they are E0 to E3 *)
    type level
    val levels : level list
    val pp_level : level -> string

    module TLBI :
    sig
      type op
      val pp_op : op -> string
      val is_at_level : level -> op -> bool
      val inv_all : op -> bool
      val sets : (string * (op -> bool)) list
    end

    val convert_if_imm_branch : int -> int -> int Label.Map.t -> int Label.Map.t -> instruction -> instruction

    module MemType:MemoryType.S

    module Barrier:AllBarrier.S with type a = barrier

    module CMO:Cmo.S

  end
