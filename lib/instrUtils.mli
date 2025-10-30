(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities for instructions *)

module type S =
  sig
    type instr_exec
    val norm_ins : instr_exec -> instr_exec
    val is_valid : instr_exec -> bool
    val get_exported_label : instr_exec -> BranchTarget.t option
  end

module No :
  functor (I : sig type instr end)
  -> S with type instr_exec = I.instr

module WithNop :
  functor
    (I : sig
           type instr
           val nop : instr
           val compare : instr -> instr -> int
         end)
  ->
  sig
    include S with type instr_exec = I.instr
    val nop : I.instr option
    val is_nop : I.instr -> bool
  end

module type Tr =
  sig
    type exec
    type data
    val from_exec : exec -> data
    val to_exec : data -> exec
  end

module IdTr :
  functor (I : sig type instr end)
  ->  Tr with type exec = I.instr and type data = I.instr
