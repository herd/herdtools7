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

module type S = sig

  type instr_exec

  (* Normalize instructions (for hashes) *)
  val norm_ins : instr_exec -> instr_exec

  (* Check validity of instructions, beyond parsing *)
  val is_valid : instr_exec -> bool

  (* This makes return label that may be used for accessing memory *)
  val get_exported_label : instr_exec -> BranchTarget.t option

end

module No (I:sig type instr end) = struct
  type instr_exec = I.instr

  let norm_ins i = i
  let is_valid _ = true
  let get_exported_label _ = None
end


module
  WithNop
    (I:sig
         type instr
         val nop : instr
         val compare : instr -> instr -> int
       end) =
  struct
    include No(I)
    let nop = Some I.nop
    let is_nop i = I.compare I.nop i = 0
  end

module type Tr = sig
  type exec
  type data

  val from_exec : exec -> data
  val to_exec : data -> exec
end

module IdTr(I:sig type instr end) =
struct
  type exec = I.instr
  type data = I.instr

  let from_exec i = i
  let to_exec i = i
end
