(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Instructions as data *)

module type S = sig
  type exec (* Instruction as instruction *)
  type t    (* Instruction as data *)

  val from_exec : exec -> t
  val to_exec : t -> exec

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val pp : t -> string
  val tr : InstrLit.t -> t
  (* val mk_imm_branch : int -> t option *)

  module Set : MySet.S with type elt = t
end

module No :
  functor (I:sig type instr end)
    -> S
       with type exec = I.instr and type t = I.instr

