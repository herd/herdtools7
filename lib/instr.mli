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

module type S = sig
  type t

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val pp : t -> string
  val tr : InstrLit.t -> t
  val nop : t option
  val is_nop : t -> bool

  val can_overwrite : t -> bool
  val get_exported_label : t -> Label.t option

  module Set : MySet.S with type elt = t
end

module No : functor (I:sig type instr end) -> S with type t = I.instr

module WithNop :
functor
  (I:sig
       type instr
       val nop : instr
       val compare : instr -> instr -> int
     end)
-> S with type t = I.instr
