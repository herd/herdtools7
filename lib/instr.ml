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

  type exec
  type t

  val from_exec : exec -> t
  val to_exec : t -> exec

  val nop : t option
  val is_nop : t -> bool

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val pp : t -> string
  val tr : InstrLit.t -> t

  module Set : MySet.S with type elt = t
end

module No (I:sig type instr end) = struct

  type exec = I.instr
  type t = I.instr

  let fail msg =
    Warn.fatal "Instruction as data, functionality %s not implemented" msg

  let from_exec i = i
  let to_exec i = i

  let nop = None
  let is_nop _ = false

  let compare _ _ = fail "compare"
  let eq _ _ = fail "eq"
  let pp _ = fail "pp"
  let tr i =
    fail ("litteral instruction " ^ InstrLit.pp i)

  module Set =
    MySet.Make
      (struct
        type t = I.instr
        let compare = compare
      end)
end

module
  WithNop
    (I:sig
      type instr val nop : instr val compare : instr -> instr -> int
    end) = struct

  type exec = I.instr
  type t = I.instr

  let fail msg =
    Warn.fatal "Instruction as data, functionality %s not implemented" msg

  let from_exec i = i
  let to_exec i = i

  let eq i1 i2 = I.compare i1 i2 = 0
  let nop = Some I.nop
  let is_nop = eq I.nop

  let compare  = I.compare

  let pp _ = fail "pp"
  let tr i =
    fail ("litteral instruction " ^ InstrLit.pp i)

  module Set =
    MySet.Make
      (struct
        type t = I.instr
        let compare = compare
      end)

end
