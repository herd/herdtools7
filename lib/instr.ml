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
  val is_overwritable : t -> bool
  val can_overwrite : t -> bool
  val get_exported_label : t -> Label.t option

  module Set : MySet.S with type elt = t
end

module No (I:sig type instr end) = struct
  type t = I.instr

  let fail msg =
    Warn.fatal "Functionality %s not implemented for -variant self" msg

  let compare _ _ = fail ""
  let eq _ _ = fail ""
  let pp _ = fail ""
  let tr i =
    fail ("litteral instruction " ^ InstrLit.pp i)
  let nop = None
  let is_nop _ = fail "is_nop"
  let is_overwritable _ = false
  let can_overwrite _ = false
  let get_exported_label _ = None

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
         type instr
         val nop : instr
         val compare : instr -> instr -> int
       end) =
  struct
    type t = I.instr

    let fail msg =
      Warn.fatal "Functionality %s not implemented for -variant self" msg

    let compare = I.compare
    let eq i1 i2 = I.compare i1 i2 = 0
    let pp _ = fail ""
    let tr i =
      fail ("litteral instruction " ^ InstrLit.pp i)
    let nop = Some I.nop
    let is_nop i = eq i I.nop
    let is_overwritable _ = false
    let can_overwrite _ = false
    let get_exported_label _ = None

    module Set =
      MySet.Make
        (struct
          type t = I.instr
          let compare = I.compare
        end)
  end
