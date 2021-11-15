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

module Make
         (Scalar:Scalar.S)
         (PteVal:PteVal.S) = struct

  module Scalar = Scalar
  module PteVal = PteVal

  type v = (Scalar.t,PteVal.t)  Constant.t
  open Constant

  let tr c = Constant.map Scalar.of_string PteVal.tr c

  let intToV i = Concrete (Scalar.of_int i)
  and nameToV s = Constant.mk_sym s
  and stringToV s = Concrete (Scalar.of_string s)

  let bit_at k v = Scalar.bit_at k v

  let zero = Concrete Scalar.zero
  and one = Concrete Scalar.one

  let pp hexa =  Constant.pp (Scalar.pp hexa) (PteVal.pp hexa)
  and pp_unsigned hexa = Constant.pp (Scalar.pp_unsigned hexa) (PteVal.pp hexa)

  let pp_v = pp false
  let pp_v_old = Constant.pp_old (Scalar.pp false) (PteVal.pp false)

  let compare c1 c2 = Constant.compare Scalar.compare PteVal.compare c1 c2

  let rec eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2 = 0
  | ConcreteVector v1, ConcreteVector v2 ->
      Misc.list_eq eq v1 v2
  | Symbolic s1, Symbolic s2 -> Constant.symbol_eq s1 s2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | PteVal p1,PteVal p2 -> PteVal.eq p1 p2
  | (PteVal _,(Symbolic _|Concrete _|ConcreteVector _|Label _|Tag _))
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|Concrete _|PteVal _))
  | (Concrete _,(Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _))
  | (Symbolic _,(Concrete _|Label _|Tag _|ConcreteVector _|PteVal _))
  | (Label _,(Concrete _|Symbolic _|Tag _|ConcreteVector _|PteVal _))
  | (Tag _,(Concrete _|Symbolic _|Label _|ConcreteVector _|PteVal _))
    -> false

 (* For building code symbols. *)
  let vToName = function
    | Symbolic s-> Constant.as_address s
    | Concrete _|ConcreteVector _ | Label _|Tag _|PteVal _
        -> assert false

end
