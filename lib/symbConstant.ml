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

  let eq c1 c2 = Constant.eq Scalar.equal PteVal.eq c1 c2
               
 (* For building code symbols. *)
  let vToName = function
    | Symbolic s-> Constant.as_address s
    | Concrete _|ConcreteVector _ | Label _|Tag _|PteVal _|Instruction _
        -> assert false

end
