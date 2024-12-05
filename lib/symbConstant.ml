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

module Make
         (Scalar:Scalar.S)
         (PteVal:PteVal.S)
         (Instr:Instr.S) = struct

  module Scalar = Scalar
  module PteVal = PteVal
  module Instr = Instr

  type v = (Scalar.t,PteVal.t,Instr.t) Constant.t
  open Constant

  let tr c = Constant.map Scalar.of_string PteVal.tr Instr.tr c

  let intToV i = Concrete (Scalar.of_int i)
  and nameToV s = Constant.mk_sym s
  and stringToV s = Concrete (Scalar.of_string s)

  let bit_at k v = Scalar.bit_at k v

  let zero = Concrete Scalar.zero
  and is_zero = function
    | Concrete sc -> Scalar.is_zero sc
    | ConcreteVector _|ConcreteRecord _|Symbolic _
    | Label (_, _)|Tag _|PteVal _|Instruction _|Frozen _
      -> false
  and one = Concrete Scalar.one

  let as_int = function
    | Concrete c ->
       begin
         try Some (Scalar.to_int c)
         with Invalid_argument _ -> None
       end
    | _ -> None

  let pp_instr_cst i = Instr.pp i

  let pp hexa =
    Constant.pp (Scalar.pp hexa) (PteVal.pp hexa) pp_instr_cst
  and pp_unsigned hexa =
    Constant.pp (Scalar.pp_unsigned hexa) (PteVal.pp hexa) pp_instr_cst

  let pp_v = pp false
  let pp_v_old =
    Constant.pp_old (Scalar.pp false) (PteVal.pp false) pp_instr_cst

  let compare c1 c2 =
    Constant.compare Scalar.compare PteVal.compare Instr.compare c1 c2
  let eq c1 c2 = Constant.eq Scalar.equal PteVal.eq Instr.eq c1 c2

(* For building code symbols. *)
  let vToName = function
    | Symbolic s-> Constant.as_address s
    | Concrete _|ConcreteVector _|ConcreteRecord _| Label _|Tag _
    | PteVal _|Instruction _|Frozen _
        -> assert false

  let is_nop = function
    | Instruction i -> Instr.is_nop i
    | Symbolic _|Concrete _|ConcreteRecord _|ConcreteVector _ | Label _|Tag _|PteVal _
    | Frozen _
      -> false

  let access_of_constant =
    function
    | Symbolic (Virtual _) -> Access.VIR
    | Symbolic (Physical _) -> Access.PHY
    | Symbolic (TagAddr _) -> Access.TAG
    | Symbolic (System ((PTE|PTE2),_)) -> Access.PTE
    | Symbolic (System (TLB,_)) -> Access.TLB
    | Label _ -> Access.VIR
    | Tag _
    | ConcreteVector _|Concrete _|ConcreteRecord _
    | PteVal _|Instruction _|Frozen _ as v
      ->
       Warn.fatal "access_of_constant %s as an address"
         (pp_v v) (* assert false *)
end
