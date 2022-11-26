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

(** Values, ie constants and variables *)

module type S =
    sig

(* Constants, notice that they include symbolic "rigid" constants *)
      module Cst : Constant.S
      type arch_op1 (* Arch specific operations *)
      val pp_arch_op1 : bool -> arch_op1 -> string

      type op1_t = arch_op1 Op.op1

(* flexible variables *)
      type csym = int (* Opened by Susmit, lose pointless abstraction *)
      val pp_csym : csym -> string
      val compare_csym : csym -> csym -> int

(* Values, ie constants + variables, that should be instanciated
   to constants later *)
      type v =
        | Var of csym
        | Val of Cst.v

      val pp_v_old  : v -> string
      val pp_v  : v -> string
      val pp : bool (* hexa *) -> v -> string
      val pp_unsigned : bool (* hexa *) -> v -> string

(* produce a fresh variable *)
      val fresh_var : unit -> v
      val from_var : csym -> v
      val as_symbol : v -> string

(* Equality (for constraint solver) is possible *)
      val equalityPossible : v -> v -> bool

(* Please use this for comparing constants... *)
      val compare : v -> v -> int

(* Build constant values, either numerical or symbolic *)
      val intToV  : int -> v
      val stringToV  : string -> v
      val nameToV  : string -> v
      val instructionToV : Cst.Instr.t -> v
      val cstToV : Cst.v -> v
      val maybevToV : MiscParser.maybev -> v

(* Convenience for intToV (0|1) *)
      val zero : v
      val one : v
      val two : v
      val default_tag : v

      (* The following operations may raise
         exception "Undetermined", if their arguments of
         type v are not determined enough to yield a result *)

      exception Undetermined


(* Bit-Twiddling Ops *)
      val bit_at: int -> v -> v

      val is_zero : v -> bool
      val is_one : v -> bool
      val check_ctag : v -> bool
      val is_virtual : v -> bool
      val as_virtual : v -> string option

      val op1 : arch_op1 Op.op1 -> v -> v
      val op : Op.op -> v -> v -> v
      val op3 : Op.op3 -> v -> v -> v -> v

      module ValueSet : MySet.S with type elt = v
      module ValueMap : MyMap.S with type key = v
      module Solution : Map.S with type key = csym

      type solution = v Solution.t

      val is_var_determined : v -> bool
      val undetermined_vars : v -> ValueSet.t
      val simplify_var : solution -> v -> v

(* Convenience, will do nothing if 'v' argument not adequate *)
      val map_const : (Cst.v -> Cst.v) -> v -> v
      val map_scalar : (Cst.Scalar.t -> Cst.Scalar.t) -> v -> v
      val map_csym : (csym -> v) -> v -> v
    end

module type AArch64 = S
  with type Cst.PteVal.t = AArch64PteVal.t
  and type Cst.Instr.t = AArch64Base.instruction
  and type arch_op1 = AArch64Op.t
