(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Operations that are arch specific *)

module type S = sig
  type extra_op
  type 'a constr_op
  type op = extra_op constr_op
  type extra_op1
  type 'a constr_op1
  type op1 = extra_op1 constr_op1

  val pp_op : op -> string
  val pp_op1 : bool (* hexa *) -> op1 -> string

  type scalar
  type pteval
  type instr
  type cst = (scalar, pteval, instr) Constant.t

  (* Specific operations *)
  val do_op : op -> cst -> cst -> cst option
  val do_op1 : op1 -> cst -> cst option

  (******************************************)
  (* Particular cases of generic operations *)
  (******************************************)

  (* Compute page key *)
  val shift_address_right : string -> scalar -> cst option

  (* Computing on page table entries *)
  val orop : pteval -> scalar -> pteval option
  val andnot2 : pteval -> scalar -> pteval option
  val andop : pteval -> scalar -> scalar option

  (* Masking some structured constant *)
  val mask : cst -> MachSize.sz -> cst option

  (* A type of predicate to represent computation with a result dependent of the
   * satisfaction of a formula, those predicates are added to the constraints of
   * the solver in valconstraint.ml *)
  type predicate
  exception Constraint of predicate * cst * cst

  (* Return if two syntactically different constants may be equal modulo the
   * satisfaction of a predicate *)
  val eq_satisfiable : cst -> cst -> predicate option
  val compare_predicate : predicate -> predicate -> int
  val pp_predicate : predicate -> string
end

type no_predicate
type no_extra_op1
type 'a no_constr_op1
type no_extra_op
type 'a no_constr_op

module No (Cst : Constant.S) :
  S
    with type scalar = Cst.Scalar.t
     and type pteval = Cst.PteVal.t
     and type instr = Cst.Instr.t
     and type extra_op = no_extra_op
     and type 'a constr_op = 'a no_constr_op
     and type extra_op1 = no_extra_op1
     and type 'a constr_op1 = 'a no_constr_op1
     and type predicate = no_predicate
= struct
  type extra_op = no_extra_op
  type 'a constr_op = 'a no_constr_op
  type op = extra_op constr_op
  type extra_op1 = no_extra_op1
  type 'a constr_op1 = 'a no_constr_op1
  type op1 = extra_op1 constr_op1

  let pp_op _ = assert false
  let pp_op1 _hexa _ = assert false

  type scalar = Cst.Scalar.t
  type pteval = Cst.PteVal.t
  type instr = Cst.Instr.t
  type cst = (scalar, pteval, instr) Constant.t

  type predicate = no_predicate
  exception Constraint of predicate * cst * cst

  let do_op _ _ _ = None
  let do_op1 _ _ = None
  let shift_address_right _ _ = None
  let orop _ _ = None
  let andnot2 _ _ = None
  let andop _ _ = None
  let mask _ _ = None

  let eq_satisfiable _ _ = None
  let compare_predicate _ _ = assert false
  let pp_predicate _ = assert false
end

module type S1 = sig
  type extra_op1
  type 'a constr_op1
  type op1 = extra_op1 constr_op1

  val pp_op1 : bool (* hexa *) -> op1 -> string

  type scalar
  type pteval
  type instr
  type cst = (scalar, pteval, instr) Constant.t

  val do_op1 : op1 -> cst -> cst option
  val shift_address_right : string -> scalar -> cst option
  val orop : pteval -> scalar -> pteval option
  val andnot2 : pteval -> scalar -> pteval option
  val andop : pteval -> scalar -> scalar option
  val mask : cst -> MachSize.sz -> cst option
end

module OnlyArchOp1 (A : S1) :
  S
    with type extra_op1 = A.extra_op1
     and type 'a constr_op1 = 'a A.constr_op1
     and type scalar = A.scalar
     and type pteval = A.pteval
     and type instr = A.instr
     and type extra_op = no_extra_op
     and type 'a constr_op = 'a no_constr_op
     and type predicate = no_predicate
= struct
  include A

  type extra_op = no_extra_op
  type 'a constr_op = 'a no_constr_op
  type op = extra_op constr_op

  type predicate = no_predicate
  exception Constraint of predicate * cst * cst
  let compare_predicate _ _ = assert false
  let pp_predicate _ = assert false
  let eq_satisfiable _ _ = None

  let pp_op _ = assert false
  let do_op _ _ _ = None
end
