(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

(** The main types of this modules are parametric in function of the runtime
    value types ['v]. Types are considered determined at compile time, and thus
    are not dependent on this value type. *)

(** Operations on base value of arity one. *)
type unop =
  | BNOT  (** Boolean inversion *)
  | NEG  (** Integer or real negation *)
  | NOT  (** Bitvector bitwise inversion *)

(** Operations on base value of arity two. *)
type binop =
  | AND  (** Bitvector bitwise and *)
  | BAND  (** Boolean and *)
  | BEQ  (** Boolean equivalence *)
  | BOR  (** Boolean or *)
  | DIV  (** Integer division *)
  | EOR  (** Bitvector bitwise exclusive or *)
  | EQ_OP  (** Equality on two base values of same type *)
  | GT  (** Greater than for int or reals *)
  | GEQ  (** Greater or equal for int or reals *)
  | IMPL  (** Boolean implication *)
  | LT  (** Less than for int or reals *)
  | LEQ  (** Less or equal for int or reals *)
  | MOD  (** Remainder of integer division *)
  | MINUS  (** Substraction for int or reals or bitvectors *)
  | MUL  (** Multiplication for int or reals or bitvectors *)
  | NEQ  (** Non equality on two base values of same type *)
  | OR  (** Bitvector bitwise or *)
  | PLUS  (** Addition for int or reals or bitvectors *)
  | RDIV  (** Division for reals *)
  | SHL  (** Shift left for ints *)
  | SHR  (** Shift right for ints *)

type identifier = string
(** Type of local identifiers in the AST. *)

(** Main value type, parametric on its base values *)
type ('i, 'b, 'r, 'bv) value =
  | V_Int of 'i
  | V_Bool of 'b
  | V_Real of 'r
  | V_BitVector of 'bv

type parsed_value = (int, bool, float, string) value
(** Type of parsed values by the module Parser.mly *)

(** Expressions. Parametric on the type of literals. *)
type 'v expr =
  | E_Literal of 'v
  | E_Var of identifier
  | E_Binop of binop * 'v expr * 'v expr
  | E_Unop of unop * 'v expr
  | E_Call of identifier * 'v expr list
  | E_Get of identifier * 'v expr list
  | E_Cond of 'v expr * 'v expr * 'v expr

type parsed_expr = parsed_value expr

type type_desc =
  | T_Int of int_constraints option
  | T_Real
  | T_String
  | T_Bool
  | T_Bits of bits_constraint
  | T_Bit
  | T_Enum of enum_type_desc
  | T_Tuple of type_desc list
  | T_Array of parsed_expr * type_desc
  | T_Record of record_type_desc
  | T_Exception of record_type_desc
  | T_ZType of type_desc
  | T_Named of identifier

and enum_type_desc = identifier list
and record_type_desc = (identifier * type_desc) list

and int_constraint =
  | Constraint_Exact of parsed_expr
  | Constraint_Range of (parsed_expr * parsed_expr)

and int_constraints = int_constraint list

and bits_constraint =
  | BitWidth_Determined of parsed_expr
  | BitWidth_ConstrainedFormType of type_desc
  | BitWidth_Constrained of int_constraints

and typed_identifier = identifier * type_desc

(** Type of left-hand side of assignments. *)
type 'v lexpr = LEVar of identifier | LESet of identifier * 'v expr list

(** Statements. Parametric on the type of literals in expressions. *)
type 'v stmt =
  | S_Pass
  | S_Then of 'v stmt * 'v stmt
  | S_Assign of 'v lexpr * 'v expr
  | S_Call of identifier * 'v expr list
  | S_Return of 'v expr list
  | S_Cond of 'v expr * 'v stmt * 'v stmt

type 'v func = {
  name : identifier;
  args : typed_identifier list;
  body : 'v stmt;
  return_type : type_desc option;
}
(** Declared functions. Parametric on the type of literals in the body. *)

(** Declarations, ie. top level statement in a asl file. *)
type 'v decl =
  | D_Func of 'v func
  | D_GlobalConst of identifier * 'v expr
  | D_TypeDecl of identifier * type_desc

type 'v t = 'v decl list
(** Main AST type. *)

type parsed_t = parsed_value t
(** Type of parsed ast by the module Parser.mly *)
