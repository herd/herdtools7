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

type bitvector = string
(** Type of bitvector string as just parsed *)

(** Main value type, parametric on its base values *)
type value =
  | V_Int of int
  | V_Bool of bool
  | V_Real of float
  | V_BitVector of bitvector
  | V_Tuple of value list
  | V_Record of (identifier * value) list
  | V_Exception of (identifier * value) list

(** Expressions. Parametric on the type of literals. *)
type expr =
  | E_Literal of value
  | E_Var of identifier
  | E_Binop of binop * expr * expr
  | E_Unop of unop * expr
  | E_Call of identifier * expr list
  | E_Slice of expr * slice list
  | E_Cond of expr * expr * expr
  | E_GetField of expr * identifier * type_annot
  | E_Record of type_desc * (identifier * expr) list * type_annot

and slice =
  | Slice_Single of expr
  | Slice_Range of expr * expr (* end first because ASL *)
  | Slice_Length of expr * expr (* start, length *)

(** Type annotations are way for the typing system to annotate
    special nodes of the AST. They are for internal use only. *)
and type_annot = TA_None | TA_InferredStructure of type_desc

(** Type descriptors.*)
and type_desc =
  | T_Int of int_constraints option
  | T_Real
  | T_String
  | T_Bool
  | T_Bits of bits_constraint
  | T_Bit
  | T_Enum of identifier list
  | T_Tuple of type_desc list
  | T_Array of expr * type_desc
  | T_Record of typed_identifier list
  | T_Exception of typed_identifier list
  | T_ZType of type_desc
      (** A Z-type correcponds to a type with a possible null value.*)
  | T_Named of identifier  (** A type variable. *)

(** A constraint on an integer part. *)
and int_constraint =
  | Constraint_Exact of expr
      (** Exactly this value, as given by a statically evaluable expression. *)
  | Constraint_Range of (expr * expr)
      (** In the range of these two statically evaluable values.*)

and int_constraints = int_constraint list
(** The int_constraints represent the union of the individual constraints.*)

(** The width of a bitvector can be constrained in multiple ways. *)
and bits_constraint =
  | BitWidth_Determined of expr  (** Statically evaluable expression. *)
  | BitWidth_ConstrainedFormType of type_desc
      (** Constrained by the domain of another type. *)
  | BitWidth_Constrained of int_constraints
      (** Constrained directly by a constraint on its width. *)

and typed_identifier = identifier * type_desc
(** An identifier declared with its type. *)

(** Type of left-hand side of assignments. *)
type lexpr =
  | LE_Var of identifier
  | LE_Slice of lexpr * slice list
  | LE_SetField of lexpr * identifier * type_annot

(** Statements. Parametric on the type of literals in expressions. *)
type stmt =
  | S_Pass
  | S_Then of stmt * stmt
  | S_Assign of lexpr * expr
  | S_Call of identifier * expr list
  | S_Return of expr list
  | S_Cond of expr * stmt * stmt
  | S_Case of expr * case_alt list

and case_alt = expr list * stmt

type func = {
  name : identifier;
  args : typed_identifier list;
  body : stmt;
  return_type : type_desc option;
}
(** Function types in the AST. For the moment, they represent getters, setters,
    functions, procedures and primitives. *)

(** Declarations, ie. top level statement in a asl file. *)
type decl =
  | D_Func of func
  | D_GlobalConst of identifier * expr
  | D_TypeDecl of identifier * type_desc
  | D_Primitive of func
(* [D_Primitive] is a placeholder for typechecking primitive calls. Only the
   function signature is relevant here. *)

type t = decl list
(** Main AST type. *)
