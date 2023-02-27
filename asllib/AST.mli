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

(* -------------------------------------------------------------------------

                                    Utils

   ------------------------------------------------------------------------- *)

type position = Lexing.position
type 'a annotated = { desc : 'a; pos_start : position; pos_end : position }

(* -------------------------------------------------------------------------

                                   Operations

   ------------------------------------------------------------------------- *)

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

(* -------------------------------------------------------------------------

                                Parsed values

   ------------------------------------------------------------------------- *)

(** Main value type, parametric on its base values *)
type value =
  | V_Int of int
  | V_Bool of bool
  | V_Real of float
  | V_BitVector of Bitvector.t
  | V_Tuple of value list
  | V_Record of (identifier * value) list
  | V_Exception of (identifier * value) list

(* -------------------------------------------------------------------------

                                Expressions

   ------------------------------------------------------------------------- *)

(** Expressions. Parametric on the type of literals. *)
type expr_desc =
  | E_Literal of value
  | E_Var of identifier
  | E_Typed of expr * ty
  | E_Binop of binop * expr * expr
  | E_Unop of unop * expr
  | E_Call of identifier * expr list * (identifier * expr) list
  | E_Slice of expr * slice list
  | E_Cond of expr * expr * expr
  | E_GetField of expr * identifier * type_annot
  | E_Record of ty * (identifier * expr) list * type_annot
  | E_Concat of expr list
  | E_Tuple of expr list
  | E_Unknown of ty
  | E_Pattern of expr * pattern

and expr = expr_desc annotated

and pattern =
  | Pattern_All
  | Pattern_Any of pattern list
  | Pattern_Geq of expr
  | Pattern_Leq of expr
  | Pattern_Mask of string
  | Pattern_Not of pattern
  | Pattern_Range of expr * expr (* lower -> upper, included *)
  | Pattern_Single of expr

and slice =
  | Slice_Single of expr
  | Slice_Range of expr * expr (* end first because ASL *)
  | Slice_Length of expr * expr (* start, length *)

(** Type annotations are way for the typing system to annotate
    special nodes of the AST. They are for internal use only. *)
and type_annot = TA_None | TA_InferredStructure of ty
(* -------------------------------------------------------------------------

                                  Types

   ------------------------------------------------------------------------- *)

(** Type descriptors.*)
and type_desc =
  | T_Int of int_constraints option
  | T_Real
  | T_String
  | T_Bool
  | T_Bits of bits_constraint * bitfields option
  | T_Bit
  | T_Enum of identifier list
  | T_Tuple of ty list
  | T_Array of expr * ty
  | T_Record of typed_identifier list
  | T_Exception of typed_identifier list
  | T_ZType of ty
      (** A Z-type correcponds to a type with a possible null value.*)
  | T_Named of identifier  (** A type variable. *)

and ty = type_desc annotated

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
  | BitWidth_ConstrainedFormType of ty
      (** Constrained by the domain of another type. *)
  | BitWidth_Constrained of int_constraints
      (** Constrained directly by a constraint on its width. *)

and bitfields = (slice list * identifier) list

and typed_identifier = identifier * ty
(** An identifier declared with its type. *)

(* -------------------------------------------------------------------------

                        l-expressions and statements

   ------------------------------------------------------------------------- *)

(** Type of left-hand side of assignments. *)
type lexpr_desc =
  | LE_Ignore
  | LE_Var of identifier
  | LE_Typed of lexpr * ty
  | LE_Slice of lexpr * slice list
  | LE_SetField of lexpr * identifier * type_annot
  | LE_TupleUnpack of lexpr list

and lexpr = lexpr_desc annotated

(** Statements. Parametric on the type of literals in expressions. *)
type stmt_desc =
  | S_Pass
  | S_Then of stmt * stmt
  | S_TypeDecl of identifier * ty
  | S_Assign of lexpr * expr
  | S_Call of identifier * expr list * (identifier * expr) list
  | S_Return of expr option
  | S_Cond of expr * stmt * stmt
  | S_Case of expr * case_alt list
  | S_Assert of expr

and stmt = stmt_desc annotated
and case_alt = (pattern * stmt) annotated

(* -------------------------------------------------------------------------

                        Functions and declarations

   ------------------------------------------------------------------------- *)

type 'body func_skeleton = {
  name : identifier;
  parameters : (identifier * ty option) list;
  args : typed_identifier list;
  body : 'body;
  return_type : ty option;
}

type func = stmt func_skeleton
(** Function types in the AST. For the moment, they represent getters, setters,
    functions, procedures and primitives. *)

(** Declarations, ie. top level statement in a asl file. *)
type decl =
  | D_Func of func
  | D_GlobalConst of identifier * ty * expr
  | D_TypeDecl of identifier * ty
  | D_Primitive of func
(* [D_Primitive] is a placeholder for typechecking primitive calls. Only the
   function signature is relevant here. *)

type t = decl list
(** Main AST type. *)
