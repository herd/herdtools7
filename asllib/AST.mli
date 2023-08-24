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

(** An Abstract Syntax Tree for ASL. *)

(* -------------------------------------------------------------------------

                                    Utils

   ------------------------------------------------------------------------- *)

(** {2 Utils} *)

type position = Lexing.position
type 'a annotated = { desc : 'a; pos_start : position; pos_end : position }

(* -------------------------------------------------------------------------

                                   Operations

   ------------------------------------------------------------------------- *)

(** {2 Operations} *)

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
  | POW  (** Exponentiation for ints *)
  | RDIV  (** Division for reals *)
  | SHL  (** Shift left for ints *)
  | SHR  (** Shift right for ints *)

type identifier = string
(** Type of local identifiers in the AST. *)

(* -------------------------------------------------------------------------

                                Parsed values

   ------------------------------------------------------------------------- *)

(** {2 Literals}

    Literals are the values written straight into ASL programs.
    There is only literal constructors for a few concepts that could be
    encapsulated into an ASL value.
*)

(** Main value type, parametric on its base values *)
type literal =
  | L_Int of Z.t
  | L_Bool of bool
  | L_Real of Q.t
  | L_BitVector of Bitvector.t
  | L_String of string

(* -------------------------------------------------------------------------

                                Expressions

   ------------------------------------------------------------------------- *)

(** {2 Expressions} *)

(** Expressions. Parametric on the type of literals. *)
type expr_desc =
  | E_Literal of literal
  | E_Var of identifier
  | E_Typed of expr * ty
  | E_Binop of binop * expr * expr
  | E_Unop of unop * expr
  | E_Call of identifier * expr list * (identifier * expr) list
  | E_Slice of expr * slice list
  | E_Cond of expr * expr * expr
  | E_GetArray of expr * expr
  | E_GetField of expr * identifier
  | E_GetFields of expr * identifier list
  | E_Record of ty * (identifier * expr) list
      (** Represents a record or an exception construction expression. *)
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
  | Pattern_Mask of Bitvector.mask
  | Pattern_Not of pattern
  | Pattern_Range of expr * expr (* lower -> upper, included *)
  | Pattern_Single of expr
  | Pattern_Tuple of pattern list

(** Indexes an array, a bitvector. *)
and slice =
  | Slice_Single of expr
      (** [Slice_Single i] is the slice of length [1] at position [i]. *)
  | Slice_Range of expr * expr
      (** [Slice_Range (j, i)] denotes the slice from [i] to [j - 1]. *)
  | Slice_Length of expr * expr
      (** [Slice_Length (i, n)] denotes the slice starting at [i] of length
          [n]. *)
  | Slice_Star of expr * expr
      (** [Slice_Start (factor, length)] denotes the slice starting at [factor
          * length] of length [n]. *)
(** All position mentionned above are included. *)
(* -------------------------------------------------------------------------

                                  Types

   ------------------------------------------------------------------------- *)

(** {2 Types} *)

(** Type descriptors.*)
and type_desc =
  | T_Int of int_constraints option
  | T_Real
  | T_String
  | T_Bool
  | T_Bits of bits_constraint * bitfields
  | T_Enum of identifier list
  | T_Tuple of ty list
  | T_Array of expr * ty
  | T_Record of typed_identifier list
  | T_Exception of typed_identifier list
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
  | BitWidth_SingleExpr of expr  (** Statically evaluable expression. *)
  | BitWidth_ConstrainedFormType of ty
      (** Constrained by the domain of another type. *)
  | BitWidth_Constraints of int_constraints
      (** Constrained directly by a constraint on its width. *)

and bitfields = (identifier * slice list) list

and typed_identifier = identifier * ty
(** An identifier declared with its type. *)

(* -------------------------------------------------------------------------

                        l-expressions and statements

   ------------------------------------------------------------------------- *)

(** {2 Statements} *)

(** Type of left-hand side of assignments. *)
type lexpr_desc =
  | LE_Ignore
  | LE_Var of identifier
  | LE_Slice of lexpr * slice list
  | LE_SetArray of lexpr * expr
  | LE_SetField of lexpr * identifier
  | LE_SetFields of lexpr * identifier list
  | LE_TupleUnpack of lexpr list

and lexpr = lexpr_desc annotated

type local_decl_keyword = LDK_Var | LDK_Constant | LDK_Let

type local_decl_item =
  | LDI_Var of identifier * ty option
  | LDI_Ignore of ty option
  | LDI_Tuple of local_decl_item list * ty option

(** Statements. Parametric on the type of literals in expressions. *)
type for_direction = Up | Down

type version = V0 | V1

type stmt_desc =
  | S_Pass
  | S_Then of stmt * stmt
  | S_Decl of local_decl_keyword * local_decl_item * expr option
  | S_Assign of lexpr * expr * version
  | S_Call of identifier * expr list * (identifier * expr) list
  | S_Return of expr option
  | S_Cond of expr * stmt * stmt
  | S_Case of expr * case_alt list
  | S_Assert of expr
  | S_For of identifier * expr * for_direction * expr * stmt
  | S_While of expr * stmt
  | S_Repeat of stmt * expr
  | S_Throw of (expr * ty option) option
      (** The ty option is a type annotation added by the type-checker to be
          matched later with the catch guards. The bigger option is to
          represent the implicit throw, such as [throw;]. *)
  | S_Try of stmt * catcher list * stmt option
      (** The stmt option is the optional otherwise guard. *)

and stmt = stmt_desc annotated
and case_alt = (pattern * stmt) annotated

and catcher = identifier option * ty * stmt
(** The optional name of the matched exception, the guard type and the
    statement to be executed if the guard matches. *)

(* -------------------------------------------------------------------------

                        Functions and declarations

   ------------------------------------------------------------------------- *)

(** {2 Top-level declarations} *)

type subprogram_type = ST_Procedure | ST_Function | ST_Getter | ST_Setter
type 'p subprogram_body = SB_ASL of stmt | SB_Primitive of 'p

type 'p func = {
  name : identifier;
  parameters : (identifier * ty option) list;
  args : typed_identifier list;
  body : 'p subprogram_body;
  return_type : ty option;
  subprogram_type : subprogram_type;
}
(** Function types in the AST. For the moment, they represent getters, setters,
    functions, procedures and primitives. *)

(** Declaration keyword for global storage elements. *)
type global_decl_keyword = GDK_Constant | GDK_Config | GDK_Let | GDK_Var

type global_decl = {
  keyword : global_decl_keyword;
  name : identifier;
  ty : ty option;
  initial_value : expr option;
}
(** Global declaration type *)

(** Declarations, ie. top level statement in a asl file. *)
type 'p decl_desc =
  | D_Func of 'p func
  | D_GlobalStorage of global_decl
  | D_TypeDecl of identifier * ty * identifier option

type 'p decl = 'p decl_desc annotated

type 'p t = 'p decl list
(** Main AST type. *)

(* -------------------------------------------------------------------------

                              Miscellaneous

   ------------------------------------------------------------------------- *)

type scope =
  | Scope_Local of identifier * int
  | Scope_Global  (** A scope is an unique identifier of the calling site. *)
