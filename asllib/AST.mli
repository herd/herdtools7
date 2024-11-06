(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(** An Abstract Syntax Tree for ASL. *)

(* -------------------------------------------------------------------------

                                    Utils

   ------------------------------------------------------------------------- *)

(** {2 Utils} *)

type position = Lexing.position
type 'a annotated = { desc : 'a; pos_start : position; pos_end : position }

type identifier = string
(** Type of local identifiers in the AST. *)

type uid = int
(** Unique identifiers *)

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
  | DIVRM
      (** Inexact integer division, with rounding towards negative infinity. *)
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

(* -------------------------------------------------------------------------

                                Parsed values

   ------------------------------------------------------------------------- *)

(** {2 Literals}

    Literals are the values written straight into ASL specifications.
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
  | E_ATC of expr * ty  (** Asserted type conversion *)
  | E_Binop of binop * expr * expr
  | E_Unop of unop * expr
  | E_Call of identifier * expr list * (identifier * expr) list
  | E_Slice of expr * slice list
  | E_Cond of expr * expr * expr
  | E_GetArray of expr * expr
  | E_GetField of expr * identifier
  | E_GetFields of expr * identifier list
  | E_GetItem of expr * int
  | E_Record of ty * (identifier * expr) list
      (** Represents a record or an exception construction expression. *)
  | E_Concat of expr list
  | E_Tuple of expr list
  | E_Array of { length : expr; value : expr }
      (** Initial value for an array of size [length] and of content [value] at
          each array cell.

          This expression constructor is only part of the typed AST, i.e. it is
          only built by the type-checker, not any parser.
      *)
  | E_Unknown of ty
  | E_Pattern of expr * pattern

and expr = expr_desc annotated

and pattern_desc =
  | Pattern_All
  | Pattern_Any of pattern list
  | Pattern_Geq of expr
  | Pattern_Leq of expr
  | Pattern_Mask of Bitvector.mask
  | Pattern_Not of pattern
  | Pattern_Range of expr * expr (* lower -> upper, included *)
  | Pattern_Single of expr
  | Pattern_Tuple of pattern list

and pattern = pattern_desc annotated

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
  (* Begin Constrained *)
  | T_Int of constraint_kind
  | T_Bits of expr * bitfield list
  (* End Constrained *)
  | T_Real
  | T_String
  | T_Bool
  | T_Enum of identifier list
  | T_Tuple of ty list
  | T_Array of array_index * ty
  | T_Record of field list
  | T_Exception of field list
  | T_Named of identifier  (** A type variable. *)

and ty = type_desc annotated

(** A constraint on an integer part. *)
and int_constraint =
  | Constraint_Exact of expr
      (** Exactly this value, as given by a statically evaluable expression. *)
  | Constraint_Range of (expr * expr)
      (** In the inclusive range of these two statically evaluable values. *)

(** The constraint_kind constrains an integer type to a certain subset. *)
and constraint_kind =
  | UnConstrained  (** The normal, unconstrained, integer type. *)
  | WellConstrained of int_constraint list
      (** An integer type constrained from ASL syntax: it is the union of each
          constraint in the list. *)
  | Parameterized of uid * identifier
      (** A parameterized integer, the default type for parameters of
          function at compile time, with a unique identifier and the variable
          bearing its name. *)

(** Represent static slices on a given bitvector type. *)
and bitfield =
  | BitField_Simple of identifier * slice list
      (** A name and its corresponding slice *)
  | BitField_Nested of identifier * slice list * bitfield list
      (** A name, its corresponding slice and some nested bitfields. *)
  | BitField_Type of identifier * slice list * ty
      (** A name, its corresponding slice and the type of the bitfield. *)

(** The type of indexes for an array. *)
and array_index =
  | ArrayLength_Expr of expr
      (** An integer expression giving the length of the array. *)
  | ArrayLength_Enum of identifier * int
      (** An enumeration name and its length. *)

and field = identifier * ty
(** A field of a record-like structure. *)

and typed_identifier = identifier * ty
(** An identifier declared with its type. *)

(* -------------------------------------------------------------------------

                        l-expressions and statements

   ------------------------------------------------------------------------- *)

(** {2 Statements} *)

(** Type of left-hand side of assignments. *)
type lexpr_desc =
  | LE_Discard
  | LE_Var of identifier
  | LE_Slice of lexpr * slice list
  | LE_SetArray of lexpr * expr
  | LE_SetField of lexpr * identifier
  | LE_SetFields of lexpr * identifier list * (int * int) list
      (** LE_SetFields (le, fields, _) unpacks the various fields. Third argument is a type annotation. *)
  | LE_Destructuring of lexpr list
  | LE_Concat of lexpr list * expr list option
      (** LE_Concat (les, _) unpacks the various lexpr. Second argument is a type annotation. *)

and lexpr = lexpr_desc annotated

type local_decl_keyword = LDK_Var | LDK_Constant | LDK_Let

(** A left-hand side of a declaration statement. In the following example of a
    declaration statement, [(2, 3, 4): (integer, integer, integer {0..32})] is
    the local declaration item:
    {v
      let (x, -, z): (integer, integer, integer {0..32}) = (2, 3, 4);
    v}
*)
type local_decl_item =
  | LDI_Discard
      (** [LDI_Discard] is the ignored [local_decl_item], for example used in:
          {v let - = 42; v}. *)
  | LDI_Var of identifier
      (** [LDI_Var x] is the variable declaration of the variable [x], used for
          example in: {v let x = 42; v}. *)
  | LDI_Tuple of local_decl_item list
      (** [LDI_Tuple ldis] is the tuple declarations of the items in [ldis],
          used for example in: {v let (x, y, -, z) = (1, 2, 3, 4); v}

          Note that a the list here must be at least 2 items long.
      *)
  | LDI_Typed of local_decl_item * ty
      (** [LDI_Typed (ldi, t)] declares the item [ldi] with type [t]. *)

(** Statements. Parametric on the type of literals in expressions. *)
type for_direction = Up | Down

type version = V0 | V1

type stmt_desc =
  | S_Pass
  | S_Seq of stmt * stmt
  | S_Decl of local_decl_keyword * local_decl_item * expr option
  | S_Assign of lexpr * expr * version
  | S_Call of identifier * expr list * (identifier * expr) list
  | S_Return of expr option
  | S_Cond of expr * stmt * stmt
  | S_Case of expr * case_alt list
  | S_Assert of expr
  | S_For of {
      index_name : identifier;
      start_e : expr;
      dir : for_direction;
      end_e : expr;
      body : stmt;
      limit : expr option;
    }
  | S_While of expr * expr option * stmt
  | S_Repeat of stmt * expr * expr option
  | S_Throw of (expr * ty option) option
      (** The ty option is a type annotation added by the type-checker to be
          matched later with the catch guards. It is always None for the untyped
          AST and never None for the typed AST.
          The outer option is used to represent the implicit throw, such as [throw;]. *)
  | S_Try of stmt * catcher list * stmt option
      (** The stmt option is the optional otherwise guard. *)
  | S_Print of { args : expr list; debug : bool }
      (** A call to print, as an explicit node as it does not require
          type-checking. *)
  | S_Unreachable
      (** The unreachable statement, as an explicit node as it has a specific
          control-flow behaviour. *)

and stmt = stmt_desc annotated
and case_alt_desc = { pattern : pattern; where : expr option; stmt : stmt }
and case_alt = case_alt_desc annotated

and catcher = identifier option * ty * stmt
(** The optional name of the matched exception, the guard type and the
    statement to be executed if the guard matches. *)

(* -------------------------------------------------------------------------

                        Functions and declarations

   ------------------------------------------------------------------------- *)

(** {2 Top-level declarations} *)

type subprogram_type =
  | ST_Procedure
      (** A procedure is a subprogram without return type, called from a
          statement. *)
  | ST_Function
      (** A function is a subprogram with a return type, called from an
          expression. *)
  | ST_Getter
      (** A getter is a special function called with a syntax similar to
          slices. *)
  | ST_EmptyGetter
      (** An empty getter is a special function called with a syntax similar to
          a variable. *)
  | ST_Setter
      (** A setter is a special procedure called with a syntax similar to slice
          assignment. *)
  | ST_EmptySetter
      (** An empty setter is a special procedure called with a syntax similar
          to an assignment to a variable. *)

type subprogram_body = SB_ASL of stmt | SB_Primitive

type func = {
  name : identifier;
  parameters : (identifier * ty option) list;
  args : typed_identifier list;
  body : subprogram_body;
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
type decl_desc =
  | D_Func of func
  | D_GlobalStorage of global_decl
  | D_TypeDecl of identifier * ty * (identifier * field list) option

type decl = decl_desc annotated

type t = decl list
(** Main AST type. *)

(* -------------------------------------------------------------------------

                              Miscellaneous

   ------------------------------------------------------------------------- *)

(** A scope is an unique identifier of the calling site. *)
type scope =
  | Scope_Local of identifier * uid
      (** Local scope of a function given by its name and an uid of the call *)
  | Scope_Global of bool
      (** Global runtime scope, with whether it was during initialization or not *)
