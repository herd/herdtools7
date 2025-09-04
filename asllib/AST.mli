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

type version = V0 | V1
type position = Lexing.position

type 'a annotated = {
  desc : 'a;
  pos_start : position;
  pos_end : position;
  version : version;
}

type identifier = string
(** Type of local identifiers in the AST. *)

type uid = int
(** Unique identifiers *)

type delayed_warning = unit -> unit
(** Represents a delayed warning by a continuation that will print the warning
    message. *)

(** Indicates if any precision loss occurred. *)
type precision_loss_flag =
  | Precision_Full  (** No loss of precision *)
  | Precision_Lost of delayed_warning list
      (** A loss of precision comes with a list of warnings that can explain why the loss of precision happened. *)

(* -------------------------------------------------------------------------

                                   Operations

   ------------------------------------------------------------------------- *)

(** {2 Operations} *)

(** Operations on base value of arity one. *)
type unop =
  | BNOT  (** Boolean inversion *)
  | NEG  (** Integer or real negation *)
  | NOT  (** Bitvector bitwise inversion *)

type binop =
  [ `AND  (** Bitvector bitwise and *)
  | `BAND  (** Boolean and *)
  | `BEQ  (** Boolean equivalence *)
  | `BOR  (** Boolean or *)
  | `DIV  (** Integer division *)
  | `DIVRM
    (** Inexact integer division, with rounding towards negative infinity. *)
  | `XOR  (** Bitvector bitwise exclusive or *)
  | `EQ  (** Equality on two base values of same type *)
  | `GT  (** Greater than for int or reals *)
  | `GE  (** Greater or equal for int or reals *)
  | `IMPL  (** Boolean implication *)
  | `LT  (** Less than for int or reals *)
  | `LE  (** Less or equal for int or reals *)
  | `MOD  (** Remainder of integer division *)
  | `SUB  (** Subtraction for int or reals or bitvectors *)
  | `MUL  (** Multiplication for int or reals or bitvectors *)
  | `NE  (** Non equality on two base values of same type *)
  | `OR  (** Bitvector bitwise or *)
  | `ADD  (** Addition for int or reals or bitvectors *)
  | `POW  (** Exponentiation for ints *)
  | `RDIV  (** Division for reals *)
  | `SHL  (** Shift left for ints *)
  | `SHR  (** Shift right for ints *)
  | `BV_CONCAT  (** Bit vector concatenation *)
  | `STR_CONCAT  (** String concatenation *)
  | `BIC  (** Bit clear operation: bitwise and with second argument inverted *)
  ]
(** Operations on base value of arity two. *)

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
  | L_Label of string  (** An enumeration label, given by its name. *)

(* -------------------------------------------------------------------------

                                Expressions

   ------------------------------------------------------------------------- *)

(** {2 Expressions} *)

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
          a variable. This is relevant only for V0. *)
  | ST_Setter
      (** A setter is a special procedure called with a syntax similar to slice
          assignment. *)
  | ST_EmptySetter
      (** An empty setter is a special procedure called with a syntax similar
          to an assignment to a variable. This is relevant only for V0. *)

(** Expressions. Parametric on the type of literals. *)
type expr_desc =
  | E_Literal of literal
  | E_Var of identifier
  | E_ATC of expr * ty  (** Asserted type conversion *)
  | E_Binop of binop * expr * expr
  | E_Unop of unop * expr
  | E_Call of call
  | E_Slice of expr * slice list
  | E_Cond of expr * expr * expr
  | E_GetArray of expr * expr
      (** [E_GetArray base index] Represents an access to an array given
        by the expression [base] at index [index].
        When this node appears in the untyped AST, the index may either
        be integer-typed or enumeration-typed.
        When this node appears in the typed AST, the index can only be
        integer-typed.
    *)
  | E_GetEnumArray of expr * expr
      (** Access an array with an enumeration index.
        This constructor is only part of the typed AST.
    *)
  | E_GetField of expr * identifier
  | E_GetFields of expr * identifier list
  | E_GetCollectionFields of identifier * identifier list
  | E_GetItem of expr * int
  | E_Record of ty * (identifier * expr) list
      (** Represents a record or an exception construction expression. *)
  | E_Tuple of expr list
  | E_Array of { length : expr; value : expr }
      (** Initial value for an array of size [length] and of content [value] at
          each array cell.

          This expression constructor is only part of the typed AST, i.e. it is
          only built by the type-checker, not any parser.
      *)
  | E_EnumArray of { enum : identifier; labels : identifier list; value : expr }
      (** Initial value for an array where the index is the enumeration [enum],
          which declares the list of labels [labels],
          and the content of each cell is given by [value].
          [enum] is only used for pretty-printing.

          This expression constructor is only part of the typed AST, i.e. it is
          only built by the type-checker, not any parser.
      *)
  | E_Arbitrary of ty
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

(** Slices define lists of indices into arrays and bitvectors. *)
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
(** All positions mentioned above are inclusive. *)

and call = {
  name : identifier;
  params : expr list;
  args : expr list;
  call_type : subprogram_type;
}
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
  | T_Collection of field list
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
  | WellConstrained of int_constraint list * precision_loss_flag
      (** An integer type constrained from ASL syntax: it is the union of each
          constraint in the list. *)
  | PendingConstrained
      (** An integer type whose constraint will be inferred during type-checking. *)
  | Parameterized of identifier
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
  | ArrayLength_Enum of identifier * identifier list
      (** An enumeration name and its list of labels. *)

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
      (** [LE_SetArray base index] represents a write to an array given
        by the expression [base] at index [index].
        When this node appears in the untyped AST, the index may either
        be integer-typed or enumeration-typed.
        When this node appears in the typed AST, the index can only be
        integer-typed.
    *)
  | LE_SetEnumArray of lexpr * expr
      (** Represents a write to an array with an enumeration index.
        This constructor is only part of the typed AST.
    *)
  | LE_SetField of lexpr * identifier
  | LE_SetFields of lexpr * identifier list * (int * int) list
      (** [LE_SetFields (le, fields, _)] unpacks the various fields. Third
          argument is a type annotation. *)
  | LE_SetCollectionFields of identifier * identifier list * (int * int) list
      (** [LE_SetCollectionFields (x, fields, _)] unpacks the various fields.
          Third argument is a type annotation. *)
  | LE_Destructuring of lexpr list

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
  | LDI_Var of identifier
      (** [LDI_Var x] is the variable declaration of the variable [x], used for
          example in: {v let x = 42; v}. *)
  | LDI_Tuple of identifier list
      (** [LDI_Tuple names] is the tuple declarations of [names], for example:
          {v let (x, y, z) = (1, 2, 3); v}
          We expect the list to contain at least 2 items.
      *)

(** Statements. Parametric on the type of literals in expressions. *)
type for_direction = Up | Down

type stmt_desc =
  | S_Pass
  | S_Seq of stmt * stmt
  | S_Decl of local_decl_keyword * local_decl_item * ty option * expr option
  | S_Assign of lexpr * expr
  | S_Call of call
  | S_Return of expr option
  | S_Cond of expr * stmt * stmt
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
  | S_Print of { args : expr list; newline : bool; debug : bool }
      (** A call to print, as an explicit node as it does not require
          type-checking.

          [newline] indicates if the print statement should add an extra new
          line after printing all the arguments.
          [debug] indicates if the print statement has been made using the
          ASLRef specific function [__debug].
      *)
  | S_Unreachable
      (** The unreachable statement, as an explicit node as it has a specific
          control-flow behaviour. *)
  | S_Pragma of identifier * expr list
      (** A pragma statement, as an explicit node to be used by tools which need
          AST level hints. *)

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

(** Represents the different types of subprogram bodies. *)
type subprogram_body =
  | SB_ASL of stmt  (** A normal body of a subprogram *)
  | SB_Primitive of bool  (** Whether or not this primitive is side-effecting *)

type func_qualifier =
  | Pure
      (** A `pure` subprogram does not read or modify mutable state. It can be
          called in types. *)
  | Readonly
      (** A `readonly` subprogram can read mutable state but not modify it. It
          can be called in assertions. *)
  | Noreturn
      (** A `noreturn` subprogram always terminates by a thrown exception
          or calling `Unreachable`. *)

type override_info =
  | Impdef  (** A function which can be overridden *)
  | Implementation
      (** A function which overrides a corresponding [Impdef] function *)

type func = {
  name : identifier;
  parameters : (identifier * ty option) list;
  args : typed_identifier list;
  body : subprogram_body;
  return_type : ty option;
  subprogram_type : subprogram_type;
  recurse_limit : expr option;
  qualifier : func_qualifier option;
  override : override_info option;
  builtin : bool;
      (** Builtin functions are treated specially when checking parameters at
          call sites - see [Typing.insert_stdlib_param]. *)
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
  | D_Pragma of identifier * expr list
      (** A global pragma, as an explicit node to be used by tools which need
          AST level hints. *)

type decl = decl_desc annotated

type t = decl list
(** Main AST type. *)
