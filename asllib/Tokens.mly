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

(* ------------------------------------------------------------------------

                                   Tokens

  ------------------------------------------------------------------------- *)

%token ACCESSOR
%token AND
%token ARBITRARY
%token ARRAY
%token ARROW
%token AS
%token ASSERT
%token BAND
%token BEGIN
%token BEQ
%token BIT
%token BITS
%token BNOT
%token BOOLEAN
%token BOR
%token CASE
%token CATCH
%token COLLECTION
%token COLON
%token COLON_COLON
%token COMMA
%token CONFIG
%token CONSTANT
%token DIV
%token DIVRM
%token DO
%token DOT
%token DOWNTO
%token ELSE
%token ELSIF
%token END
%token ENUMERATION
%token EOF
%token EQ
%token EQ_EQ
%token EXCEPTION
%token FOR
%token FUNC
%token GE
%token GETTER
%token GT
%token IF
%token IMPDEF
%token IMPL
%token IMPLEMENTATION
%token IN
%token INTEGER
%token LBRACE
%token LBRACKET
%token LE
%token LET
%token LLBRACKET
%token LOOPLIMIT
%token LPAR
%token LT
%token MINUS
%token MOD
%token MUL
%token NE
%token NORETURN
%token NOT
%token OF
%token OR
%token OTHERWISE
%token PASS
%token PLUS
%token PLUS_COLON
%token POW
%token PRAGMA
%token PRINT
%token PRINTLN
%token PURE
%token RBRACE
%token RBRACKET
%token RDIV
%token READONLY
%token REAL
%token RECORD
%token RECURSELIMIT
%token REPEAT
%token RETURN
%token RPAR
%token RRBRACKET
%token SEMI_COLON
%token SETTER
%token SHL
%token SHR
%token SLICING
%token STAR_COLON
%token STRING
%token SUBTYPES
%token THEN
%token THROW
%token TO
%token TRY
%token TYPE
%token UNREACHABLE
%token UNTIL
%token VAR
%token WHEN
%token WHERE
%token WHILE
%token WITH
%token XOR

%token DEBUG [@internal true]

%token <string> IDENTIFIER STRING_LIT
%token <Bitvector.mask> MASK_LIT
%token <Bitvector.t> BITVECTOR_LIT
%token <Z.t> INT_LIT
%token <Q.t> REAL_LIT
%token <bool> BOOL_LIT


(* ------------------------------------------------------------------------

                           Associativity and priority

  ------------------------------------------------------------------------- *)

(*
   This section on associativity uses menhir associativity and priority
   features. Internally, it is used by menhir to resolve some conflicts that
   could arrise from different conflicting expressions, e.g. [3 + 4 + 5].

   For a quick intro, menhir assigns a priority level to tokens that have a
   [left], [right], or [nonassoc] declaration in the order in which they are
   declared. For example, here [PLUS]'s associativity is declared before [MUL]
   so [3 + 4 * 5] will be parsed as [3 + (4 * 5)].

   Associativity is straigh-forward.

   Priority declarations that follow are created because of the fusion of
   multiple recursive bnf rules into one, e.g. [expr] is the fusion of [expr]
   and many others such as [cexpr].
   The rule tree that I am translating here into priority rules is the
   following:

     expr <-----------------------|IF|----------------------< cexpr
     cexpr <----|binop_boolean, checked_type_constraint|---<  cexpr_cmp
     cexpr_cmp <-----------|binop_comparison|---------------< cexpr_add_sub
     cexpr_add_sub <------|binop_add_sub_logic|-------------< cexpr_mul_div
     cexpr_mul_div <------|binop_mul_div_shift|-------------< cexpr_pow
     cepxr_pow <---------------|binop_pow|------------------< bexpr
     bexpr <---------------------|unop|---------------------< expr_term
     expr_term <------------------|IN|----------------------< expr_atom
     expr_atom <-----------|DOT, brackets, ...|-------------< expr


  Note that the token MINUS has two different precedence: one for when it is a
  binary operator, in that case it has the same precedence as PLUS, and one for
  when it is a unary operator, in which case it has the same precendence as
  NOT.
*)

(* IF *)
%nonassoc ELSE

(* binop_boolean, checked_type_constraint *)
%left BOR BAND IMPL BEQ AS

(* binop_comparison *)
%left EQ_EQ NE
%nonassoc GT GE LT LE

(* binop_add_sub_logic *)
%left PLUS MINUS OR XOR AND COLON_COLON

(* binop_mul_div_shift *)
%left MUL DIV DIVRM RDIV MOD SHL SHR

(* binop_pow *)
%left POW

(* unop: NOT, BNOT, MINUS *)
%nonassoc UNOPS

(* IN *)
%nonassoc IN

(* DOT, brackets, etc. *)
%left DOT LBRACKET LLBRACKET

%%
