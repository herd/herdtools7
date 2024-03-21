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

open AST

(** Error handling for {!Asllib}. *)

type error_desc =
  | BadField of string * ty
  | MissingField of string list * ty
  | BadSlices of slice list * int
  | TypeInferenceNeeded
  | UndefinedIdentifier of identifier
  | MismatchedReturnValue of string
  | BadArity of identifier * int * int
  | UnsupportedBinop of binop * literal * literal
  | UnsupportedUnop of unop * literal
  | UnsupportedExpr of expr
  | MismatchType of string * type_desc list
  | NotYetImplemented of string
  | ConflictingTypes of type_desc list * ty
  | AssertionFailed of expr
  | CannotParse
  | UnknownSymbol
  | NoCallCandidate of string * ty list
  | TooManyCallCandidates of string * ty list
  | BadTypesForBinop of binop * ty * ty
  | CircularDeclarations of string
  | UnpureExpression of expr
  | UnreconciliableTypes of ty * ty
  | AssignToImmutable of string
  | AlreadyDeclaredIdentifier of string
  | BadReturnStmt of ty option
  | UnexpectedSideEffect of string
  | UncaughtException of string
  | OverlappingSlices of slice list
  | BadLDI of AST.local_decl_item
  | BadRecursiveDecls of identifier list
  | UnrespectedParserInvariant
  | ConstrainedIntegerExpected of ty
  | ParameterWithoutDecl of identifier

type error = error_desc annotated

exception ASLException of error

type 'a result = ('a, error) Result.t

let fatal e = raise (ASLException e)
let fatal_from pos e = fatal (ASTUtils.add_pos_from pos e)

let fatal_here pos_start pos_end e =
  fatal (ASTUtils.annotated e pos_start pos_end)

let fatal_unknown_pos e = fatal (ASTUtils.add_dummy_pos e)
let intercept f () = try Ok (f ()) with ASLException e -> Error e

let pp_error =
  let open Format in
  let open PP in
  let pp_comma_list pp_elt f li =
    pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") pp_elt f li
  in
  let pp_type_desc f ty = pp_ty f (ASTUtils.add_dummy_pos ty) in
  fun f e ->
    pp_open_vbox f 0;
    if e.pos_end != Lexing.dummy_pos && e.pos_start != Lexing.dummy_pos then
      fprintf f "@[<h>%a:@]@ " pp_pos e;
    pp_open_hovbox f 2;
    (match e.desc with
    | UnsupportedBinop (op, v1, v2) ->
        fprintf f
          "ASL Execution error: Illegal application of operator %s for values@ \
           %a@ and %a."
          (binop_to_string op) pp_literal v1 pp_literal v2
    | UnsupportedUnop (op, v) ->
        fprintf f
          "ASL Execution error: Illegal application of operator %s for value@ \
           %a."
          (unop_to_string op) pp_literal v
    | UnsupportedExpr e ->
        fprintf f "ASL Error: Unsupported expression %a." pp_expr e
    | MismatchType (v, [ ty ]) ->
        fprintf f
          "ASL Execution error: Mismatch type:@ value %s does not belong to \
           type %a."
          v pp_type_desc ty
    | MismatchType (v, li) ->
        fprintf f
          "ASL Execution error: Mismatch type:@ value %s@ does not subtype any \
           of those types:@ %a"
          v
          (pp_comma_list pp_type_desc)
          li
    | BadField (s, ty) ->
        fprintf f "ASL Error: There are no field '%s'@ on type %a." s pp_ty ty
    | MissingField (fields, ty) ->
        fprintf f
          "ASL Error: Fields mismatch for creating a value of type %a@ -- \
           Passed fields are:@ %a"
          pp_ty ty
          (pp_print_list ~pp_sep:pp_print_space pp_print_string)
          fields
    | BadSlices (slices, length) ->
        fprintf f
          "ASL Typing error: Cannot extract from bitvector of length %d slices \
           %a."
          length pp_slice_list slices
    | TypeInferenceNeeded ->
        pp_print_text f
          "ASL Internal error: Interpreter blocked. Type inference needed."
    | UndefinedIdentifier s ->
        fprintf f "ASL Error: Undefined identifier:@ '%s'" s
    | MismatchedReturnValue s ->
        fprintf f "ASL Error: Mismatched use of return value from call to '%s'"
          s
    | BadArity (name, expected, provided) ->
        fprintf f
          "ASL Error: Arity error while calling '%s':@ %d arguments expected \
           and %d provided"
          name expected provided
    | NotYetImplemented s ->
        pp_print_text f @@ "ASL Internal error: Not yet implemented: " ^ s
    | ConflictingTypes ([ expected ], provided) ->
        fprintf f
          "ASL Typing error:@ a subtype of@ %a@ was expected,@ provided %a."
          pp_type_desc expected pp_ty provided
    | ConflictingTypes (expected, provided) ->
        fprintf f "ASL Typing error:@ %a does@ not@ subtype@ any@ of:@ %a."
          pp_ty provided
          (pp_comma_list pp_type_desc)
          expected
    | AssertionFailed e ->
        fprintf f "ASL Execution error: Assertion failed:@ %a" pp_expr e
    | CannotParse -> pp_print_string f "ASL Error: Cannot parse."
    | UnknownSymbol -> pp_print_string f "ASL Error: Unknown symbol."
    | NoCallCandidate (name, types) ->
        fprintf f
          "ASL Typing error: No subprogram declaration matches the \
           invocation:@ %s(%a)"
          name (pp_comma_list pp_ty) types
    | TooManyCallCandidates (name, types) ->
        fprintf f
          "ASL Typing error: Too many subprogram declaration match the \
           invocation:@ %s(%a)"
          name (pp_comma_list pp_ty) types
    | BadTypesForBinop (op, t1, t2) ->
        fprintf f
          "ASL Typing error: Illegal application of operator %s on types@ %a@ \
           and %a"
          (binop_to_string op) pp_ty t1 pp_ty t2
    | CircularDeclarations x ->
        fprintf f
          "ASL Evaluation error: circular definition of constants, including \
           %S."
          x
    | UnpureExpression e ->
        fprintf f
          "ASL Typing error:@ a pure expression was expected,@ found@ %a"
          pp_expr e
    | UnreconciliableTypes (t1, t2) ->
        fprintf f
          "ASL Typing error:@ cannot@ find@ a@ common@ ancestor@ to@ those@ \
           two@ types@ %a@ and@ %a."
          pp_ty t1 pp_ty t2
    | AssignToImmutable x ->
        fprintf f
          "ASL Typing error:@ cannot@ assign@ to@ immutable@ storage@ %S." x
    | AlreadyDeclaredIdentifier x ->
        fprintf f
          "ASL Typing error:@ cannot@ declare@ already@ declared@ element@ %S."
          x
    | BadReturnStmt None ->
        fprintf f
          "ASL Typing error:@ cannot@ return@ something@ from@ a@ procedure."
    | UnexpectedSideEffect s -> fprintf f "Unexpected side-effect: %s" s
    | UncaughtException s -> fprintf f "Uncaught exception: %s" s
    | OverlappingSlices slices ->
        fprintf f "ASL Typing error:@ overlapping slices@ @[%a@]." pp_slice_list
          slices
    | BadLDI ldi ->
        fprintf f "Unsupported declaration:@ @[%a@]." pp_local_decl_item ldi
    | BadRecursiveDecls decls ->
        fprintf f "ASL Typing error:@ multiple recursive declarations:@ @[%a@]"
          (pp_comma_list (fun f -> fprintf f "%S"))
          decls
    | UnrespectedParserInvariant -> fprintf f "Parser invariant broke."
    | ConstrainedIntegerExpected t ->
        fprintf f
          "ASL Typing error:@ constrained@ integer@ expected,@ provided@ %a"
          pp_ty t
    | ParameterWithoutDecl s ->
        fprintf f
          "ASL Typing error:@ explicit@ parameter@ %S@ does@ not@ have@ a@ \
           corresponding@ defining@ argument"
          s
    | BadReturnStmt (Some t) ->
        fprintf f
          "ASL Typing error:@ cannot@ return@ nothing@ from@ a@ function,@ an@ \
           expression@ of@ type@ %a@ is@ expected."
          pp_ty t);
    pp_close_box f ();
    pp_close_box f ()

let error_to_string = Format.asprintf "%a" pp_error
let eprintln = Format.eprintf "@[<2>%a@]@." pp_error

let () =
  Printexc.register_printer @@ function
  | ASLException e -> Some (error_to_string e)
  | _ -> None
