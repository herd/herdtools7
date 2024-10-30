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

type error_handling_time = Static | Dynamic

type error_desc =
  | ReservedIdentifier of string
  | BadField of string * ty
  | MissingField of string list * ty
  | BadSlices of error_handling_time * slice list * int
  | BadSlice of slice
  | EmptySlice
  | TypeInferenceNeeded
  | UndefinedIdentifier of identifier
  | MismatchedReturnValue of string
  | BadArity of identifier * int * int
  | UnsupportedBinop of error_handling_time * binop * literal * literal
  | UnsupportedUnop of error_handling_time * unop * literal
  | UnsupportedExpr of error_handling_time * expr
  | InvalidExpr of expr
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
  | ImpureExpression of expr
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
  | BadATC of ty * ty  (** asserting, asserted *)
  | BadPattern of pattern * ty
  | ConstrainedIntegerExpected of ty
  | ParameterWithoutDecl of identifier
  | BaseValueEmptyType of ty
  | BaseValueNonStatic of ty * expr
  | SettingIntersectingSlices of bitfield list
  | SetterWithoutCorrespondingGetter of func
  | UnexpectedATC
  | UnreachableReached

type error = error_desc annotated

exception ASLException of error

type 'a result = ('a, error) Result.t

let fatal e = raise (ASLException e)
let fatal_from pos e = fatal (ASTUtils.add_pos_from pos e)

let fatal_here pos_start pos_end e =
  fatal (ASTUtils.annotated e pos_start pos_end)

let fatal_unknown_pos e = fatal (ASTUtils.add_dummy_pos e)
let intercept f () = try Ok (f ()) with ASLException e -> Error e

let error_handling_time_to_string = function
  | Static -> "Static"
  | Dynamic -> "Dynamic"

type warning_desc =
  | IntervalTooBigToBeExploded of Z.t * Z.t
  | RemovingValuesFromConstraints of {
      op : binop;
      prev : int_constraint list;
      after : int_constraint list;
    }

type warning = warning_desc annotated

module PPrint = struct
  open Format
  open PP

  let pp_comma_list pp_elt f li =
    pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") pp_elt f li

  let pp_type_desc f ty = pp_ty f (ASTUtils.add_dummy_pos ty)

  let pp_error_desc f e =
    pp_open_hovbox f 2;
    (match e.desc with
    | ReservedIdentifier id ->
        fprintf f "ASL Lexical error: %S is a reserved keyword." id
    | UnsupportedBinop (t, op, v1, v2) ->
        fprintf f
          "ASL %s error: Illegal application of operator %s for values@ %a@ \
           and %a."
          (error_handling_time_to_string t)
          (binop_to_string op) pp_literal v1 pp_literal v2
    | UnsupportedUnop (t, op, v) ->
        fprintf f
          "ASL %s error: Illegal application of operator %s for value@ %a."
          (error_handling_time_to_string t)
          (unop_to_string op) pp_literal v
    | UnsupportedExpr (t, e) ->
        fprintf f "ASL %s Error: Unsupported expression %a."
          (error_handling_time_to_string t)
          pp_expr e
    | InvalidExpr e -> fprintf f "ASL Error: invalid expression %a." pp_expr e
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
    | EmptySlice ->
        pp_print_text f
          "ASL Static Error: cannot slice with empty slicing operator. This \
           might also be due to an incorrect getter/setter invocation."
    | BadSlices (t, slices, length) ->
        fprintf f
          "ASL %s error: Cannot extract from bitvector of length %d slice %a."
          (error_handling_time_to_string t)
          length pp_slice_list slices
    | BadSlice slice -> fprintf f "ASL error: invalid slice %a." pp_slice slice
    | TypeInferenceNeeded ->
        pp_print_text f
          "ASL Internal error: Interpreter blocked. Type inference needed."
    | UndefinedIdentifier s ->
        fprintf f "ASL Error: Undefined identifier:@ '%s'" s
    | MismatchedReturnValue s ->
        fprintf f "ASL Error: Mismatched use of return value from call to '%s'."
          s
    | BadArity (name, expected, provided) ->
        fprintf f
          "ASL Error: Arity error while calling '%s':@ %d arguments expected \
           and %d provided."
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
        fprintf f "ASL Execution error: Assertion failed:@ %a." pp_expr e
    | CannotParse -> pp_print_string f "ASL Error: Cannot parse."
    | UnknownSymbol -> pp_print_string f "ASL Error: Unknown symbol."
    | NoCallCandidate (name, types) ->
        fprintf f
          "ASL Typing error: No subprogram declaration matches the \
           invocation:@ %s(%a)."
          name (pp_comma_list pp_ty) types
    | TooManyCallCandidates (name, types) ->
        fprintf f
          "ASL Typing error: Too many subprogram declaration match the \
           invocation:@ %s(%a)."
          name (pp_comma_list pp_ty) types
    | BadTypesForBinop (op, t1, t2) ->
        fprintf f
          "ASL Typing error: Illegal application of operator %s on types@ %a@ \
           and %a."
          (binop_to_string op) pp_ty t1 pp_ty t2
    | CircularDeclarations x ->
        fprintf f
          "ASL Evaluation error: circular definition of constants, including \
           %S."
          x
    | ImpureExpression e ->
        fprintf f
          "ASL Typing error:@ a pure expression was expected,@ found@ %a."
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
        pp_print_text f
          "ASL Typing error: cannot return something from a procedure."
    | UnexpectedSideEffect s -> fprintf f "Unexpected side-effect: %s." s
    | UncaughtException s -> fprintf f "Uncaught exception: %s." s
    | OverlappingSlices slices ->
        fprintf f "ASL Typing error:@ overlapping slices@ @[%a@]." pp_slice_list
          slices
    | BadLDI ldi ->
        fprintf f "Unsupported declaration:@ @[%a@]." pp_local_decl_item ldi
    | BadRecursiveDecls decls ->
        fprintf f "ASL Typing error:@ multiple recursive declarations:@ @[%a@]."
          (pp_comma_list (fun f -> fprintf f "%S"))
          decls
    | UnrespectedParserInvariant -> fprintf f "Parser invariant broke."
    | ConstrainedIntegerExpected t ->
        fprintf f
          "ASL Typing error:@ constrained@ integer@ expected,@ provided@ %a."
          pp_ty t
    | ParameterWithoutDecl s ->
        fprintf f
          "ASL Typing error:@ explicit@ parameter@ %S@ does@ not@ have@ a@ \
           corresponding@ defining@ argument."
          s
    | BaseValueEmptyType t ->
        fprintf f "ASL Typing error: base value of empty type %a." pp_ty t
    | BaseValueNonStatic (t, e) ->
        fprintf f
          "ASL Typing error:@ base@ value@ of@ type@ %a@ cannot@ be@ \
           statically@ determined@ since@ it@ consists@ of@ %a."
          pp_ty t pp_expr e
    | BadATC (t1, t2) ->
        fprintf f
          "ASL Typing error:@ cannot@ perform@ Asserted@ Type@ Conversion@ on@ \
           %a@ by@ %a."
          pp_ty t1 pp_ty t2
    | SettingIntersectingSlices bitfields ->
        fprintf f "ASL Typing error:@ setting@ intersecting@ bitfields@ [%a]."
          pp_bitfields bitfields
    | SetterWithoutCorrespondingGetter func ->
        let ret, args =
          match func.args with
          | (_, ret) :: args -> (ret, List.map snd args)
          | _ -> assert false
        in
        fprintf f
          "ASL Typing error:@ setter@ \"%s\"@ does@ not@ have@ a@ \
           corresponding@ getter@ of@ signature@ @[@[%a@]@ ->@ %a@]."
          func.name (pp_comma_list pp_ty) args pp_ty ret
    | UnexpectedATC -> pp_print_text f "ASL Typing error: unexpected ATC."
    | BadPattern (p, t) ->
        fprintf f
          "ASL Typing error:@ Erroneous@ pattern@ %a@ for@ expression@ of@ \
           type@ %a."
          pp_pattern p pp_ty t
    | UnreachableReached ->
        pp_print_text f "ASL Dynamic error: Unreachable reached."
    | BadReturnStmt (Some t) ->
        fprintf f
          "ASL Typing error:@ cannot@ return@ nothing@ from@ a@ function,@ an@ \
           expression@ of@ type@ %a@ is@ expected."
          pp_ty t);
    pp_close_box f ()

  let pp_warning_desc f w =
    match w.desc with
    | IntervalTooBigToBeExploded (za, zb) ->
        fprintf f
          "@[Interval too large: @[<h>[ %a .. %a ]@].@ Keeping it as an \
           interval.@]"
          Z.pp_print za Z.pp_print zb
    | RemovingValuesFromConstraints { op; prev; after } ->
        fprintf f
          "@[Warning:@ Removing@ some@ values@ that@ would@ fail@ with@ op %s@ \
           from@ constraint@ set@ @[<h>{%a}@]@ gave@ @[<h>{%a}@].@ Continuing@ \
           with@ this@ constraint@ set.@]"
          (binop_to_string op) PP.pp_int_constraints prev PP.pp_int_constraints
          after

  let pp_pos_begin f pos =
    if pos.pos_end != Lexing.dummy_pos && pos.pos_start != Lexing.dummy_pos then
      fprintf f "@[<h>%a:@]@ " pp_pos pos

  let pp_error f e = fprintf f "@[<v 0>%a%a@]" pp_pos_begin e pp_error_desc e

  let pp_warning f e =
    fprintf f "@[<v 0>%a%a@]" pp_pos_begin e pp_warning_desc e

  let error_desc_to_string = asprintf "%a" pp_error_desc

  let desc_to_string_inf pp_desc =
    asprintf "%a" @@ fun f e ->
    pp_set_margin f 1_000_000_000;
    pp_desc f e

  let error_to_string = asprintf "%a" pp_error
end

include PPrint

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' ->
          Buffer.add_char b '"';
          Buffer.add_char b '"'
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let pp_csv pp_desc =
  let pos_in_line pos = Lexing.(pos.pos_cnum - pos.pos_bol) in
  fun f pos ->
    Printf.fprintf f "\"%s\",%d,%d,%d,%d,\"%s\""
      (escape pos.pos_start.pos_fname)
      pos.pos_start.pos_lnum
      (pos_in_line pos.pos_start)
      pos.pos_end.pos_lnum (pos_in_line pos.pos_end)
      (desc_to_string_inf pp_desc pos |> escape)

let pp_error_csv f e = pp_csv pp_error_desc f e
let pp_warning_csv f w = pp_csv pp_warning_desc f w

type output_format = HumanReadable | CSV | Silence

module type ERROR_PRINTER_CONFIG = sig
  val output_format : output_format
end

module ErrorPrinter (C : ERROR_PRINTER_CONFIG) = struct
  let eprintln e =
    match C.output_format with
    | HumanReadable -> Format.eprintf "@[<2>%a@]@." pp_error e
    | CSV -> Printf.eprintf "%a\n" pp_error_csv e
    | Silence -> ()

  let warn w =
    match C.output_format with
    | HumanReadable -> Format.eprintf "@[<2>%a@]@." pp_warning w
    | CSV -> Printf.eprintf "%a\n" pp_warning_csv w
    | Silence -> ()

  let warn_from ~loc w = ASTUtils.add_pos_from loc w |> warn
end

let () =
  Printexc.register_printer @@ function
  | ASLException e -> Some (error_to_string e)
  | _ -> None
