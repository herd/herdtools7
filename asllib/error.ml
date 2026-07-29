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
  | BadIndex of int * int
  | BadSlice of slice
  | EmptySlice
  | TypeInferenceNeeded
  | UndefinedIdentifier of error_handling_time * identifier
  | MismatchedCallType of {
      subprogram_name : string;
      expected_call_type : subprogram_type;
      found_call_type : subprogram_type;
    }
  | BadArity of error_handling_time * identifier * int * int
  | BadParameterArity of error_handling_time * version * identifier * int * int
  | UnsupportedBinop of error_handling_time * binop * literal * literal
  | UnsupportedUnop of error_handling_time * unop * literal
  | UnsupportedExpr of error_handling_time * expr
  | UnsupportedTy of error_handling_time * ty
  | InvalidExpr of expr
  | MismatchType of string * type_desc list
  | ConflictingTypes of type_desc list * ty
  | AssertionFailed of expr
  | CannotParse of string option
  | UnknownSymbol of string
  | NoCallCandidate of string * ty list
  | BadTypesForBinop of binop * ty * ty
  | ImpureExpression of expr * SideEffect.SES.t
      (** used for fine-grained analysis *)
  | MismatchedPurity of string  (** Used for coarse-grained analysis *)
  | UnreconcilableTypes of ty * ty
  | AssignToImmutable of string
  | AssignToTupleElement of lexpr
  | AlreadyDeclaredIdentifier of string
  | BadReturnStmt of ty option
  | UnexpectedSideEffect of string
  | UncaughtException of string
  | OverlappingSlices of slice list * error_handling_time
  | BadLDI of AST.local_decl_item
  | BadRecursiveDecls of identifier list
  | UnrespectedParserInvariant
  | BadATC of ty * ty  (** asserting, asserted *)
  | DynamicATCFailure of string * type_desc
  | BadPattern of pattern * ty
  | ConstrainedIntegerExpected of ty
  | ParameterWithoutDecl of identifier
  | BadParameterDecl of identifier * identifier list * identifier list
      (** name, expected, actual *)
  | BaseValueEmptyType of ty
  | ArbitraryEmptyType of ty
  | BaseValueNonSymbolic of ty * expr
  | SetterWithoutCorrespondingGetter of func
  | NonReturningFunction of identifier
  | NoreturnViolation of identifier
  | ConflictingSideEffects of SideEffect.t * SideEffect.t
  | UnreachableReached
  | LoopLimitReached
  | RecursionLimitReached of error_handling_time
  | EmptyConstraints
  | UnexpectedPendingConstrained
  | BitfieldsDontAlign of {
      field1_absname : string;
      field2_absname : string;
      field1_absslices : string;
      field2_absslices : string;
    }
  | ExpectedSingularType of ty
  | ExpectedNamedType of ty
  | ConstantTimeBroken of expr * SideEffect.SES.t
  | MultipleWrites of identifier
  | UnexpectedInitialisationThrow of
      ty * identifier (* Exception type and global storage element name. *)
  | NegativeArrayLength of expr * int
  | MultipleImplementations of func annotated * func annotated
  | NoOverrideCandidate
  | TooManyOverrideCandidates of func annotated list
  | PrecisionLostDefining
  | UnexpectedCollection
  | BadPrimitiveArgument of identifier * string
  | NoEntryPoint
  | ObsoleteSyntax of (Format.formatter -> unit)

type error = error_desc annotated

exception ASLException of error

type 'a result = ('a, error) Result.t

let fatal e = raise (ASLException e)
let fatal_from pos e = fatal (ASTUtils.add_pos_from pos e)

let fatal_here pos_start pos_end e =
  fatal (ASTUtils.annotated e pos_start pos_end ASTUtils.default_version)

let fatal_unknown_pos e = fatal (ASTUtils.add_dummy_pos e)
let intercept f () = try Ok (f ()) with ASLException e -> Error e

type warning_desc =
  | NoRecursionLimit of identifier list
  | NoLoopLimit
  | IntervalTooBigToBeExploded of Z.t * Z.t
  | ConstraintSetPairToBigToBeExploded of {
      op : binop;
      left : int_constraint list;
      right : int_constraint list;
      log_max : int;  (** Maximum size breached by this constraint set pair. *)
    }
  | RemovingValuesFromConstraints of {
      op : binop;
      prev : int_constraint list;
      after : int_constraint list;
    }
  | PragmaUse of identifier
  | UnexpectedImplementation
  | MissingOverride

type warning = warning_desc annotated

module ErrorCode = struct
  type build =
    | LE  (** Lexical *)
    | PE  (** Parse *)
    | RI  (** Reserved identifier *)
    | BOP  (** Binary operation priority *)
    | BD  (** Bad declaration *)

  type typing =
    | UI  (** Undefined identifier *)
    | IAD  (** Identifier already declared *)
    | AIM  (** Assign to immutable *)
    | TSF  (** Type satisfaction failure *)
    | LCA  (** Lowest common ancestor *)
    | NBV  (** No base value *)
    | TAF  (** Type assertion failure *)
    | SEF  (** Static evaluation failure *)
    | BO  (** Bad operands *)
    | UT  (** Unexpected type *)
    | BTI  (** Bad tuple index *)
    | BS  (** Bad slices *)
    | BF  (** Bad field *)
    | BSPD  (** Bad subprogram declaration *)
    | BD  (** Bad declaration *)
    | BC  (** Bad call *)
    | SEV  (** Side effect violation *)
    | OE  (** Overriding error *)
    | PLD  (** Declaration with an imprecise type *)

  type dynamic =
    | UNR  (** Unreachable *)
    | DAF  (** Assertion failure *)
    | TAF  (** Type assertion failure *)
    | AET  (** Arbitrary empty type *)
    | BO  (** Bad operands *)
    | LE  (** Limit exceeded *)
    | UE  (** Uncaught exception *)
    | BI  (** Bad index *)
    | OSA  (** Overlapping slice assignment *)
    | NAL  (** Negative array length *)
    | NEP  (** No entry point *)

  type t = Build of build | Typing of typing | Dynamic of dynamic

  (* TODO: consider using ppx to derive strings *)

  let build_to_string : build -> string = function
    | LE -> "LE"
    | PE -> "PE"
    | RI -> "RI"
    | BOP -> "BOP"
    | BD -> "BD"

  let typing_to_string : typing -> string = function
    | UI -> "UI"
    | IAD -> "IAD"
    | AIM -> "AIM"
    | TSF -> "TSF"
    | LCA -> "LCA"
    | NBV -> "NBV"
    | TAF -> "TAF"
    | SEF -> "SEF"
    | BO -> "BO"
    | UT -> "UT"
    | BTI -> "BTI"
    | BS -> "BS"
    | BF -> "BF"
    | BSPD -> "BSPD"
    | BD -> "BD"
    | BC -> "BC"
    | SEV -> "SEV"
    | OE -> "OE"
    | PLD -> "PLD"

  let dynamic_to_string : dynamic -> string = function
    | UNR -> "UNR"
    | DAF -> "DAF"
    | TAF -> "TAF"
    | AET -> "AET"
    | BO -> "BO"
    | LE -> "LE"
    | UE -> "UE"
    | BI -> "BI"
    | OSA -> "OSA"
    | NAL -> "NAL"
    | NEP -> "NEP"

  let to_string = function
    | Build b -> "BE_" ^ build_to_string b
    | Typing t -> "TE_" ^ typing_to_string t
    | Dynamic d -> "DE_" ^ dynamic_to_string d

  let of_error e =
    match e.desc with
    (********** Errors that correspond to error codes **********)
    | ReservedIdentifier _ -> Some (Build RI)
    | UnknownSymbol _ -> Some (Build LE)
    | BadField _ | MissingField _ -> Some (Typing BF)
    | BadPattern _ | BadTypesForBinop _
    | UnsupportedUnop (Static, _, _)
    | UnsupportedBinop (Static, _, _, _) ->
        Some (Typing BO)
    | BadSlices (Static, _, _)
    | BadSlice _ | EmptySlice
    | OverlappingSlices (_, Static)
    | BitfieldsDontAlign _ ->
        Some (Typing BS) (* TODO: consider combining BadSlices and BadSlice *)
    | BadSlices (Dynamic, _, _) -> Some (Dynamic BI)
    | BadIndex _ -> Some (Dynamic BI)
    | UndefinedIdentifier (Static, _) -> Some (Typing UI)
    | ConflictingTypes _ | AssignToTupleElement _ | ConstrainedIntegerExpected _
    | UnexpectedPendingConstrained | ExpectedSingularType _
    | ExpectedNamedType _ | UnexpectedCollection ->
        Some (Typing UT)
    | MismatchedCallType _
    | BadParameterArity (Static, _, _, _, _)
    | NoCallCandidate _ ->
        Some (Typing BC)
    | UnsupportedUnop (Dynamic, _, _) | UnsupportedBinop (Dynamic, _, _, _) ->
        Some (Dynamic BO)
    | AssertionFailed _ | BadPrimitiveArgument _ -> Some (Dynamic DAF)
    | ImpureExpression _ | MismatchedPurity _ -> Some (Typing SEV)
    | AssignToImmutable _ -> Some (Typing AIM)
    | AlreadyDeclaredIdentifier _ -> Some (Typing IAD)
    | BadReturnStmt _ | BadParameterDecl _ | NonReturningFunction _
    | NoreturnViolation _ ->
        Some (Typing BSPD)
    | UncaughtException _ -> Some (Dynamic UE)
    | OverlappingSlices (_, Dynamic) -> Some (Dynamic OSA)
    | BadLDI _ | BadRecursiveDecls _ -> Some (Typing BD)
    | BadATC _ -> Some (Typing TAF)
    | DynamicATCFailure _ -> Some (Dynamic TAF)
    | BaseValueEmptyType _ | BaseValueNonSymbolic _ -> Some (Typing NBV)
    | ArbitraryEmptyType _ -> Some (Dynamic AET)
    | UnreachableReached -> Some (Dynamic UNR)
    | LoopLimitReached | RecursionLimitReached Dynamic -> Some (Dynamic LE)
    | NegativeArrayLength _ -> Some (Dynamic NAL)
    | MultipleImplementations _ | NoOverrideCandidate
    | TooManyOverrideCandidates _ ->
        Some (Typing OE)
    | PrecisionLostDefining -> Some (Typing PLD)
    | NoEntryPoint -> Some (Dynamic NEP)
    | RecursionLimitReached Static -> Some (Typing SEF)
    (********** TODO tidy up - does not cleanly correspond to a code **********)
    | BadArity (Static, _, _, _) (* also used for tuple unpacking *) -> None
    | UnsupportedExpr _ | UnsupportedTy _
    (* For static interpretation, parameters, and collections *) ->
        None
    | MismatchType _
    (* Backend type mismatches and mismatched integers for loop limits. *) ->
        None
    | CannotParse _ (* used in lexing too *) -> Some (Build PE)
    | UnreconcilableTypes _ (* both LCA and check_bit_widths_equal *) -> None
    | EmptyConstraints (* does this need to be reflected in reference? *) ->
        None
    | MultipleWrites _
    (* For desugaring, but uses `check_no_duplicates` which is always TE_IAD? *)
      ->
        None
    | UnexpectedInitialisationThrow _ -> Some (Dynamic UE)
    (********** Should not happen **********)
    (* e.g. skipped type-checking, ASL0, internal option or invariant *)
    | TypeInferenceNeeded
    | UndefinedIdentifier (Dynamic, _)
    | BadArity (Dynamic, _, _, _)
    | BadParameterArity (Dynamic, _, _, _, _)
    | InvalidExpr _ | UnexpectedSideEffect _ | UnrespectedParserInvariant
    | ParameterWithoutDecl _ | SetterWithoutCorrespondingGetter _
    | ConflictingSideEffects _ | ConstantTimeBroken _ ->
        None
    (********** Other **********)
    | ObsoleteSyntax _ -> None
end

module PrintContext = struct
  (* Straight out of stdlib v5.2 *)
  let with_open filename continuation =
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in_noerr chan)
      (fun () -> continuation chan)

  (** [trim_end str] is [str] without any spaces at the end. *)
  let trim_end str =
    let n0 = String.length str - 1 in
    let n = ref n0 in
    let is_space (* Out of stdlib *) = function
      | ' ' | '\012' | '\n' | '\r' | '\t' -> true
      | _ -> false
    in
    while !n > 0 && is_space (String.get str !n) do
      decr n
    done;
    if Int.equal !n n0 then str else String.sub str 0 (!n + 1)

  (** [fetch_lines ~start_bol ~end_bol filename] returns a string containing the
      lines from the line indicated by [start_bol] to (and including) the line
      indicated by [end_bol], without any spaces at the end. *)
  let fetch_lines ~start_bol ~end_bol filename =
    with_open filename @@ fun chan ->
    seek_in chan start_bol;
    let prev_lines =
      if end_bol > start_bol then really_input_string chan (end_bol - start_bol)
      else ""
    in
    let last_line =
      (* [input_line] raises [End_of_file] if EOF is at the start of the line *)
      try input_line chan with End_of_file -> ""
    in
    let () =
      if false then
        Format.eprintf "Got prev_lines = %S and last_line = %S.@." prev_lines
          last_line
    in
    prev_lines ^ last_line |> trim_end

  (** [chevrons ~start_col ~end_col] is a line starting with [start_col] spaces
      and completed with [^] until [end_col] is reached. *)
  let chevrons ~start_col ~end_col : string =
    if end_col < start_col then raise (Invalid_argument "chevrons");
    String.make start_col ' ' ^ String.make (end_col - start_col) '^'

  let display_error_context e : string option =
    let open AST in
    let open Lexing in
    let filename = e.pos_start.pos_fname
    and end_filename = e.pos_end.pos_fname
    and start_lnum = e.pos_start.pos_lnum
    and end_lnum = e.pos_end.pos_lnum
    and start_cnum = e.pos_start.pos_cnum
    and end_cnum = e.pos_end.pos_cnum
    and start_bol = e.pos_start.pos_bol
    and end_bol = e.pos_end.pos_bol in
    if ASTUtils.is_dummy_pos e then None
    else if String.equal filename end_filename && Sys.file_exists filename then
      let lines = fetch_lines ~start_bol ~end_bol filename in
      let lines =
        if Int.equal start_lnum end_lnum then
          let start_col = start_cnum - start_bol
          and end_col = end_cnum - end_bol in
          let chevrons = chevrons ~start_col ~end_col in
          Printf.sprintf "%s\n%s" lines chevrons
        else lines
      in
      Some lines
    else None
end

(** TODO
    - Various errors are overused in several places - need to clearly
      distinguish between ASL1 errors and e.g. ASL0 non-typechecked errors,
      assertion failures, cases we don't expect to hit etc.
    - TypingRule.TInt mismatch on empty case *)
(* TODO: check_implementations_unique should be TE_OE in reference - instead just generic #TE *)
(* TODO: BE_RI unused in reference *)
(* TODO: following not recoverable from implementation:
- BE_BOP
- BE_BD
- TE_TSF
- TE_LCA
- TE_SEF
- TE_BTI
*)

module PPrint = struct
  open Format
  open PP

  let pp_comma_list pp_elt f li =
    pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") pp_elt f li

  let pp_type_desc f ty = pp_ty f (ASTUtils.add_dummy_pos ty)
  let lexical = "Lexical"
  let parse = "Grammar"
  let static = "Static"
  let typing = "Type"
  let dynamic = "Dynamic"
  let internal = "Internal"

  let fprintf_err f kind code_opt =
    let code_string =
      match code_opt with
      | None -> ""
      | Some code -> " (" ^ ErrorCode.to_string code ^ ")"
    in
    let () =
      match code_opt with
      | Some (Typing _) -> assert (kind = typing || kind = static)
      | Some (Build _) ->
          assert (kind = lexical || kind = parse || kind = static)
      | Some (Dynamic _) -> assert (kind = dynamic)
      | None -> ()
    in
    kdprintf (fun msg ->
        fprintf f "@[<hov 2>ASL %s error%s:@ %t@]" kind code_string msg)

  let error_handling_time_to_string = function
    | Static -> static
    | Dynamic -> dynamic

  let pp_error_desc f e =
    let pp_err s fmt = fprintf_err f s (ErrorCode.of_error e) fmt in
    match e.desc with
    | ReservedIdentifier id -> pp_err lexical "%S is a reserved keyword." id
    | UnsupportedBinop (t, op, v1, v2) ->
        pp_err
          (error_handling_time_to_string t)
          "Illegal application of operator %s for values@ %a@ and %a."
          (binop_to_string op) pp_literal v1 pp_literal v2
    | UnsupportedUnop (t, op, v) ->
        pp_err
          (error_handling_time_to_string t)
          "Illegal application of operator %s for value@ %a."
          (unop_to_string op) pp_literal v
    | UnsupportedExpr (t, e) ->
        pp_err
          (error_handling_time_to_string t)
          "Unsupported expression %a." pp_expr e
    | UnsupportedTy (t, ty) ->
        pp_err (error_handling_time_to_string t) "Unsupported type %a." pp_ty ty
    | InvalidExpr e -> pp_err typing "invalid expression %a." pp_expr e
    | MismatchType (v, [ ty ]) ->
        pp_err dynamic "Mismatch type:@ value %s does not belong to type %a." v
          pp_type_desc ty
    | MismatchType (v, li) ->
        pp_err dynamic
          "Mismatch type:@ value %s@ does not subtype any of those types:@ %a" v
          (pp_comma_list pp_type_desc)
          li
    | BadField (s, ty) ->
        pp_err typing "There is no field '%s'@ on type %a." s pp_ty ty
    | MissingField (fields, ty) ->
        pp_err typing
          "Fields mismatch for creating a value of type %a@ -- Passed fields \
           are:@ %a"
          pp_ty ty
          (pp_print_list ~pp_sep:pp_print_space pp_print_string)
          fields
    | EmptySlice ->
        assert (e.version = V0);
        pp_err static
          "cannot slice with empty slicing operator. This might also be due to \
           an incorrect getter/setter invocation."
    | BadSlices (Dynamic, slices, _) ->
        pp_err dynamic
          "invalid slice %a: start and length must be non-negative."
          pp_slice_list slices
    | BadSlices (Static, slices, length) ->
        pp_err static "Cannot extract from bitvector of length %d slice %a."
          length pp_slice_list slices
    | BadIndex (index, length) ->
        pp_err dynamic "index %d is outside the valid range 0..%d." index
          (length - 1)
    | BadSlice slice -> pp_err static "invalid slice %a." pp_slice slice
    | TypeInferenceNeeded ->
        pp_err internal "Interpreter blocked. Type inference needed."
    | UndefinedIdentifier (t, s) ->
        pp_err (error_handling_time_to_string t) "Undefined identifier:@ '%s'" s
    | MismatchedCallType
        { subprogram_name = s; expected_call_type; found_call_type } ->
        let call_type_description call_type =
          match call_type with
          | ST_Function -> "function"
          | ST_Getter -> "getter"
          | ST_Setter -> "setter"
          | ST_Procedure -> "procedure"
        in
        pp_err static
          "Mismatched call type for subprogram '%s': expected a %s and found a \
           %s."
          s
          (call_type_description expected_call_type)
          (call_type_description found_call_type)
    | BadArity (t, name, expected, provided) ->
        pp_err
          (error_handling_time_to_string t)
          "Arity error while calling '%s':@ %d arguments expected and %d \
           provided."
          name expected provided
    | BadParameterArity (t, version, name, expected, provided) -> (
        match (t, version) with
        | Static, V0 ->
            pp_err
              (error_handling_time_to_string t)
              "Could not infer all parameters while calling '%s':@ %d \
               parameters expected and %d inferred"
              name expected provided
        | _ ->
            pp_err
              (error_handling_time_to_string t)
              "Arity error while calling '%s':@ %d parameters expected and %d \
               provided"
              name expected provided)
    | ConflictingTypes ([ expected ], provided) ->
        pp_err typing "a subtype of@ %a@ was expected,@ provided %a."
          pp_type_desc expected pp_ty provided
    | ConflictingTypes (expected, provided) ->
        pp_err typing "%a does@ not@ subtype@ any@ of:@ %a." pp_ty provided
          (pp_comma_list pp_type_desc)
          expected
    | AssertionFailed e -> pp_err dynamic "Assertion failed:@ %a." pp_expr e
    | CannotParse s -> (
        match s with
        | None -> pp_err parse "Cannot parse."
        | Some s -> pp_err parse "Cannot parse.@ %a" pp_print_text s)
    | UnknownSymbol s ->
        let codes = List.map Char.code (List.of_seq (String.to_seq s)) in
        let not_printable code = code < 33 || code > 126 in
        if List.exists not_printable codes then
          pp_err lexical "Unknown symbol (ASCII code point(s): %a)."
            (pp_comma_list pp_print_int)
            codes
        else pp_err lexical "Unknown symbol."
    | NoCallCandidate (name, types) ->
        pp_err typing
          "No subprogram declaration matches the invocation:@ %s(%a)." name
          (pp_comma_list pp_ty) types
    | BadTypesForBinop (op, t1, t2) ->
        pp_err typing "Illegal application of operator %s on types@ %a@ and %a."
          (binop_to_string op) pp_ty t1 pp_ty t2
    | ImpureExpression (e, ses) ->
        pp_err typing
          "a pure expression was expected,@ found %a,@ which@ produces@ the@ \
           following@ side-effects:@ %a."
          pp_expr e SideEffect.SES.pp_print ses
    | MismatchedPurity s ->
        pp_err typing "expected@ a@ %s@ expression/subprogram." s
    | UnreconcilableTypes (t1, t2) ->
        pp_err typing
          "cannot@ find@ a@ common@ ancestor@ to@ those@ two@ types@ %a@ and@ \
           %a."
          pp_ty t1 pp_ty t2
    | AssignToImmutable x ->
        pp_err typing "cannot@ assign@ to@ immutable@ storage@ %S." x
    | AssignToTupleElement tuple_e ->
        pp_err typing "cannot@ assign@ to@ the@ (immutable)@ tuple@ value@ %a."
          pp_lexpr tuple_e
    | AlreadyDeclaredIdentifier x ->
        pp_err typing "cannot@ declare@ already@ declared@ element@ %S." x
    | BadReturnStmt None ->
        pp_err typing "cannot return something from a procedure."
    | UnexpectedSideEffect s -> pp_err dynamic "Unexpected side-effect: %s." s
    | UncaughtException s -> pp_err dynamic "Uncaught exception: %s." s
    | OverlappingSlices (slices, t) ->
        pp_err
          (error_handling_time_to_string t)
          "overlapping slices@ @[%a@]." pp_slice_list slices
    | BadLDI ldi ->
        pp_err typing "Unsupported declaration:@ @[%a@]." pp_local_decl_item ldi
    | BadRecursiveDecls decls ->
        pp_err typing "multiple recursive declarations:@ @[%a@]."
          (pp_comma_list (fun f -> fprintf f "%S"))
          decls
    | UnrespectedParserInvariant -> pp_err typing "Parser invariant broke."
    | ConstrainedIntegerExpected t ->
        pp_err typing "constrained@ integer@ expected,@ provided@ %a." pp_ty t
    | ParameterWithoutDecl s ->
        pp_err typing
          "explicit@ parameter@ %S@ does@ not@ have@ a@ corresponding@ \
           defining@ argument."
          s
    | BadParameterDecl (name, expected, actual) ->
        pp_err typing
          "incorrect@ parameter@ declaration@ for@ %S,@ expected@ @[{%a}@]@ \
           but@ @[{%a}@]@ provided"
          name
          (pp_comma_list pp_print_string)
          expected
          (pp_comma_list pp_print_string)
          actual
    | ArbitraryEmptyType t ->
        pp_err dynamic "ARBITRARY of empty type %a." pp_ty t
    | BaseValueEmptyType t ->
        pp_err typing "base value of empty type %a." pp_ty t
    | BaseValueNonSymbolic (t, e) ->
        pp_err typing
          "base@ value@ of@ type@ %a@ cannot@ be@ symbolically@ reduced@ \
           since@ it@ consists@ of@ %a."
          pp_ty t pp_expr e
    | BadATC (t1, t2) ->
        pp_err typing
          "cannot@ perform@ Asserted@ Type@ Conversion@ on@ %a@ by@ %a." pp_ty
          t1 pp_ty t2
    | DynamicATCFailure (value, ty) ->
        pp_err dynamic "value %s does not satisfy the asserted type %a." value
          pp_type_desc ty
    | SetterWithoutCorrespondingGetter func ->
        let ret, args =
          match func.args with
          | (_, ret) :: args -> (ret, List.map snd args)
          | _ -> assert false
        in
        pp_err typing
          "setter@ \"%s\"@ does@ not@ have@ a@ corresponding@ getter@ of@ \
           signature@ @[@[%a@]@ ->@ %a@]."
          func.name (pp_comma_list pp_ty) args pp_ty ret
    | BadPattern (p, t) ->
        pp_err typing "Erroneous@ pattern@ %a@ for@ expression@ of@ type@ %a."
          pp_pattern p pp_ty t
    | UnreachableReached -> pp_err dynamic "unreachable reached."
    | NonReturningFunction name ->
        pp_err typing "not all control flow paths of the function %S@ %a." name
          pp_print_text
          "are guaranteed to either return, raise an exception, or invoke \
           unreachable"
    | NoreturnViolation name ->
        pp_err typing "the@ function %S@ %a." name pp_print_text
          "is qualified with noreturn but may return on some control flow path"
    | RecursionLimitReached t ->
        pp_err (error_handling_time_to_string t) "recursion limit reached."
    | LoopLimitReached -> pp_err dynamic "loop limit reached."
    | ConflictingSideEffects (s1, s2) ->
        pp_err typing "conflicting side effects %a and %a" SideEffect.pp_print
          s1 SideEffect.pp_print s2
    | ConstantTimeBroken (e, ses) ->
        pp_err typing
          "expected@ constant-time@ expression,@ got@ %a,@ which@ produces@ \
           the@ following@ side-effects:@ %a."
          pp_expr e SideEffect.SES.pp_print ses
    | BadReturnStmt (Some t) ->
        pp_err typing
          "cannot@ return@ nothing@ from@ a@ function,@ an@ expression@ of@ \
           type@ %a@ is@ expected."
          pp_ty t
    | EmptyConstraints ->
        pp_err typing
          "a well-constrained integer cannot have empty constraints."
    | ExpectedSingularType t ->
        pp_err typing "%a@ %a." pp_print_text "expected singular type, found"
          pp_ty t
    | ExpectedNamedType t ->
        pp_err typing "%a@ %a." pp_print_text "expected a named type, found"
          pp_ty t
    | UnexpectedPendingConstrained ->
        pp_err typing "a pending constrained integer is illegal here."
    | BitfieldsDontAlign
        { field1_absname; field2_absname; field1_absslices; field2_absslices }
      ->
        pp_err typing
          "bitfields `%s` and `%s` are in the same scope but define different \
           slices of the containing bitvector type: %s and %s, respectively."
          field1_absname field2_absname field1_absslices field2_absslices
    | UnexpectedInitialisationThrow (exception_ty, global_storage_element_name)
      ->
        pp_err dynamic
          "unexpected@ exception@ %a@ thrown@ during@ the@ evaluation@ of@ \
           the@ initialisation@ of@ the global@ storage@ element@ %S."
          pp_ty exception_ty global_storage_element_name
    | PrecisionLostDefining ->
        pp_err typing
          "type@ used@ to@ define@ storage@ item@ is@ the@ result@ of@ \
           precision@ loss."
    | NegativeArrayLength (e_length, length) ->
        pp_err dynamic
          "array@ length@ expression@ %a@ has@ negative@ length@a: %i." pp_expr
          e_length length
    | MultipleWrites id -> pp_err parse "multiple@ writes@ to@ %S." id
    | MultipleImplementations (impl1, impl2) ->
        pp_err typing
          "multiple@ overlapping@ `implementation`@ functions@ for@ %s:@ %a"
          impl1.desc.name (pp_print_list pp_pos) [ impl1; impl2 ]
    | NoOverrideCandidate ->
        pp_err typing "no `impdef` for `implementation` function."
    | UnexpectedCollection -> pp_err typing "unexpected collection."
    | TooManyOverrideCandidates impdefs ->
        pp_err typing
          "multiple@ `impdef`@ candidates@ for@ `implementation`:@ %a"
          (pp_print_list pp_pos) impdefs
    | BadPrimitiveArgument (name, reason) ->
        pp_err dynamic "%s (primitive) expected an argument %s" name reason
    | NoEntryPoint ->
        pp_err dynamic "%a" pp_print_text
          "no entrypoint supplied. Have you defined `func main() => integer`, \
           or did you mean to pass `--no-exec`?"
    | ObsoleteSyntax fmt -> pp_err parse "Obsolete syntax:@ @[%t@]" fmt

  let fprintf_warn f =
    kdprintf (fun msg -> fprintf f "@[ASL Warning:@ %t@]" msg)

  let pp_warning_desc formatter w =
    let pp_warn format_string = fprintf_warn formatter format_string in
    match w.desc with
    | NoRecursionLimit [ name ] ->
        pp_warn "the recursive function %s%a" name pp_print_text
          " has no recursive limit annotation."
    | NoRecursionLimit li ->
        pp_warn "the mutually-recursive functions @[%a@]%a"
          (pp_comma_list pp_print_string)
          li pp_print_text " have no recursive limit annotation."
    | NoLoopLimit -> pp_warn "%a" pp_print_text "Loop does not have a limit."
    | ConstraintSetPairToBigToBeExploded { op; left; right; log_max } ->
        pp_warn "%a@ %s@ %a%d@ with@ constraints@ %a@ and@ %a.@ %a"
          pp_print_text "Exploding sets for the binary operation"
          (binop_to_string op) pp_print_text
          "could result in a constraint set bigger than 2^" log_max
          PP.pp_int_constraints left PP.pp_int_constraints right pp_print_text
          "Continuing with the non-expanded constraints."
    | IntervalTooBigToBeExploded (za, zb) ->
        pp_warn
          "Interval too large: @[<h>[ %a .. %a ]@].@ Keeping it as an interval."
          Z.pp_print za Z.pp_print zb
    | RemovingValuesFromConstraints { op; prev; after } ->
        pp_warn
          "Removing@ some@ values@ that@ would@ fail@ with@ op %s@ from@ \
           constraint@ set@ @[<h>{%a}@]@ gave@ @[<h>{%a}@].@ Continuing@ with@ \
           this@ constraint@ set."
          (binop_to_string op) PP.pp_int_constraints prev PP.pp_int_constraints
          after
    | PragmaUse id -> pp_warn "pragma %s%a" id pp_print_text " will be ignored."
    | UnexpectedImplementation ->
        pp_warn "%a" pp_print_text "Unexpected `implementation` function."
    | MissingOverride ->
        pp_warn "%a" pp_print_text
          "Missing `implementation` for `impdef` function."

  let pp_pos_begin f pos =
    match PrintContext.display_error_context pos with
    | None when ASTUtils.is_dummy_pos pos -> ()
    | None -> fprintf f "@[<h>%a:@]@ " pp_pos pos
    | Some ctx -> fprintf f "@[<h>%a:@]@ %s@ " pp_pos pos ctx

  let pp_error f e = fprintf f "@[<v 0>%a%a@]" pp_pos_begin e pp_error_desc e

  let pp_warning f e =
    fprintf f "@[<v 0>%a%a@]" pp_pos_begin e pp_warning_desc e

  let error_desc_to_string e = asprintf "%a" pp_error_desc e

  let desc_to_string_inf pp_desc =
    asprintf "%a" @@ fun f e ->
    pp_set_margin f 1_000_000_000;
    pp_desc f e

  let error_to_string = asprintf "%a" pp_error
end

include PPrint

module CSV = struct
  let error_label = function
    | ReservedIdentifier _ -> "ReservedIdentifier"
    | BadField _ -> "BadField"
    | BadPattern _ -> "BadPattern"
    | MissingField _ -> "MissingField"
    | BadSlices _ -> "BadSlices"
    | BadIndex _ -> "BadIndex"
    | BadSlice _ -> "BadSlice"
    | EmptySlice -> "EmptySlice"
    | TypeInferenceNeeded -> "TypeInferenceNeeded"
    | UndefinedIdentifier _ -> "UndefinedIdentifier"
    | MismatchedCallType _ -> "MismatchedCallType"
    | BadArity _ -> "BadArity"
    | BadParameterArity _ -> "BadParameterArity"
    | UnsupportedBinop _ -> "UnsupportedBinop"
    | UnsupportedUnop _ -> "UnsupportedUnop"
    | UnsupportedExpr _ -> "UnsupportedExpr"
    | UnsupportedTy _ -> "UnsupportedTy"
    | InvalidExpr _ -> "InvalidExpr"
    | MismatchType _ -> "MismatchType"
    | ConflictingTypes _ -> "ConflictingTypes"
    | AssertionFailed _ -> "AssertionFailed"
    | CannotParse _ -> "CannotParse"
    | UnknownSymbol _ -> "UnknownSymbol"
    | NoCallCandidate _ -> "NoCallCandidate"
    | BadTypesForBinop _ -> "BadTypesForBinop"
    | ImpureExpression _ -> "ImpureExpression"
    | MismatchedPurity _ -> "MismatchedPurity"
    | UnreconcilableTypes _ -> "UnreconcilableTypes"
    | AssignToImmutable _ -> "AssignToImmutable"
    | AssignToTupleElement _ -> "AssignToTupleElement"
    | AlreadyDeclaredIdentifier _ -> "AlreadyDeclaredIdentifier"
    | BadReturnStmt _ -> "BadReturnStmt"
    | UnexpectedSideEffect _ -> "UnexpectedSideEffect"
    | UncaughtException _ -> "UncaughtException"
    | OverlappingSlices _ -> "OverlappingSlices"
    | BadLDI _ -> "BadLDI"
    | BadRecursiveDecls _ -> "BadRecursiveDecls"
    | UnrespectedParserInvariant -> "UnrespectedParserInvariant"
    | BadATC _ -> "BadATC"
    | DynamicATCFailure _ -> "DynamicATCFailure"
    | ConstrainedIntegerExpected _ -> "ConstrainedIntegerExpected"
    | ParameterWithoutDecl _ -> "ParameterWithoutDecl"
    | BadParameterDecl _ -> "BadParameterDecl"
    | BaseValueEmptyType _ -> "BaseValueEmptyType"
    | ArbitraryEmptyType _ -> "ArbitraryEmptyType"
    | BaseValueNonSymbolic _ -> "BaseValueNonSymbolic"
    | SetterWithoutCorrespondingGetter _ -> "SetterWithoutCorrespondingGetter"
    | NonReturningFunction _ -> "NonReturningFunction"
    | NoreturnViolation _ -> "NoreturnViolation"
    | UnreachableReached -> "UnreachableReached"
    | LoopLimitReached -> "LoopLimitReached"
    | RecursionLimitReached _ -> "RecursionLimitReached"
    | EmptyConstraints -> "EmptyConstraints"
    | UnexpectedPendingConstrained -> "UnexpectedPendingConstrained"
    | BitfieldsDontAlign _ -> "BitfieldsDontAlign"
    | ExpectedSingularType _ -> "ExpectedSingularType"
    | ExpectedNamedType _ -> "ExpectedNamedType"
    | ConflictingSideEffects _ -> "ConflictingSideEffects"
    | ConstantTimeBroken _ -> "ConstantTimeBroken"
    | MultipleWrites _ -> "MultipleWrites"
    | UnexpectedInitialisationThrow _ -> "UnexpectedInitialisationThrow"
    | NegativeArrayLength _ -> "NegativeArrayLength"
    | MultipleImplementations _ -> "ClashingImplementations"
    | NoOverrideCandidate -> "NoOverrideCandidate"
    | TooManyOverrideCandidates _ -> "TooManyOverrideCandidates"
    | PrecisionLostDefining -> "PrecisionLostDefining"
    | UnexpectedCollection -> "UnexpectedCollection"
    | BadPrimitiveArgument _ -> "BadPrimitiveArgument"
    | NoEntryPoint -> "NoEntryPoint"
    | ObsoleteSyntax _ -> "ObsoleteSyntax"

  let warning_label = function
    | NoLoopLimit -> "NoLoopLimit"
    | IntervalTooBigToBeExploded _ -> "IntervalTooBigToBeExploded"
    | ConstraintSetPairToBigToBeExploded _ ->
        "ConstraintSetPairToBigToBeExploded"
    | RemovingValuesFromConstraints _ -> "RemovingValuesFromConstraints"
    | NoRecursionLimit _ -> "NoRecursionLimit"
    | PragmaUse _ -> "PragmaUse"
    | UnexpectedImplementation -> "UnexpectedImplementation"
    | MissingOverride -> "MissingOverride"

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

  let pp_csv pp_desc label =
    let pos_in_line pos = Lexing.(pos.pos_cnum - pos.pos_bol) in
    fun f pos ->
      Printf.fprintf f "\"%s\",%d,%d,%d,%d,%s,\"%s\""
        (escape pos.pos_start.pos_fname)
        pos.pos_start.pos_lnum
        (pos_in_line pos.pos_start)
        pos.pos_end.pos_lnum (pos_in_line pos.pos_end) (label pos.desc)
        (desc_to_string_inf pp_desc pos |> escape)

  let pp_error f e = pp_csv pp_error_desc error_label f e
  let pp_warning f w = pp_csv pp_warning_desc warning_label f w
end

module GNU = struct
  let pp pp_desc =
    let pos_in_line pos = Lexing.(pos.pos_cnum - pos.pos_bol) in
    fun f pos ->
      Printf.fprintf f "aslref: %s:%d:%d: %s" pos.pos_start.pos_fname
        pos.pos_start.pos_lnum
        (pos_in_line pos.pos_start)
        (desc_to_string_inf pp_desc pos)
end

type output_format = HumanReadable | CSV | GNU

module type ERROR_PRINTER_CONFIG = sig
  val output_format : output_format
end

module ErrorPrinter (C : ERROR_PRINTER_CONFIG) = struct
  let eprintln e =
    match C.output_format with
    | HumanReadable -> Format.eprintf "@[<2>%a@]@." pp_error e
    | CSV -> Printf.eprintf "%a\n" CSV.pp_error e
    | GNU -> Printf.eprintf "%a\n" (GNU.pp pp_error_desc) e

  let warn w =
    match C.output_format with
    | HumanReadable -> Format.eprintf "@[<2>%a@]@." pp_warning w
    | CSV -> Printf.eprintf "%a\n" CSV.pp_warning w
    | GNU -> Printf.eprintf "%a\n" (GNU.pp pp_warning_desc) w

  let warn_from ~loc w = ASTUtils.add_pos_from loc w |> warn
end

let () =
  Printexc.register_printer @@ function
  | ASLException e -> Some (error_to_string e)
  | _ -> None
