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

(** Failures that evaluation may encounter when it runs without full type
    checking. They have no error codes. *)
type unchecked_execution_error =
  | TypeMismatch of string * type_desc list
      (** The displayed runtime value and the types it was expected to have. *)
  | TypeInferenceNeeded
      (** Type information normally resolved during typing is absent. *)
  | MissingIdentifier of identifier
      (** An identifier normally resolved during typing is absent. *)
  | ArgumentArityMismatch of {
      name : identifier;
      expected : int;
      provided : int;
    }  (** A call has a different number of arguments from its declaration. *)
  | ParameterArityMismatch of {
      name : identifier;
      expected : int;
      provided : int;
    }  (** A call has a different number of parameters from its declaration. *)
  | AssignmentArityMismatch of { expected : int; provided : int }
      (** A multi-assignment has a different number of targets and values. *)
  | EntrypointResultArityMismatch of {
      name : identifier;
      expected : int;
      provided : int;
    }
      (** The entrypoint (main function) returned a different number of values
          than required. *)
  | UnexpectedThrow of ty * expr
      (** A side-effect-free expression unexpectedly threw an exception. *)

module UncheckedExecutionError = struct
  type t = unchecked_execution_error

  let label = "UncheckedExecutionError"
  let pp_type_desc f ty = PP.pp_ty f (ASTUtils.add_dummy_pos ty)
  let pp_comma f () = Format.fprintf f ",@ "

  let pp f = function
    | TypeMismatch (v, [ ty ]) ->
        Format.fprintf f "Type mismatch:@ value %s does not belong to type %a."
          v pp_type_desc ty
    | TypeMismatch (v, tys) ->
        Format.fprintf f
          "Type mismatch:@ value %s does not belong to any of:@ %a." v
          (Format.pp_print_list ~pp_sep:pp_comma pp_type_desc)
          tys
    | TypeInferenceNeeded ->
        Format.fprintf f
          "Evaluation requires type information unavailable without type \
           checking."
    | MissingIdentifier s -> Format.fprintf f "Undefined identifier %S." s
    | ArgumentArityMismatch { name; expected; provided } ->
        Format.fprintf f
          "Call to %S has incorrect argument arity:@ expected %d argument(s); \
           provided %d."
          name expected provided
    | ParameterArityMismatch { name; expected; provided } ->
        Format.fprintf f
          "Call to %S has incorrect parameter arity:@ expected %d \
           parameter(s); provided %d."
          name expected provided
    | AssignmentArityMismatch { expected; provided } ->
        Format.fprintf f
          "Multi-assignment arity mismatch:@ expected %d value(s); provided %d."
          expected provided
    | EntrypointResultArityMismatch { name; expected; provided } ->
        Format.fprintf f "Entry point %S returned %d value(s); expected %d."
          name provided expected
    | UnexpectedThrow (ty, e) ->
        Format.fprintf f
          "Side-effect-free expression %a unexpectedly threw an exception of \
           type %a."
          PP.pp_expr e PP.pp_ty ty
end

type bad_binop_priority =
  | NonAssociativeBinop of binop
  | SamePriorityBinops of binop * binop

type bad_declaration =
  | LocalDeclarationWithoutName
  | LocalTupleDeclarationWithoutName
  | GlobalDeclarationWithoutName
  | LocalConstantDeclaration
  | EmptyRecordTypeDeclaration
  | EmptyExceptionTypeDeclaration
  | EmptyCollectionTypeDeclaration
  | ElidedParameterWithoutBitvectorType

type static_evaluation_failure_reason =
  | IndexOutOfBounds of int * int
  | ValueOutsideAssertedType of string * type_desc

(** Diagnostics specific to ASLv0. *)
type v0_error =
  | EmptySlice
  | InvalidExpr of expr
  | ParameterWithoutDecl of identifier
  | SetterWithoutCorrespondingGetter of func

module V0Error = struct
  type t = v0_error

  let label = function
    | EmptySlice -> "V0EmptySlice"
    | InvalidExpr _ -> "V0InvalidExpr"
    | ParameterWithoutDecl _ -> "V0ParameterWithoutDecl"
    | SetterWithoutCorrespondingGetter _ -> "V0SetterWithoutCorrespondingGetter"
end

(** Violations of invariants between internal processing stages. They are not
    ASL errors and therefore have no error codes. *)
type internal_invariant_error =
  | TypedArrayExpressionInAnnotation
  | EmptyWellConstrainedInteger
  | UninitialisedImmutableLocal
  | V0SetterWithoutValueArgument
  | GlobalWithoutTypeOrInitialiser
  | ParameterizedIntegerAtRuntime

module InternalInvariantError = struct
  type t = internal_invariant_error

  let label = "InternalInvariantError"

  let pp f = function
    | TypedArrayExpressionInAnnotation ->
        Format.fprintf f
          "A typed-only array expression reached expression annotation."
    | EmptyWellConstrainedInteger ->
        Format.fprintf f "A well-constrained integer type has no constraints."
    | UninitialisedImmutableLocal ->
        Format.fprintf f "An immutable local declaration has no initialiser."
    | V0SetterWithoutValueArgument ->
        Format.fprintf f "A setter has no value argument."
    | GlobalWithoutTypeOrInitialiser ->
        Format.fprintf f
          "A global declaration has neither a type nor an initialiser."
    | ParameterizedIntegerAtRuntime ->
        Format.fprintf f
          "A parameterized integer type reached dynamic evaluation."
end

(** Whether conflicting types arose from a general structural expectation or the
    specification's type-satisfaction check. *)
type conflicting_types_reason = UnexpectedType | TypeSatisfaction

type bad_slices =
  | NonPositiveLength of { slice : slice; length : int }
  | OutOfBitvectorBounds of slice list * int
  | NegativeStartOrLength of error_handling_time * slice list

type error_desc =
  | ReservedIdentifier of string
  | BadField of string * ty
  | MissingField of string list * ty
  | BadSlices of bad_slices
  | BadIndex of int * int
  | BadTupleIndex of int * int
  | V0Error of v0_error
  | UndefinedIdentifier of identifier
  | MismatchedCallType of {
      subprogram_name : string;
      expected_call_type : subprogram_type;
      found_call_type : subprogram_type;
    }
  | BadCallArity of { name : identifier; expected : int; provided : int }
  | BadTupleArity of { expected : int; actual : int }
  | BadParameterArity of {
      version : version;
      name : identifier;
      expected : int;
      provided : int;
    }
  | UnsupportedBinop of error_handling_time * binop * literal * literal
  | UnsupportedUnop of error_handling_time * unop * literal
  | StaticEvaluationFailure of expr * static_evaluation_failure_reason option
  | ImplementationIntegerOverflow of Z.t
  | BadParameterType of ty
  | UncheckedExecutionError of error_handling_time * unchecked_execution_error
  | ConflictingTypes of conflicting_types_reason * type_desc list * ty
  | AssertionFailed of expr
  | CannotParse of string option
  | UnknownSymbol of string * string option
  | NoCallCandidate of string * ty list
  | BadTypesForBinop of binop * ty * ty
  | ImpureExpression of expr * SideEffect.SES.t
      (** used for fine-grained analysis *)
  | MismatchedPurity of string  (** Used for coarse-grained analysis *)
  | MismatchedBitvectorWidths of ty * ty
  | ExpectedBitvectorType of ty
  | CollectionBaseNotVariable of expr
  | NoCommonAncestor of ty * ty
  | AssignToImmutable of string
  | AssignToTupleElement of lexpr
  | AlreadyDeclaredIdentifier of string
  | BadReturnStmt of ty option
  | UncaughtException of string
  | OverlappingSlices of slice list * error_handling_time
  | BadLDI of AST.local_decl_item
  | BadRecursiveDecls of identifier list
  | InternalInvariantError of internal_invariant_error
  | BadATC of ty * ty  (** asserting, asserted *)
  | DynamicATCFailure of string * type_desc
  | BadPattern of pattern * ty
  | ConstrainedIntegerExpected of ty
  | BadParameterDecl of identifier * identifier list * identifier list
      (** name, expected, actual *)
  | BadParameterExpr of expr
  | BaseValueEmptyType of ty
  | ArbitraryEmptyType of ty
  | BaseValueNonSymbolic of ty * expr
  | NonReturningFunction of identifier
  | NoreturnViolation of identifier
  | ConflictingSideEffects of SideEffect.t * SideEffect.t
  | UnreachableReached
  | LoopLimitReached
  | RecursionLimitReached of error_handling_time
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
  | BadBinopPriority of bad_binop_priority
  | BadDeclarationSyntax of bad_declaration

type error = error_desc annotated

exception ASLException of error

type 'a result = ('a, error) Result.t

let unknown_symbol id = UnknownSymbol (id, None)

let unknown_symbol_with_alternative ~lexeme ~alternative_lexeme =
  UnknownSymbol (lexeme, Some alternative_lexeme)

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
    (********** Build errors **********)
    | BadBinopPriority _ -> Some (Build BOP)
    | BadDeclarationSyntax _ -> Some (Build BD)
    | CannotParse _ -> Some (Build PE)
    | MultipleWrites _ -> Some (Build PE)
    | ObsoleteSyntax _ -> Some (Build PE)
    | ReservedIdentifier _ -> Some (Build RI)
    | UnknownSymbol _ -> Some (Build LE)
    (********** Typing errors **********)
    | AlreadyDeclaredIdentifier _ -> Some (Typing IAD)
    | AssignToImmutable _ -> Some (Typing AIM)
    | AssignToTupleElement _ -> Some (Typing UT)
    | BadATC _ -> Some (Typing TAF)
    | BadCallArity _ -> Some (Typing BC)
    | BadField _ -> Some (Typing BF)
    | BadLDI _ -> Some (Typing BD)
    | BadParameterArity _ -> Some (Typing BC)
    | BadParameterDecl _ | BadParameterExpr _ | BadParameterType _ ->
        Some (Typing BSPD)
    | BadPattern _ -> Some (Typing BO)
    | BadRecursiveDecls _ -> Some (Typing BD)
    | BadReturnStmt _ -> Some (Typing BSPD)
    | BadSlices (NonPositiveLength _)
    | BadSlices (OutOfBitvectorBounds _)
    | BadSlices (NegativeStartOrLength (Static, _)) ->
        Some (Typing BS)
    | BadTupleArity _ -> Some (Typing UT)
    | BadTupleIndex _ -> Some (Typing BTI)
    | BadTypesForBinop _ -> Some (Typing BO)
    | BaseValueEmptyType _ | BaseValueNonSymbolic _ -> Some (Typing NBV)
    | BitfieldsDontAlign _ -> Some (Typing BS)
    | CollectionBaseNotVariable _ -> Some (Typing UT)
    | ConflictingSideEffects _ -> Some (Typing SEV)
    | ConflictingTypes (TypeSatisfaction, _, _) -> Some (Typing TSF)
    | ConflictingTypes (UnexpectedType, _, _) -> Some (Typing UT)
    | ConstantTimeBroken _ -> Some (Typing SEV)
    | ConstrainedIntegerExpected _ -> Some (Typing UT)
    | ExpectedBitvectorType _ | ExpectedNamedType _ | ExpectedSingularType _ ->
        Some (Typing UT)
    | ImpureExpression _ -> Some (Typing SEV)
    | MismatchedBitvectorWidths _ -> Some (Typing UT)
    | MismatchedCallType _ -> Some (Typing BC)
    | MismatchedPurity _ -> Some (Typing SEV)
    | MissingField _ -> Some (Typing BF)
    | MultipleImplementations _ -> Some (Typing OE)
    | NoCallCandidate _ -> Some (Typing BC)
    | NoCommonAncestor _ -> Some (Typing LCA)
    | NonReturningFunction _ -> Some (Typing BSPD)
    | NoOverrideCandidate -> Some (Typing OE)
    | NoreturnViolation _ -> Some (Typing BSPD)
    | OverlappingSlices (_, Static) -> Some (Typing BS)
    | PrecisionLostDefining -> Some (Typing PLD)
    | RecursionLimitReached Static -> Some (Typing SEF)
    | StaticEvaluationFailure _ -> Some (Typing SEF)
    | TooManyOverrideCandidates _ -> Some (Typing OE)
    | UndefinedIdentifier _ -> Some (Typing UI)
    | UnexpectedCollection | UnexpectedPendingConstrained -> Some (Typing UT)
    | UnsupportedBinop (Static, _, _, _) | UnsupportedUnop (Static, _, _) ->
        Some (Typing BO)
    | V0Error EmptySlice -> Some (Typing BS)
    (********** Dynamic errors **********)
    | ArbitraryEmptyType _ -> Some (Dynamic AET)
    | AssertionFailed _ -> Some (Dynamic DAF)
    | BadIndex _ -> Some (Dynamic BI)
    | BadPrimitiveArgument _ -> Some (Dynamic DAF)
    | BadSlices (NegativeStartOrLength (Dynamic, _)) -> Some (Dynamic BI)
    | DynamicATCFailure _ -> Some (Dynamic TAF)
    | LoopLimitReached -> Some (Dynamic LE)
    | NegativeArrayLength _ -> Some (Dynamic NAL)
    | NoEntryPoint -> Some (Dynamic NEP)
    | OverlappingSlices (_, Dynamic) -> Some (Dynamic OSA)
    | RecursionLimitReached Dynamic -> Some (Dynamic LE)
    | UncaughtException _ -> Some (Dynamic UE)
    | UnexpectedInitialisationThrow _ -> Some (Dynamic UE)
    | UnreachableReached -> Some (Dynamic UNR)
    | UnsupportedBinop (Dynamic, _, _, _) | UnsupportedUnop (Dynamic, _, _) ->
        Some (Dynamic BO)
    (********** Errors without specification codes **********)
    (* Implementation limitations are not ASL errors and have no specification
       error code. *)
    | ImplementationIntegerOverflow _ -> None
    (* Internal invariant violations are not ASL errors. *)
    | InternalInvariantError _ -> None
    (* When type checking is skipped, evaluation can lack inferred type
       information or encounter mismatches that would normally be reported
       during typing. The specification therefore assigns these failures no
       dynamic error code. *)
    | UncheckedExecutionError _ -> None
    (* These ASLv0 diagnostics have no ASLv1 specification error codes. *)
    | V0Error
        ( InvalidExpr _ | ParameterWithoutDecl _
        | SetterWithoutCorrespondingGetter _ ) ->
        None
end

(** [fatal_from_static_evaluation e cause] converts the cause of an exception
    raised while statically evaluating [e] into a typing error [TE_SEF] so that
    it is not classied as a dynamic error. Details are retained for an
    out-of-bounds index and a failed asserted type conversion; other dynamic
    errors have no additional reason. Build errors, typing errors, and errors
    without a specification code are re-raised unchanged. *)
let fatal_from_static_evaluation e cause =
  match ErrorCode.of_error cause with
  | Some ErrorCode.(Dynamic (BI | TAF)) ->
      let reason =
        match cause.desc with
        | BadIndex (index, length) -> Some (IndexOutOfBounds (index, length))
        | DynamicATCFailure (value, ty) ->
            Some (ValueOutsideAssertedType (value, ty))
        | _ -> None
      in
      fatal_from e (StaticEvaluationFailure (e, reason))
  | Some ErrorCode.(Dynamic (UNR | DAF | LE | AET | BO | UE | OSA | NAL | NEP))
    ->
      fatal_from e (StaticEvaluationFailure (e, None))
  | Some ErrorCode.(Build _ | Typing _) | None -> fatal cause

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

module PPrint = struct
  open Format
  open PP

  let pp_comma_list pp_elt f li =
    pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") pp_elt f li

  let pp_type_desc f ty = pp_ty f (ASTUtils.add_dummy_pos ty)

  module ErrorPhase = struct
    type t = Lexical | Grammar | Static | Typing | Dynamic | Internal

    let to_string = function
      | Lexical -> "Lexical"
      | Grammar -> "Grammar"
      | Static -> "Static"
      | Typing -> "Type"
      | Dynamic -> "Dynamic"
      | Internal -> "Internal"
  end

  type error_phase = ErrorPhase.t

  let fprintf_err f phase code_opt =
    let () =
      let open ErrorCode in
      match code_opt with
      | Some (Typing _) ->
          assert (phase = ErrorPhase.Typing || phase = ErrorPhase.Static)
      | Some (Build _) ->
          assert (
            phase = ErrorPhase.Lexical || phase = ErrorPhase.Grammar
            || phase = ErrorPhase.Static)
      | Some (Dynamic _) -> assert (phase = ErrorPhase.Dynamic)
      | None -> ()
    in
    let pp_code fmt code = fprintf fmt " (%s)" (ErrorCode.to_string code) in
    kdprintf (fun msg ->
        fprintf f "@[<hov 2>ASL %s error%a:@ %t@]"
          (ErrorPhase.to_string phase)
          (pp_print_option pp_code) code_opt msg)

  let error_handling_time_to_phase : error_handling_time -> error_phase =
    function
    | Static -> ErrorPhase.Static
    | Dynamic -> ErrorPhase.Dynamic

  let pp_bad_index f (index, length) =
    fprintf f "Index %d is outside the valid range 0..%d." index (length - 1)

  let pp_value_outside_asserted_type f (value, ty) =
    fprintf f "Value %s does not satisfy the asserted type %a." value
      pp_type_desc ty

  let pp_error_desc f e =
    let pp_err s fmt = fprintf_err f s (ErrorCode.of_error e) fmt in
    let open ErrorPhase in
    match e.desc with
    | ReservedIdentifier id -> pp_err Lexical "%S is a reserved keyword." id
    | UnsupportedBinop (t, op, v1, v2) ->
        pp_err
          (error_handling_time_to_phase t)
          "Operator %s is not defined for values@ %a@ and %a."
          (binop_to_string op) pp_literal v1 pp_literal v2
    | UnsupportedUnop (t, op, v) ->
        pp_err
          (error_handling_time_to_phase t)
          "Operator %s is not defined for value@ %a." (unop_to_string op)
          pp_literal v
    | StaticEvaluationFailure (e, None) ->
        pp_err Typing "Static evaluation of expression %a failed." pp_expr e
    | StaticEvaluationFailure (e, Some (IndexOutOfBounds (index, length))) ->
        pp_err Typing "Static evaluation of expression %a failed:@ %a" pp_expr e
          pp_bad_index (index, length)
    | StaticEvaluationFailure (e, Some (ValueOutsideAssertedType (value, ty)))
      ->
        pp_err Typing "Static evaluation of expression %a failed:@ %a" pp_expr e
          pp_value_outside_asserted_type (value, ty)
    | ImplementationIntegerOverflow z ->
        pp_err Internal "Integer %a exceeds aslref implementation limits."
          Z.pp_print z
    | BadParameterType ty ->
        pp_err Typing "Type %a is not supported in a subprogram signature."
          pp_ty ty
    | V0Error (InvalidExpr e) ->
        pp_err Typing "invalid expression %a." pp_expr e
    | UncheckedExecutionError (t, unchecked_error) ->
        pp_err
          (error_handling_time_to_phase t)
          "%a" UncheckedExecutionError.pp unchecked_error
    | BadField (s, ty) -> pp_err Typing "Type %a has no field %S." pp_ty ty s
    | MissingField (fields, ty) ->
        pp_err Typing
          "Cannot create a value of type %a with the provided fields:@ %a."
          pp_ty ty
          (pp_comma_list (fun f -> fprintf f "%S"))
          fields
    | V0Error EmptySlice ->
        assert (e.version = V0);
        pp_err Static
          "cannot slice with empty slicing operator. This might also be due to \
           an incorrect getter/setter invocation."
    | BadSlices (NegativeStartOrLength (t, slices)) ->
        pp_err
          (error_handling_time_to_phase t)
          "Slice %a is invalid: its start and length must be non-negative."
          pp_slice_list slices
    | BadSlices (OutOfBitvectorBounds (slices, length)) ->
        pp_err Static
          "Slice %a is outside the bounds of a bitvector of length %d."
          pp_slice_list slices length
    | BadIndex (index, length) ->
        pp_err Dynamic "%a" pp_bad_index (index, length)
    | BadTupleIndex (index, length) ->
        pp_err Typing "Tuple index %d is outside the valid range 0..%d." index
          (length - 1)
    | BadSlices (NonPositiveLength { slice; length }) ->
        pp_err Static
          "Slice %a has length %d; slice lengths must be at least 1." pp_slice
          slice length
    | UndefinedIdentifier s -> pp_err Static "Undefined identifier %S." s
    | MismatchedCallType
        { subprogram_name = s; expected_call_type; found_call_type } ->
        let call_type_description call_type =
          match call_type with
          | ST_Function -> "function"
          | ST_Getter -> "getter"
          | ST_Setter -> "setter"
          | ST_Procedure -> "procedure"
        in
        pp_err Static
          "Call to subprogram %S has the wrong call type:@ expected a %s; \
           found a %s."
          s
          (call_type_description expected_call_type)
          (call_type_description found_call_type)
    | BadCallArity { name; expected; provided } ->
        pp_err Typing
          "Call to %S has incorrect argument arity:@ expected %d argument(s); \
           provided %d."
          name expected provided
    | BadTupleArity { expected; actual } ->
        pp_err Typing
          "Tuple arity mismatch:@ expected %d element(s); provided %d." expected
          actual
    | BadParameterArity { version; name; expected; provided } -> (
        match version with
        | V0 ->
            pp_err Static
              "Could not infer all parameters while calling '%s':@ %d \
               parameters expected and %d inferred"
              name expected provided
        | V1 ->
            pp_err Static
              "Call to %S has incorrect parameter arity:@ expected %d \
               parameter(s); provided %d."
              name expected provided)
    | ConflictingTypes (_, [ expected ], provided) ->
        pp_err Typing "Expected a subtype of@ %a;@ provided %a." pp_type_desc
          expected pp_ty provided
    | ConflictingTypes (_, expected, provided) ->
        pp_err Typing "Type %a is not a subtype of any of:@ %a." pp_ty provided
          (pp_comma_list pp_type_desc)
          expected
    | AssertionFailed e -> pp_err Dynamic "Assertion failed:@ %a." pp_expr e
    | CannotParse s -> (
        match s with
        | None -> pp_err Grammar "Cannot parse."
        | Some s -> pp_err Grammar "Cannot parse.@ %a" pp_print_text s)
    | UnknownSymbol (s, alternative_symbol_opt) ->
        let pp_alternative_symbol f = function
          | None -> ()
          | Some alternative_symbol ->
              fprintf f "@ Did you mean `%s`?" alternative_symbol
        in
        let codes = List.map Char.code (List.of_seq (String.to_seq s)) in
        let not_printable code = code < 33 || code > 126 in
        if String.length s = 0 then
          pp_err Lexical "Unknown symbol.%a" pp_alternative_symbol
            alternative_symbol_opt
        else if List.exists not_printable codes then
          pp_err Lexical "Unknown symbol with byte value(s): %a.%a"
            (pp_comma_list pp_print_int)
            codes pp_alternative_symbol alternative_symbol_opt
        else
          pp_err Lexical "Unknown symbol %S.%a" s pp_alternative_symbol
            alternative_symbol_opt
    | NoCallCandidate (name, types) ->
        pp_err Typing
          "No subprogram declaration matches the invocation:@ %s(%a)." name
          (pp_comma_list pp_ty) types
    | BadTypesForBinop (op, t1, t2) ->
        pp_err Typing "Operator %s is not defined for types@ %a@ and %a."
          (binop_to_string op) pp_ty t1 pp_ty t2
    | ImpureExpression (e, ses) ->
        pp_err Typing
          "Expected a pure expression,@ but %a has the following side \
           effects:@ %a."
          pp_expr e SideEffect.SES.pp_print ses
    | MismatchedPurity s ->
        pp_err Typing "Expected a %s expression or subprogram." s
    | MismatchedBitvectorWidths (t1, t2) ->
        pp_err Typing "Bitvector types %a and %a must have equal widths." pp_ty
          t1 pp_ty t2
    | ExpectedBitvectorType ty ->
        pp_err Typing "Expected a bitvector type; provided %a." pp_ty ty
    | CollectionBaseNotVariable e ->
        pp_err Typing
          "Collection fields can only be accessed through a variable;@ \
           provided base: %a."
          pp_expr e
    | NoCommonAncestor (t1, t2) ->
        pp_err Typing "Types %a and %a have no common ancestor." pp_ty t1 pp_ty
          t2
    | AssignToImmutable x ->
        pp_err Typing "Cannot assign to immutable storage %S." x
    | AssignToTupleElement tuple_e ->
        pp_err Typing "Cannot assign to an element of immutable tuple %a."
          pp_lexpr tuple_e
    | AlreadyDeclaredIdentifier x ->
        pp_err Typing "Identifier %S is already declared." x
    | BadReturnStmt None -> pp_err Typing "A procedure cannot return a value."
    | UncaughtException s -> pp_err Dynamic "Uncaught exception: %s." s
    | OverlappingSlices (slices, t) ->
        pp_err
          (error_handling_time_to_phase t)
          "Slices @[%a@] overlap." pp_slice_list slices
    | BadLDI ldi ->
        pp_err Typing "Unsupported local declaration:@ @[%a@]."
          pp_local_decl_item ldi
    | BadRecursiveDecls decls ->
        pp_err Typing
          "Only subprogram declarations may be mutually recursive; cycle \
           contains:@ @[%a@]."
          (pp_comma_list (fun f -> fprintf f "%S"))
          decls
    | InternalInvariantError invariant_error ->
        pp_err Internal "%a" InternalInvariantError.pp invariant_error
    | ConstrainedIntegerExpected t ->
        pp_err Typing "Expected a constrained integer type; provided %a." pp_ty
          t
    | V0Error (ParameterWithoutDecl s) ->
        pp_err Typing
          "explicit@ parameter@ %S@ does@ not@ have@ a@ corresponding@ \
           defining@ argument."
          s
    | BadParameterDecl (name, expected, actual) ->
        pp_err Typing
          "Incorrect parameter declaration for %S:@ expected @[{%a}@];@ \
           provided @[{%a}@]."
          name
          (pp_comma_list pp_print_string)
          expected
          (pp_comma_list pp_print_string)
          actual
    | BadParameterExpr e ->
        pp_err Typing
          "Expression %a is not supported in a subprogram parameter definition."
          pp_expr e
    | ArbitraryEmptyType t ->
        pp_err Dynamic
          "ARBITRARY cannot produce a value of type@ %a@ because the type has \
           an empty domain."
          pp_ty t
    | BaseValueEmptyType t ->
        pp_err Typing "Cannot determine a base value for empty type %a." pp_ty t
    | BaseValueNonSymbolic (t, e) ->
        pp_err Typing
          "Cannot symbolically determine a base value for type@ %a@ from \
           expression@ %a."
          pp_ty t pp_expr e
    | BadATC (t1, t2) ->
        pp_err Typing
          "Cannot perform an asserted type conversion from@ %a@ to@ %a." pp_ty
          t1 pp_ty t2
    | DynamicATCFailure (value, ty) ->
        pp_err Dynamic "%a" pp_value_outside_asserted_type (value, ty)
    | V0Error (SetterWithoutCorrespondingGetter func) ->
        let ret, args =
          match func.args with
          | (_, ret) :: args -> (ret, List.map snd args)
          | _ -> assert false
        in
        pp_err Typing
          "setter@ \"%s\"@ does@ not@ have@ a@ corresponding@ getter@ of@ \
           signature@ @[@[%a@]@ ->@ %a@]."
          func.name (pp_comma_list pp_ty) args pp_ty ret
    | BadPattern (p, t) ->
        pp_err Typing "Pattern %a is incompatible with expression type %a."
          pp_pattern p pp_ty t
    | UnreachableReached ->
        pp_err Dynamic "Execution reached an unreachable statement."
    | NonReturningFunction name ->
        pp_err Typing "Not all control flow paths of function %S@ %a." name
          pp_print_text
          "are guaranteed to either return, raise an exception, or invoke \
           unreachable"
    | NoreturnViolation name ->
        pp_err Typing "Function %S@ %a." name pp_print_text
          "is qualified with noreturn but may return on some control flow path"
    | RecursionLimitReached t ->
        pp_err (error_handling_time_to_phase t) "Recursion limit reached."
    | LoopLimitReached -> pp_err Dynamic "Loop limit reached."
    | ConflictingSideEffects (s1, s2) ->
        pp_err Typing "Side effects %a and %a conflict." SideEffect.pp_print s1
          SideEffect.pp_print s2
    | ConstantTimeBroken (e, ses) ->
        pp_err Typing
          "Expected a constant-time expression,@ but %a has the following side \
           effects:@ %a."
          pp_expr e SideEffect.SES.pp_print ses
    | BadReturnStmt (Some t) ->
        pp_err Typing "A function must return a value of type %a." pp_ty t
    | ExpectedSingularType t ->
        pp_err Typing "Expected a singular type; provided %a." pp_ty t
    | ExpectedNamedType t ->
        pp_err Typing "Expected a named type; provided %a." pp_ty t
    | UnexpectedPendingConstrained ->
        pp_err Typing "A pending constrained integer is not permitted here."
    | BitfieldsDontAlign
        { field1_absname; field2_absname; field1_absslices; field2_absslices }
      ->
        pp_err Typing
          "Bitfields `%s` and `%s` are in the same scope but define different \
           slices of the containing bitvector type: %s and %s, respectively."
          field1_absname field2_absname field1_absslices field2_absslices
    | UnexpectedInitialisationThrow (exception_ty, global_storage_element_name)
      ->
        pp_err Dynamic
          "Unexpected exception@ %a@ was thrown while initialising global \
           storage element@ %S."
          pp_ty exception_ty global_storage_element_name
    | PrecisionLostDefining ->
        pp_err Typing
          "A storage item cannot be defined using a type that has lost \
           precision."
    | NegativeArrayLength (e_length, length) ->
        pp_err Dynamic
          "Array@ length@ expression@ %a@ evaluated@ to@ %i;@ array@ lengths@ \
           must@ be@ non-negative."
          pp_expr e_length length
    | MultipleWrites id ->
        pp_err Grammar "Storage element %S is written more than once." id
    | MultipleImplementations (impl1, impl2) ->
        pp_err Typing
          "Multiple overlapping `implementation` functions exist for %S:@ %a."
          impl1.desc.name (pp_print_list pp_pos) [ impl1; impl2 ]
    | NoOverrideCandidate ->
        pp_err Typing
          "No matching `impdef` declaration exists for the `implementation` \
           function."
    | UnexpectedCollection ->
        pp_err Typing "A collection type is not permitted here."
    | TooManyOverrideCandidates impdefs ->
        pp_err Typing
          "Multiple `impdef` declarations match the `implementation` \
           function:@ %a."
          (pp_print_list pp_pos) impdefs
    | BadPrimitiveArgument (name, reason) ->
        pp_err Dynamic "Primitive %S expected its argument to be %s." name
          reason
    | NoEntryPoint ->
        pp_err Dynamic
          "No entry point was supplied.@ Define `func main() => integer`,@ or \
           pass `--no-exec` to disable execution."
    | ObsoleteSyntax fmt -> pp_err Grammar "Obsolete syntax:@ @[%t@]" fmt
    | BadBinopPriority (NonAssociativeBinop op) ->
        pp_err Grammar
          "Binary operator `%s` is not associative; parenthesise to \
           disambiguate."
          (binop_to_string op)
    | BadBinopPriority (SamePriorityBinops (op1, op2)) ->
        pp_err Grammar
          "Operators `%s` and `%s` have the same priority; parenthesise to \
           disambiguate."
          (binop_to_string op1) (binop_to_string op2)
    | BadDeclarationSyntax LocalDeclarationWithoutName ->
        pp_err Grammar "A local declaration must declare a name."
    | BadDeclarationSyntax LocalTupleDeclarationWithoutName ->
        pp_err Grammar "A local declaration must declare at least one name."
    | BadDeclarationSyntax GlobalDeclarationWithoutName ->
        pp_err Grammar "A global declaration must declare a name."
    | BadDeclarationSyntax LocalConstantDeclaration ->
        pp_err Grammar
          "Local constant declarations are not valid ASL1. Did you mean `let`?"
    | BadDeclarationSyntax EmptyRecordTypeDeclaration ->
        pp_err Grammar
          "Empty record types must be declared with empty field list `{-}`."
    | BadDeclarationSyntax EmptyExceptionTypeDeclaration ->
        pp_err Grammar
          "Empty exception types must be declared with empty field list `{-}`."
    | BadDeclarationSyntax EmptyCollectionTypeDeclaration ->
        pp_err Grammar
          "Empty collection types must be declared with empty field list `{-}`."
    | BadDeclarationSyntax ElidedParameterWithoutBitvectorType ->
        pp_err Grammar
          "Cannot desugar elided parameter: left-hand side must have a \
           `bits(...)` type annotation."

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
    | BadTupleIndex _ -> "BadTupleIndex"
    | V0Error v0_error -> V0Error.label v0_error
    | UndefinedIdentifier _ -> "UndefinedIdentifier"
    | MismatchedCallType _ -> "MismatchedCallType"
    | BadCallArity _ -> "BadCallArity"
    | BadTupleArity _ -> "BadTupleArity"
    | BadParameterArity _ -> "BadParameterArity"
    | UnsupportedBinop _ -> "UnsupportedBinop"
    | UnsupportedUnop _ -> "UnsupportedUnop"
    | StaticEvaluationFailure _ -> "StaticEvaluationFailure"
    | ImplementationIntegerOverflow _ -> "ImplementationIntegerOverflow"
    | BadParameterType _ -> "BadParameterType"
    | UncheckedExecutionError _ -> UncheckedExecutionError.label
    | ConflictingTypes _ -> "ConflictingTypes"
    | AssertionFailed _ -> "AssertionFailed"
    | CannotParse _ -> "CannotParse"
    | UnknownSymbol _ -> "UnknownSymbol"
    | NoCallCandidate _ -> "NoCallCandidate"
    | BadTypesForBinop _ -> "BadTypesForBinop"
    | ImpureExpression _ -> "ImpureExpression"
    | MismatchedPurity _ -> "MismatchedPurity"
    | MismatchedBitvectorWidths _ -> "MismatchedBitvectorWidths"
    | ExpectedBitvectorType _ -> "ExpectedBitvectorType"
    | CollectionBaseNotVariable _ -> "CollectionBaseNotVariable"
    | NoCommonAncestor _ -> "NoCommonAncestor"
    | AssignToImmutable _ -> "AssignToImmutable"
    | AssignToTupleElement _ -> "AssignToTupleElement"
    | AlreadyDeclaredIdentifier _ -> "AlreadyDeclaredIdentifier"
    | BadReturnStmt _ -> "BadReturnStmt"
    | UncaughtException _ -> "UncaughtException"
    | OverlappingSlices _ -> "OverlappingSlices"
    | BadLDI _ -> "BadLDI"
    | BadRecursiveDecls _ -> "BadRecursiveDecls"
    | InternalInvariantError _ -> InternalInvariantError.label
    | BadATC _ -> "BadATC"
    | DynamicATCFailure _ -> "DynamicATCFailure"
    | ConstrainedIntegerExpected _ -> "ConstrainedIntegerExpected"
    | BadParameterDecl _ -> "BadParameterDecl"
    | BadParameterExpr _ -> "BadParameterExpr"
    | BaseValueEmptyType _ -> "BaseValueEmptyType"
    | ArbitraryEmptyType _ -> "ArbitraryEmptyType"
    | BaseValueNonSymbolic _ -> "BaseValueNonSymbolic"
    | NonReturningFunction _ -> "NonReturningFunction"
    | NoreturnViolation _ -> "NoreturnViolation"
    | UnreachableReached -> "UnreachableReached"
    | LoopLimitReached -> "LoopLimitReached"
    | RecursionLimitReached _ -> "RecursionLimitReached"
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
    | BadBinopPriority _ -> "BadBinopPriority"
    | BadDeclarationSyntax _ -> "BadDeclarationSyntax"

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
