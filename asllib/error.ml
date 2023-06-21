open AST

type error_desc =
  | BadField of string * ty
  | BadFields of string list * ty
  | TypeInferenceNeeded
  | UndefinedIdentifier of identifier
  | MismatchedReturnValue of string
  | BadArity of identifier * int * int
  | UnsupportedBinop of binop * value * value
  | UnsupportedUnop of unop * value
  | UnsupportedExpr of expr
  | MismatchType of value * type_desc list
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
  | UnreconciableTypes of ty * ty
  | AssignToImmutable of string
  | AlreadyDeclaredIdentifier of string
  | BadReturnStmt of ty option
  | UnexpectedSideEffect of string
  | UncaughtException of string

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
          (binop_to_string op) pp_value v1 pp_value v2
    | UnsupportedUnop (op, v) ->
        fprintf f
          "ASL Execution error: Illegal application of operator %s for value@ \
           %a."
          (unop_to_string op) pp_value v
    | UnsupportedExpr e ->
        fprintf f "ASL Error: Unsupported expression %a." pp_expr e
    | MismatchType (v, [ ty ]) ->
        fprintf f
          "ASL Execution error: Mismatch type:@ value %a does not belong to \
           type %a."
          pp_value v pp_type_desc ty
    | MismatchType (v, li) ->
        fprintf f
          "ASL Execution error: Mismatch type:@ value %a@ does not subtype any \
           of those types:@ %a"
          pp_value v
          (pp_comma_list pp_type_desc)
          li
    | BadField (s, ty) ->
        fprintf f "ASL Error: There are no field '%s'@ on type %a." s pp_ty ty
    | BadFields (fields, ty) ->
        fprintf f
          "ASL Error: Fields mismatch for creating a value of type %a@ -- \
           Passed fields are:@ %a"
          pp_ty ty
          (pp_print_list ~pp_sep:pp_print_space pp_print_string)
          fields
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
           and %a@."
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
    | UnreconciableTypes (t1, t2) ->
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
          "ASL Typing error:@ cannot@ return@ something@ from@ a@ procedure@."
    | UnexpectedSideEffect s -> fprintf f "Unexpected side-effect: %s" s
    | UncaughtException s -> fprintf f "Uncaught exception: %s" s
    | BadReturnStmt (Some t) ->
        fprintf f
          "ASL Typing error:@ cannot@ return@ nothing@ from@ a@ function,@ an@ \
           expression@ of@ type@ %a@ is@ expected."
          pp_ty t);
    pp_close_box f ();
    pp_close_box f ()

let error_to_string = Format.asprintf "%a" pp_error
let eprintln = Format.eprintf "@[<2>%a@]@." pp_error
