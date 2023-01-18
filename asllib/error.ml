open AST

type error =
  | BadField of string * type_desc
  | BadFields of string list * type_desc
  | TypeInferenceNeeded
  | UndefinedIdentifier of identifier
  | MismatchedReturnValue of string
  | BadArity of identifier * int * int
  | UnsupportedBinop of binop * value * value
  | UnsupportedUnop of unop * value
  | UnsupportedExpr of expr
  | MismatchType of value * type_desc list
  | NotYetImplemented of string
  | ConflictingTypes of type_desc list * type_desc
  | AssertionFailed of expr

exception ASLException of error

let fatal e = raise (ASLException e)
let intercept f () = try Ok (f ()) with ASLException e -> Error e

let pp_error =
  let open Format in
  let open PP in
  let pp_comma_list = pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") in
  fun f e ->
    pp_open_box f 2;
    (match e with
    | UnsupportedBinop (op, v1, v2) ->
        fprintf f "Unsupported binop %s for values@ %a@ and %a."
          (binop_to_string op) pp_value v1 pp_value v2
    | UnsupportedUnop (op, v) ->
        fprintf f "Unsupported unop %s for value@ %a." (unop_to_string op)
          pp_value v
    | UnsupportedExpr e -> fprintf f "Unsupported expression %a." pp_expr e
    | MismatchType (v, [ ty ]) ->
        fprintf f "Mismatch type:@ value %a does not belong to type %a."
          pp_value v pp_type_desc ty
    | MismatchType (v, li) ->
        fprintf f
          "Mismatch type:@ value %a@ does not subtype any of those types:@ %a"
          pp_value v
          (pp_comma_list pp_type_desc)
          li
    | BadField (s, ty) ->
        fprintf f "Cannot get field '%s'@ on type %a." s pp_type_desc ty
    | BadFields (fields, ty) ->
        fprintf f
          "Fields mismatch for creating a value of type %a@ -- Passed fields \
           are:@ %a"
          pp_type_desc ty
          (pp_print_list ~pp_sep:pp_print_space pp_print_string)
          fields
    | TypeInferenceNeeded ->
        pp_print_text f "Interpreter blocked. Type inference needed."
    | UndefinedIdentifier s -> fprintf f "Undefined identifier:@ '%s'" s
    | MismatchedReturnValue s ->
        fprintf f "Mismatched use of return value from call to '%s'" s
    | BadArity (name, expected, provided) ->
        fprintf f
          "Arity error while calling '%s':@ %d arguments expected and %d \
           provided"
          name expected provided
    | NotYetImplemented s -> pp_print_text f @@ "Not yet implemented: " ^ s
    | ConflictingTypes ([ expected ], provided) ->
        fprintf f "Type error:@ a subtype of@ %a@ was expected,@ provided %a."
          pp_type_desc expected pp_type_desc provided
    | ConflictingTypes (expected, provided) ->
        fprintf f "Type error:@ %a does@ not@ subtype@ any@ of:@ %a."
          pp_type_desc provided
          (pp_comma_list pp_type_desc)
          expected
    | AssertionFailed e -> fprintf f "Assertion failed:@ %a" pp_expr e);
    pp_close_box f ()

let error_to_string = Format.asprintf "%a" pp_error
