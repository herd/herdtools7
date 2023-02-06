open AST

type error =
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

exception ASLException of error annotated

let fatal e = raise (ASLException e)
let fatal_from pos e = fatal (ASTUtils.add_pos_from pos e)

let fatal_here pos_start pos_end e =
  fatal (ASTUtils.annotated e pos_start pos_end)

let fatal_unknown_pos e = fatal (ASTUtils.add_dummy_pos e)
let intercept f () = try Ok (f ()) with ASLException e -> Error e

let pp_error =
  let open Format in
  let open PP in
  let pp_comma_list = pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") in
  let pp_pos f { pos_start; pos_end; _ } =
    let open Lexing in
    let pp_char_num f { pos_cnum; pos_bol; _ } =
      pp_print_int f (pos_cnum - pos_bol)
    in
    pp_open_hovbox f 2;
    fprintf f "File %s,@ " pos_start.pos_fname;
    if String.equal pos_start.pos_fname pos_end.pos_fname then
      if pos_start.pos_lnum = pos_end.pos_lnum then
        if pos_start.pos_cnum = pos_end.pos_cnum then
          fprintf f "line %d,@ character %a:" pos_start.pos_lnum pp_char_num
            pos_start
        else
          fprintf f "line %d,@ characters@ %a@ to@ %a:" pos_start.pos_lnum
            pp_char_num pos_start pp_char_num pos_end
      else
        fprintf f "line %d,@ character %a@ to@ line@ %2d,@ character %a:"
          pos_start.pos_lnum pp_char_num pos_start pos_end.pos_lnum pp_char_num
          pos_end
    else
      fprintf f "line %d,@ character %a:" pos_start.pos_lnum pp_char_num
        pos_start;
    close_box ()
  in
  let pp_type_desc f ty = pp_ty f (ASTUtils.add_dummy_pos ty) in
  fun f e ->
    fprintf f "@[<v 0>%a@ @[<2>Error:@ " pp_pos e;
    (match e.desc with
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
        fprintf f "Cannot get field '%s'@ on type %a." s pp_ty ty
    | BadFields (fields, ty) ->
        fprintf f
          "Fields mismatch for creating a value of type %a@ -- Passed fields \
           are:@ %a"
          pp_ty ty
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
          pp_type_desc expected pp_ty provided
    | ConflictingTypes (expected, provided) ->
        fprintf f "Type error:@ %a does@ not@ subtype@ any@ of:@ %a." pp_ty
          provided
          (pp_comma_list pp_type_desc)
          expected
    | AssertionFailed e -> fprintf f "Assertion failed:@ %a" pp_expr e
    | CannotParse -> pp_print_string f "Cannot parse."
    | UnknownSymbol -> pp_print_string f "Unknown symbol.");
    pp_close_box f ();
    pp_close_box f ()

let error_to_string = Format.asprintf "%a" pp_error
