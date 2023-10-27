(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

open AST
open ASTUtils
open Infix

let _log = false

let list_update i f li =
  let rec aux acc i li =
    match (li, i) with
    | [], _ -> raise (Invalid_argument "list_update")
    | h :: t, 0 -> List.rev_append acc (f h :: t)
    | h :: t, i -> aux (h :: acc) (i - 1) t
  in
  aux [] i li

type native_value =
  | NV_Literal of AST.literal
  | NV_Vector of native_value list
  | NV_Record of native_value ASTUtils.IMap.t

let nv_literal l = NV_Literal l

let rec pp_native_value f =
  let open Format in
  let open PP in
  let pp_comma f () = fprintf f ",@ " in
  function
  | NV_Literal lit -> pp_literal f lit
  | NV_Vector li ->
      fprintf f "@[[%a]@]" (pp_print_list ~pp_sep:pp_comma pp_native_value) li
  | NV_Record map -> IMap.pp_print pp_native_value f map

let native_value_to_string = Format.asprintf "%a" pp_native_value

let mismatch_type v types =
  Error.fatal_unknown_pos (Error.MismatchType (native_value_to_string v, types))

module NativeBackend = struct
  type 'a m = 'a
  type value = native_value
  type primitive = value m list -> value list m
  type ast = primitive AST.t
  type scope = AST.identifier * int

  module ScopedIdentifiers = struct
    type t = identifier * scope

    let compare = compare
  end

  module SIMap = Map.Make (ScopedIdentifiers)

  let is_undetermined _ = false
  let v_of_int i = L_Int (Z.of_int i) |> nv_literal
  let v_of_literal = nv_literal
  let debug_value = native_value_to_string

  let v_to_int = function
    | NV_Literal (L_Int i) -> Some (Z.to_int i)
    | _ -> None

  let bind (vm : 'a m) (f : 'a -> 'b m) : 'b m = f vm
  let prod_par (r1 : 'a m) (r2 : 'b m) : ('a * 'b) m = (r1, r2)
  let return v = v

  let v_unknown_of_type ty =
    Types.base_value dummy_annotated StaticEnv.empty ty
    |> StaticInterpreter.static_eval StaticEnv.empty
    |> nv_literal

  let warnT msg v =
    (* Should not be called... *)
    Printf.eprintf "Warning: message %s found its way, something is wrong\n" msg;
    return v

  let bind_data = bind
  let bind_seq = bind
  let bind_ctrl = bind
  let appl_data m f = bind_data m (fun v -> return (f v))

  let choice (c : value m) (m_true : 'b m) (m_false : 'b m) : 'b m =
    let open AST in
    bind c (function
      | NV_Literal (L_Bool true) -> m_true
      | NV_Literal (L_Bool false) -> m_false
      | v -> mismatch_type v [ T_Bool ])

  let delay m k = k m m

  let binop op v1 v2 =
    match (v1, v2) with
    | NV_Literal v1, NV_Literal v2 ->
        StaticInterpreter.binop_values dummy_annotated op v1 v2 |> nv_literal
    | NV_Literal _, v | v, _ ->
        mismatch_type v [ T_Bool; T_Int None; T_Real; default_t_bits ]

  let ternary = function
    | NV_Literal (L_Bool true) -> fun m_true _m_false -> m_true ()
    | NV_Literal (L_Bool false) -> fun _m_true m_false -> m_false ()
    | v -> mismatch_type v [ T_Bool ]

  let unop op v =
    match v with
    | NV_Literal v ->
        StaticInterpreter.unop_values dummy_annotated op v |> nv_literal
    | _ -> mismatch_type v [ T_Bool; T_Int None; T_Real; default_t_bits ]

  let on_write_identifier x scope value =
    if _log then
      Format.eprintf "Writing %a to %s in %a.@." pp_native_value value x
        PP.pp_scope scope

  let on_read_identifier x scope value =
    if _log then
      Format.eprintf "Reading %a from %s in %a.@." pp_native_value value x
        PP.pp_scope scope

  let v_tuple li = return (NV_Vector li)
  let v_record li = return (NV_Record (IMap.of_list li))
  let v_exception li = v_record li
  let non_tuple_exception v = mismatch_type v [ T_Tuple [] ]

  let doesnt_have_fields_exception v =
    mismatch_type v [ T_Record []; T_Exception [] ]

  let get_index i vec =
    match vec with
    | NV_Vector li -> List.nth li i |> return
    | v -> non_tuple_exception v

  let set_index i v vec =
    match vec with
    | NV_Vector li -> list_update i (Fun.const v) li |> v_tuple
    | v -> non_tuple_exception v

  let get_field name record =
    match record with
    | NV_Record map -> IMap.find name map
    | v -> doesnt_have_fields_exception v

  let set_field name v record =
    match record with
    | NV_Record li -> NV_Record (IMap.add name v li)
    | v -> doesnt_have_fields_exception v

  let create_vector = v_tuple
  let create_record = v_record
  let create_exception = v_exception

  let as_bitvector = function
    | NV_Literal (L_BitVector bits) -> bits
    | NV_Literal (L_Int i) -> Bitvector.of_z (Z.numbits i) i
    | v -> mismatch_type v [ default_t_bits ]

  let as_int = function
    | NV_Literal (L_Int i) -> Z.to_int i
    | v -> mismatch_type v [ T_Int None ]

  let bitvector_to_value bv = L_BitVector bv |> nv_literal |> return
  let int_max x y = if x >= y then x else y

  let read_from_bitvector positions bv =
    let positions = slices_to_positions as_int positions in
    let max_pos = List.fold_left int_max 1 positions in
    let bv =
      match bv with
      | NV_Literal (L_BitVector bv) -> bv
      | NV_Literal (L_Int i) -> Bitvector.of_z (max_pos + 1) i
      | _ -> mismatch_type bv [ default_t_bits ]
    in
    let res = Bitvector.extract_slice bv positions in
    bitvector_to_value res

  let write_to_bitvector positions bits bv =
    let bv = as_bitvector bv
    and bits = as_bitvector bits
    and positions = slices_to_positions as_int positions in
    Bitvector.write_slice bv bits positions |> bitvector_to_value

  let concat_bitvectors bvs =
    let bvs = List.map as_bitvector bvs in
    Bitvector.concat bvs |> bitvector_to_value

  let bitvector_length bv =
    let bv = as_bitvector bv in
    Bitvector.length bv |> v_of_int
end

module NativePrimitives = struct
  open NativeBackend

  let return_one v = return [ return v ]

  let uint = function
    | [ NV_Literal (L_BitVector bv) ] ->
        L_Int (Bitvector.to_z_unsigned bv) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ T_Int None ]
    | li -> Error.fatal_unknown_pos @@ Error.BadArity ("UInt", 1, List.length li)

  let sint = function
    | [ NV_Literal (L_BitVector bv) ] ->
        L_Int (Bitvector.to_z_signed bv) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ default_t_bits ]
    | li -> Error.fatal_unknown_pos @@ Error.BadArity ("SInt", 1, List.length li)

  let print =
    let print_one = function
      | NV_Literal (L_String s) -> print_string s
      | v -> mismatch_type v [ T_String ]
    in
    fun li ->
      List.iter print_one li;
      Printf.printf "\n%!";
      return []

  let dec_str = function
    | [ NV_Literal (L_Int i) ] ->
        L_String (Z.to_string i) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ T_Int None ]
    | li ->
        Error.fatal_unknown_pos @@ Error.BadArity ("DecStr", 1, List.length li)

  let hex_str = function
    | [ NV_Literal (L_Int i) ] ->
        L_String (Printf.sprintf "%a" Z.sprint i) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ T_Int None ]
    | li ->
        Error.fatal_unknown_pos @@ Error.BadArity ("DecStr", 1, List.length li)

  let ascii_range = Constraint_Range (!$0, !$127)
  let ascii_integer = T_Int (Some [ ascii_range ])

  let ascii_str =
    let open! Z in
    function
    | [ NV_Literal (L_Int i) ] when geq zero i && leq ~$127 i ->
        L_String (char_of_int (Z.to_int i) |> String.make 1)
        |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ ascii_integer ]
    | li ->
        Error.fatal_unknown_pos @@ Error.BadArity ("DecStr", 1, List.length li)

  let log2 = function
    | [ NV_Literal (L_Int i) ] when Z.gt i Z.zero ->
        [ L_Int (Z.log2 i |> Z.of_int) |> nv_literal ]
    | [ v ] -> mismatch_type v [ T_Int None ]
    | li -> Error.fatal_unknown_pos @@ Error.BadArity ("Log2", 1, List.length li)

  let int_to_real = function
    | [ NV_Literal (L_Int i) ] ->
        L_Real (Q.of_bigint i) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ T_Int None ]
    | li -> Error.fatal_unknown_pos @@ Error.BadArity ("Real", 1, List.length li)

  let truncate q = Q.to_bigint q

  let floor q =
    if Q.sign q = -1 then
      if Q.den q = Z.one then Q.num q else truncate q |> Z.pred
    else truncate q

  let ceiling q =
    if Q.sign q = 1 then
      if Q.den q = Z.one then Q.num q else truncate q |> Z.succ
    else truncate q

  let wrap_real_to_int name f = function
    | [ NV_Literal (L_Real q) ] -> L_Int (f q) |> nv_literal |> return_one
    | [ v ] -> mismatch_type v [ T_Real ]
    | li -> Error.fatal_unknown_pos @@ Error.BadArity (name, 1, List.length li)

  let round_down = wrap_real_to_int "RoundDown" floor
  let round_up = wrap_real_to_int "RoundUp" ceiling
  let round_towards_zero = wrap_real_to_int "RoundTowardsZero" truncate

  let primitives =
    let with_pos = add_dummy_pos in
    let t_bits e = T_Bits (BitWidth_SingleExpr e, []) |> with_pos in
    let e_var x = E_Var x |> with_pos in
    let d_func_string i =
      D_Func
        {
          name = "print";
          parameters = [];
          args = List.init i (fun j -> ("s" ^ string_of_int j, string));
          body = SB_Primitive print;
          return_type = None;
          subprogram_type = ST_Procedure;
        }
    in
    let here = ASTUtils.add_pos_from_pos_of in
    [
      D_Func
        {
          name = "UInt";
          parameters = [ ("N", Some integer) ];
          args = [ ("x", t_bits (e_var "N")) ];
          body = SB_Primitive uint;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "SInt";
          parameters = [ ("N", Some integer) ];
          args = [ ("x", t_bits (e_var "N")) ];
          body = SB_Primitive sint;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "DecStr";
          parameters = [];
          args = [ ("x", integer) ];
          body = SB_Primitive dec_str;
          return_type = Some string;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "HexStr";
          parameters = [];
          args = [ ("x", integer) ];
          body = SB_Primitive hex_str;
          return_type = Some string;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "AsciiStr";
          parameters = [];
          args = [ ("x", integer) ];
          body = SB_Primitive ascii_str;
          return_type = Some string;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "Log2";
          parameters = [];
          args = [ ("x", integer) ];
          body = SB_Primitive log2;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "Real";
          parameters = [];
          args = [ ("x", integer) ];
          body = SB_Primitive int_to_real;
          return_type = Some real;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "RoundDown";
          parameters = [];
          args = [ ("x", real) ];
          body = SB_Primitive round_down;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "RoundUp";
          parameters = [];
          args = [ ("x", real) ];
          body = SB_Primitive round_up;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      D_Func
        {
          name = "RoundTowardsZero";
          parameters = [];
          args = [ ("x", real) ];
          body = SB_Primitive round_towards_zero;
          return_type = Some integer;
          subprogram_type = ST_Function;
        }
      |> __POS_OF__ |> here;
      d_func_string 0 |> __POS_OF__ |> here;
      d_func_string 1 |> __POS_OF__ |> here;
      d_func_string 2 |> __POS_OF__ |> here;
      d_func_string 3 |> __POS_OF__ |> here;
      d_func_string 4 |> __POS_OF__ |> here;
    ]
end

module NativeInterpreter (C : Interpreter.Config) =
  Interpreter.Make (NativeBackend) (C)

let exit_value = function
  | NV_Literal (L_Int i) -> i |> Z.to_int
  | v -> mismatch_type v [ T_Int None ]

let instrumentation_buffer = function
  | Some true ->
      (module Instrumentation.SemanticsSingleSetBuffer
      : Instrumentation.SEMBUFFER)
  | Some false | None ->
      (module Instrumentation.SemanticsNoBuffer : Instrumentation.SEMBUFFER)

let interprete strictness ?instrumentation ?static_env ast =
  let module B = (val instrumentation_buffer instrumentation) in
  let module C : Interpreter.Config = struct
    let type_checking_strictness = strictness
    let unroll = 0

    module Instr = Instrumentation.SemMake (B)
  end in
  let module I = NativeInterpreter (C) in
  B.reset ();
  let res =
    match static_env with
    | Some static_env -> I.run_typed ast static_env
    | None ->
        let ast = List.rev_append NativePrimitives.primitives ast in
        I.run ast
  in
  (exit_value res, B.get ())
