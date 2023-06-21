(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

open AST

let _log = false

let list_update i f li =
  let rec aux acc i li =
    match (li, i) with
    | [], _ -> raise (Invalid_argument "list_update")
    | h :: t, 0 -> List.rev_append acc (f h :: t)
    | h :: t, i -> aux (h :: acc) (i - 1) t
  in
  aux [] i li

let fatal_from = Error.fatal_from

let mismatch_type v types =
  Error.fatal_unknown_pos (Error.MismatchType (v, types))

module NativeBackend = struct
  type 'a m = 'a
  type value = AST.value
  type scope = AST.identifier * int

  module ScopedIdentifiers = struct
    type t = identifier * scope

    let compare = compare
  end

  module SIMap = Map.Make (ScopedIdentifiers)

  let is_undetermined _ = false
  let v_of_int i = V_Int i
  let v_of_parsed_v = Fun.id
  let v_to_int = function V_Int i -> Some i | _ -> None
  let debug_value = PP.value_to_string
  let bind (vm : 'a m) (f : 'a -> 'b m) : 'b m = f vm
  let prod (r1 : 'a m) (r2 : 'b m) : ('a * 'b) m = (r1, r2)
  let return v = v

  let warnT msg v =
    (* Should not be called... *)
    Printf.eprintf "Warning: message %s found its way, something is wrong\n" msg;
    return v

  let bind_data = bind
  let bind_seq = bind
  let bind_ctrl = bind

  let choice (c : value m) (m_true : 'b m) (m_false : 'b m) : 'b m =
    let open AST in
    bind c (function
      | V_Bool true -> m_true
      | V_Bool false -> m_false
      | v -> mismatch_type v [ T_Bool ])

  let delay m k = k m m

  let binop op v1 v2 =
    StaticInterpreter.binop_values ASTUtils.dummy_annotated op v1 v2

  let ternary = function
    | V_Bool true -> fun m_true _m_false -> m_true ()
    | V_Bool false -> fun _m_true m_false -> m_false ()
    | v -> mismatch_type v [ T_Bool ]

  let unop op v = StaticInterpreter.unop_values ASTUtils.dummy_annotated op v

  let on_write_identifier x scope value =
    if _log then
      Format.eprintf "Writing %a to %s in %a.@." PP.pp_value value x PP.pp_scope
        scope

  let on_read_identifier x scope value =
    if _log then
      Format.eprintf "Reading %a from %s in %a.@." PP.pp_value value x
        PP.pp_scope scope

  let v_tuple li = return (V_Tuple li)
  let v_record li = return (V_Record li)
  let v_exception li = return (V_Exception li)
  let indexables = [ T_Tuple []; T_Record []; T_Exception [] ]
  let non_indexable_error v = mismatch_type v indexables

  let get_i i vec =
    match vec with
    | V_Tuple li -> List.nth li i |> return
    | V_Record li -> List.nth li i |> snd |> return
    | V_Exception li -> List.nth li i |> snd |> return
    | v -> non_indexable_error v

  let set_i i v vec =
    let field_update i v li =
      let update_field v (name, _v) = (name, v) in
      list_update i (update_field v) li
    in
    match vec with
    | V_Tuple li -> list_update i (Fun.const v) li |> v_tuple
    | V_Record li -> field_update i v li |> v_record
    | V_Exception li -> field_update i v li |> v_exception
    | v -> non_indexable_error v

  let create_vector ty li =
    let assoc_name_val (name, _ty) v = (name, v) in
    let assoc_names_values = List.map2 assoc_name_val in
    match ty.desc with
    | T_Tuple _ -> v_tuple li
    | T_Record field_types -> assoc_names_values field_types li |> v_record
    | T_Exception field_types ->
        assoc_names_values field_types li |> v_exception
    | _ -> fatal_from ty @@ Error.ConflictingTypes (indexables, ty)

  let as_bitvector = function
    | V_BitVector bits -> bits
    | V_Int i -> Bitvector.of_int i
    | v -> mismatch_type v [ ASTUtils.default_t_bits ]

  let as_int = function V_Int i -> i | v -> mismatch_type v [ T_Int None ]
  let bitvector_to_value bv = return (V_BitVector bv)

  let read_from_bitvector positions bv =
    let bv =
      match bv with
      | V_BitVector bv -> bv
      | V_Int i -> Bitvector.of_int i
      | _ -> mismatch_type bv [ ASTUtils.default_t_bits ]
    and positions = ASTUtils.slices_to_positions as_int positions in
    let res = Bitvector.extract_slice bv positions in
    bitvector_to_value res

  let write_to_bitvector positions bits bv =
    let bv = as_bitvector bv
    and bits = as_bitvector bits
    and positions = ASTUtils.slices_to_positions as_int positions in
    Bitvector.write_slice bv bits positions |> bitvector_to_value

  let concat_bitvectors bvs =
    let bvs = List.map as_bitvector bvs in
    Bitvector.concat bvs |> bitvector_to_value
end

module NativeStdlib = struct
  open NativeBackend

  let return_one v = return [ return v ]

  let uint = function
    | [ V_BitVector bv ] -> V_Int (Bitvector.to_int bv) |> return_one
    | [ v ] ->
        Error.fatal_unknown_pos
        @@ Error.MismatchType (v, [ ASTUtils.default_t_bits ])
    | li ->
        Error.fatal_unknown_pos
        @@ Error.BadArity ("Replicate", 1, List.length li)

  let sint = function
    | [ V_BitVector bv ] -> V_Int (Bitvector.to_int_signed bv) |> return_one
    | [ v ] ->
        Error.fatal_unknown_pos
        @@ Error.MismatchType (v, [ ASTUtils.default_t_bits ])
    | li ->
        Error.fatal_unknown_pos
        @@ Error.BadArity ("Replicate", 1, List.length li)

  let primitives =
    let with_pos = ASTUtils.add_dummy_pos in
    let t_bits e = T_Bits (BitWidth_Determined e, []) |> with_pos in
    let e_var x = E_Var x |> with_pos in
    let integer = T_Int None |> with_pos in
    [
      {
        name = "UInt";
        parameters = [ ("N", Some integer) ];
        args = [ ("x", t_bits (e_var "N")) ];
        body = uint;
        return_type = Some integer;
      };
      {
        name = "SInt";
        parameters = [ ("N", Some integer) ];
        args = [ ("x", t_bits (e_var "N")) ];
        body = sint;
        return_type = Some integer;
      };
    ]

  let stdlib = []
end

module NativeInterpreter (C : Interpreter.Config) =
  Interpreter.Make (NativeBackend) (C)

let run (module C : Interpreter.Config) ast =
  let module I = NativeInterpreter (C) in
  I.run (List.rev_append NativeStdlib.stdlib ast) NativeStdlib.primitives

let interprete strictness ast =
  let module C : Interpreter.Config = struct
    let type_checking_strictness = strictness
    let unroll = 0 (* Does not matter, as all computations are performed *)

    module Instr = Instrumentation.NoInstr
  end in
  run (module C) ast

let interprete_with_instrumentation strictness ast =
  let module B = Instrumentation.SingleSetBuffer in
  B.reset ();
  let module C = struct
    let type_checking_strictness = strictness
    let unroll = 0

    module Instr = Instrumentation.Make (B)
  end in
  run (module C) ast;
  B.get ()
