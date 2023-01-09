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

type err =
  | UnknownIdentifier of string
  | TypeError of string
  | InterpreterError of string
  | NonIndexableValue of value
  | IndexOutOfBounds of (int * value)

let pp_err chan = function
  | UnknownIdentifier x -> Printf.fprintf chan "Unknown identifier %s" x
  | TypeError msg -> Printf.fprintf chan "Type error: %s" msg
  | InterpreterError msg -> Printf.fprintf chan "Interpreter error: %s" msg
  | NonIndexableValue value ->
      Printf.fprintf chan "Non indexable value: %s" (PP.value_to_string value)
  | IndexOutOfBounds (i, v) ->
      Printf.fprintf chan "Index %d out of bounds for value %s" i
        (PP.value_to_string v)

let list_update i f li =
  let rec aux acc i li =
    match (li, i) with
    | [], _ -> raise (Invalid_argument "list_update")
    | h :: t, 0 -> List.rev_append acc (f h :: t)
    | h :: t, i -> aux (h :: acc) (i - 1) t
  in
  aux [] i li

exception NativeInterpreterExn of err

module NativeBackend = struct
  type 'a m = unit -> 'a
  type value = AST.value
  type scope = AST.identifier * int

  module ScopedIdentifiers = struct
    type t = identifier * scope

    let compare = compare
  end

  module SIMap = Map.Make (ScopedIdentifiers)

  let v_of_int i = V_Int i
  let v_of_parsed_v = Fun.id

  let bind (vm : 'a m) (f : 'a -> 'b m) : 'b m =
   fun () ->
    let v = vm () in
    f v ()

  let prod (r1 : 'a m) (r2 : 'b m) : ('a * 'b) m = fun () -> (r1 (), r2 ())
  let return v () = v
  let fail err = raise (NativeInterpreterExn err)
  let bind_data = bind
  let bind_seq = bind

  let choice (c : value m) (m_true : 'b m) (m_false : 'b m) : 'b m =
    bind c (function
      | AST.V_Bool true -> m_true
      | AST.V_Bool false -> m_false
      | _ -> fail (TypeError "Boolean expected."))

  let fatal msg = fail (InterpreterError msg)
  let binop op v1 v2 () = StaticInterpreter.binop op v1 v2
  let unop op v () = StaticInterpreter.unop op v
  let on_write_identifier _x _scope _value = return ()
  let on_read_identifier _x _scope _value = return ()
  let v_tuple li = return (V_Tuple li)
  let v_record li = return (V_Record li)
  let v_exception li = return (V_Exception li)

  let get_i i vec =
    try
      match vec with
      | V_Tuple li -> List.nth li i |> return
      | V_Record li -> List.nth li i |> snd |> return
      | V_Exception li -> List.nth li i |> snd |> return
      | v -> fail (NonIndexableValue v)
    with Invalid_argument _ -> fail (IndexOutOfBounds (i, vec))

  let set_i i v vec =
    let field_update i v li =
      let update_field v (name, _v) = (name, v) in
      list_update i (update_field v) li
    in
    try
      match vec with
      | V_Tuple li -> list_update i (Fun.const v) li |> v_tuple
      | V_Record li -> field_update i v li |> v_record
      | V_Exception li -> field_update i v li |> v_exception
      | _ -> fail (NonIndexableValue vec)
    with Invalid_argument _ -> fail (IndexOutOfBounds (i, vec))

  let create_vector ty li =
    let assoc_name_val (name, _ty) v = (name, v) in
    let assoc_names_values = List.map2 assoc_name_val in
    match ty with
    | T_Tuple _ -> v_tuple li
    | T_Record field_types -> assoc_names_values field_types li |> v_record
    | T_Exception field_types ->
        assoc_names_values field_types li |> v_exception
    | ty ->
        fail
          (InterpreterError
             ("Cannot create a vector of type " ^ PP.type_desc_to_string ty))

  let as_bitvector_string = function
    | V_BitVector bits -> bits
    | _ -> fail (TypeError "Unsupported operation on bitvectors: slicing")

  let bitvector_of_string s = return (V_BitVector s)

  let read_from_bitvector positions bv =
    List.to_seq positions
    |> Seq.map (String.get (as_bitvector_string bv))
    |> String.of_seq |> bitvector_of_string

  let write_to_bitvector positions bits bv =
    let result = Bytes.of_string (as_bitvector_string bv) in
    let bits = bits |> as_bitvector_string |> String.to_seq |> List.of_seq in
    let () = List.iter2 (Bytes.set result) positions bits in
    bitvector_of_string (Bytes.to_string result)
end

module NativeInterpreter = Interpreter.Make (NativeBackend)
