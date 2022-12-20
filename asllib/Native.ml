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

type err =
  | UnknownIdentifier of string
  | TypeError of string
  | InterpreterError of string

let pp_err chan = function
  | UnknownIdentifier x -> Printf.fprintf chan "Unknown identifier %s" x
  | TypeError msg -> Printf.fprintf chan "Type error: %s" msg
  | InterpreterError msg -> Printf.fprintf chan "Interpreter error: %s" msg

open AST

module NativeBackend = struct
  type vint = int
  type vbool = bool
  type vreal = float
  type vbitvector = int
  type 'a m = unit -> ('a, err) result
  type loc = string
  type value = (vint, vbool, vreal, vbitvector) AST.value
  type scope = AST.identifier * int

  module ScopedIdentifiers = struct
    type t = identifier * scope

    let compare = compare
  end

  module SIMap = Map.Make (ScopedIdentifiers)

  let v_of_int i = AST.V_Int i

  let bind (vm : 'a m) (f : 'a -> 'b m) : 'b m =
   fun () -> Result.bind (vm ()) (fun v -> f v ())

  let prod (r1 : 'a m) (r2 : 'b m) : ('a * 'b) m =
   fun () ->
    match (r1 (), r2 ()) with
    | Error e, _ | _, Error e -> Error e
    | Ok v1, Ok v2 -> Ok (v1, v2)

  let return : 'a -> 'a m = fun v () -> Result.ok v
  let fail : err -> 'a m = fun e () -> Result.error e
  let bind_data = bind
  let bind_seq = bind

  let choice (c : value m) (m_true : 'b m) (m_false : 'b m) : 'b m =
    bind c (function
      | AST.V_Bool true -> m_true
      | AST.V_Bool false -> m_false
      | _ -> fail (TypeError "Boolean expected."))

  let fatal msg = fail (InterpreterError msg)

  let binop op v1 v2 =
    let vint v = return (V_Int v) in
    let vbool v = return (V_Bool v) in
    let vreal r = return (V_Real r) in
    match (op, v1, v2) with
    (* int -> int -> int *)
    | PLUS, V_Int v1, V_Int v2 -> vint (v1 + v2)
    | MUL, V_Int v1, V_Int v2 -> vint (v1 * v2)
    | MINUS, V_Int v1, V_Int v2 -> vint (v1 - v2)
    | DIV, V_Int v1, V_Int v2 -> vint (v1 / v2)
    (* int -> int -> bool*)
    | EQ_OP, V_Int v1, V_Int v2 -> vbool (v1 == v2)
    | NEQ, V_Int v1, V_Int v2 -> vbool (v1 <> v2)
    | LEQ, V_Int v1, V_Int v2 -> vbool (v1 <= v2)
    | LT, V_Int v1, V_Int v2 -> vbool (v1 < v2)
    | GEQ, V_Int v1, V_Int v2 -> vbool (v1 >= v2)
    | GT, V_Int v1, V_Int v2 -> vbool (v1 > v2)
    (* bool -> bool -> bool *)
    | BAND, V_Bool b1, V_Bool b2 -> vbool (b1 && b2)
    | BOR, V_Bool b1, V_Bool b2 -> vbool (b1 || b2)
    | BEQ, V_Bool b1, V_Bool b2 -> vbool (b1 == b2)
    | IMPL, V_Bool b1, V_Bool b2 -> vbool ((not b1) || b2)
    | EQ_OP, V_Bool b1, V_Bool b2 -> vbool (b1 == b2)
    | NEQ, V_Bool b1, V_Bool b2 -> vbool (b1 <> b2)
    (* real -> real -> real *)
    | PLUS, V_Real v1, V_Real v2 -> vreal (v1 +. v2)
    | MUL, V_Real v1, V_Real v2 -> vreal (v1 *. v2)
    | MINUS, V_Real v1, V_Real v2 -> vreal (v1 -. v2)
    | DIV, V_Real v1, V_Real v2 -> vreal (v1 /. v2)
    (* real -> real -> bool *)
    | EQ_OP, V_Real v1, V_Real v2 -> vbool (v1 == v2)
    | NEQ, V_Real v1, V_Real v2 -> vbool (v1 <> v2)
    | LEQ, V_Real v1, V_Real v2 -> vbool (v1 <= v2)
    | LT, V_Real v1, V_Real v2 -> vbool (v1 < v2)
    | GEQ, V_Real v1, V_Real v2 -> vbool (v1 >= v2)
    | GT, V_Real v1, V_Real v2 -> vbool (v1 > v2)
    | _ ->
        fail
          (InterpreterError "Operation not yet implemented for native backend.")

  let unop op v =
    match (op, v) with
    | NEG, V_Int i -> return (V_Int ~-i)
    | NEG, V_Real r -> return (V_Real ~-.r)
    | BNOT, V_Bool b -> return (V_Bool (not b))
    | _ -> assert false

  let on_write_identifier _x _scope _value = return ()
  let on_read_identifier _x _scope _value = return ()
end

module NativeInterpreter = Interpreter.Make (NativeBackend)

let of_parsed_ast =
  ASTUtils.tr_values
    (fun i -> V_Int i)
    (fun b -> V_Bool b)
    (fun r -> V_Real r)
    (fun s -> V_BitVector (int_of_string s))
