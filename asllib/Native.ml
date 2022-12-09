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

  let vint_of_int i = i

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
      | AST.VBool true -> m_true
      | AST.VBool false -> m_false
      | _ -> fail (TypeError "Boolean expected."))

  let fatal msg = fail (InterpreterError msg)

  let binop op v1 v2 =
    let vint v = return (VInt v) in
    let vbool v = return (VBool v) in
    let vreal r = return (VReal r) in
    match (op, v1, v2) with
    (* int -> int -> int *)
    | PLUS, VInt v1, VInt v2 -> vint (v1 + v2)
    | MUL, VInt v1, VInt v2 -> vint (v1 * v2)
    | MINUS, VInt v1, VInt v2 -> vint (v1 - v2)
    | DIV, VInt v1, VInt v2 -> vint (v1 / v2)
    (* int -> int -> bool*)
    | EQ_OP, VInt v1, VInt v2 -> vbool (v1 == v2)
    | NEQ, VInt v1, VInt v2 -> vbool (v1 <> v2)
    | LEQ, VInt v1, VInt v2 -> vbool (v1 <= v2)
    | LT, VInt v1, VInt v2 -> vbool (v1 < v2)
    | GEQ, VInt v1, VInt v2 -> vbool (v1 >= v2)
    | GT, VInt v1, VInt v2 -> vbool (v1 > v2)
    (* bool -> bool -> bool *)
    | BAND, VBool b1, VBool b2 -> vbool (b1 && b2)
    | BOR, VBool b1, VBool b2 -> vbool (b1 || b2)
    | BEQ, VBool b1, VBool b2 -> vbool (b1 == b2)
    | IMPL, VBool b1, VBool b2 -> vbool ((not b1) || b2)
    | EQ_OP, VBool b1, VBool b2 -> vbool (b1 == b2)
    | NEQ, VBool b1, VBool b2 -> vbool (b1 <> b2)
    (* real -> real -> real *)
    | PLUS, VReal v1, VReal v2 -> vreal (v1 +. v2)
    | MUL, VReal v1, VReal v2 -> vreal (v1 *. v2)
    | MINUS, VReal v1, VReal v2 -> vreal (v1 -. v2)
    | DIV, VReal v1, VReal v2 -> vreal (v1 /. v2)
    (* real -> real -> bool *)
    | EQ_OP, VReal v1, VReal v2 -> vbool (v1 == v2)
    | NEQ, VReal v1, VReal v2 -> vbool (v1 <> v2)
    | LEQ, VReal v1, VReal v2 -> vbool (v1 <= v2)
    | LT, VReal v1, VReal v2 -> vbool (v1 < v2)
    | GEQ, VReal v1, VReal v2 -> vbool (v1 >= v2)
    | GT, VReal v1, VReal v2 -> vbool (v1 > v2)
    | _ ->
        fail
          (InterpreterError "Operation not yet implemented for native backend.")

  let unop op v =
    match (op, v) with
    | NEG, VInt i -> return (VInt ~-i)
    | NEG, VReal r -> return (VReal ~-.r)
    | BNOT, VBool b -> return (VBool (not b))
    | _ -> assert false

  let on_write_identifier _x _scope _value = return ()
  let on_read_identifier _x _scope _value = return ()
end

module NativeInterpreter = Interpreter.Make (NativeBackend)

let of_parsed_ast =
  AST.tr_values value_of_vint value_of_vbool value_of_vreal (fun s ->
      VBitVector (int_of_string s))
