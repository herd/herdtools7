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

type value = (int, bool, float, int) AST.value

type err =
  | UnknownIdentifier of string
  | TypeError of string
  | InterpreterError of string
  | NonIndexableValue of value
  | IndexOutOfBounds of (int * value)

val pp_err : out_channel -> err -> unit

module NativeBackend :
  Backend.S with type value = value and type 'a m = unit -> ('a, err) result

module NativeInterpreter : Interpreter.S with module B = NativeBackend

val of_parsed_ast : AST.parsed_t -> NativeBackend.value AST.t
