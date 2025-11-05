(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open ARMBase

type exec = instruction
type t = instruction

let from_exec = Misc.identity
let to_exec = Misc.identity

let compare = Misc.polymorphic_compare
let eq = (=)

let pp = function
  | I_NOP -> "NOP"
  | i -> Printf.sprintf "instr:%S" (dump_instruction i)

module Lexer =
  ARMLexer.Make
    (struct
      let debug = false
    end)

let parse_instr s =
  let lexbuf = Lexing.from_string s in
  let pi =
    GenParserUtils.call_parser
      "ARMInstr" lexbuf Lexer.token ARMParser.one_instr in
  parsed_tr pi

let tr =
  let open InstrLit in
  function
  | LIT_NOP -> I_NOP
  | LIT_INSTR s -> parse_instr s

let can_overwrite _ = false

module Set =
  MySet.Make
    (struct
      type t = instruction
      let compare = compare
    end)
