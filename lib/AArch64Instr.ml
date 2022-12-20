(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(C:sig val is_morello : bool end) = struct

  module Lexer =
    AArch64Lexer.Make
      (struct
        let debug = false
        let is_morello = C.is_morello
       end)

  let parse_instr s =
    let lexbuf = Lexing.from_string s in
    let pi =
      GenParserUtils.call_parser
        "AArch64Instr" lexbuf Lexer.token AArch64Parser.one_instr in
    AArch64Base.PseudoI.parsed_tr pi

  include
    AArch64Base.MakeInstr
      (struct
        let is_morello = C.is_morello
        let parser = parse_instr
      end)
end
