(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(Conf:RunTest.Config)(ModelConfig:CMem.Config) = struct
  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end
  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)
  module JavaValue = Int64Value.Make(JavaBase.Instr)
  module Java = JavaArch_herd.Make(ArchConfig)(JavaValue)
  module JavaLexParse = struct
    type pseudo     = Java.pseudo
    type token      = JavaParser.token
    module Lexer    = JavaLexer.Make(LexConfig)
    let lexer       = Lexer.token
    let parser      = JavaParser.main
  end
  module JavaS  = JavaSem.Make(Conf)(JavaValue)
  module JavaM  = CMem.Make(ModelConfig)(JavaS)
  module P      = JavaGenParser_lib.Make (Conf) (Java) (JavaLexParse)
  module X      = RunTest.Make (JavaS) (P) (JavaM) (Conf)
  let run = X.run
end


