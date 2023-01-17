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

module Make(Conf:RunTest.Config)(ModelConfig:MemWithCav12.Config) = struct
  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end
  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)
  module X86_64Value = Int64Value.Make(X86_64Base.Instr)
  module X86_64 = X86_64Arch_herd.Make(ArchConfig)(X86_64Value)
  module X86_64LexParse = struct
    type instruction = X86_64.pseudo
    type token = X86_64Parser.token
    module Lexer = X86_64Lexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic X86_64Parser.main
  end
  module X86_64S = X86_64Sem.Make(Conf)(X86_64Value)
  module X86_64M = MemWithCav12.Make(ModelConfig)(X86_64S)
  module P = GenParser.Make(Conf)(X86_64)(X86_64LexParse)
  module X = RunTest.Make(X86_64S)(P)(X86_64M)(Conf)
  let run = X.run
end

