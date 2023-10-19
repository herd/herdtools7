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
  module X86Value = Int32Value.Make(X86Base.Instr)
  module X86 = X86Arch_herd.Make(ArchConfig)(X86Value)
  module X86LexParse = struct
    type instruction = X86.pseudo
    type token = X86Parser.token
    module Lexer = X86Lexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic X86Parser.main
  end
  module X86S = X86Sem.Make(Conf)(X86Value)
  module X86M = MemWithCav12.Make(ModelConfig)(X86S)
  module P = GenParser.Make (Conf) (X86) (X86LexParse)
  module X = RunTest.Make (X86S) (P) (X86M) (Conf)
  let run = X.run
end
