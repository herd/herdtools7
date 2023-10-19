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
  module PPCValue = Int64Value.Make(PPCBase.Instr)
  module PPC = PPCArch_herd.Make(ArchConfig)(PPCValue)
  module PPCLexParse = struct
    type instruction = PPC.parsedPseudo
    type token = PPCParser.token
    module Lexer = PPCLexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic PPCParser.main
  end
  module PPCS = PPCSem.Make(Conf)(PPCValue)
  module PPCM = MemWithCav12.Make(ModelConfig)(PPCS)
  module P = GenParser.Make (Conf) (PPC) (PPCLexParse)
  module X = RunTest.Make (PPCS) (P) (PPCM) (Conf)
  let run = X.run
end
