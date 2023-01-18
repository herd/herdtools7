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
  module MIPSValue = Int64Value.Make(MIPSBase.Instr)
  module MIPS = MIPSArch_herd.Make(ArchConfig)(MIPSValue)
  module MIPSLexParse = struct
    type instruction = MIPS.pseudo
    type token = MIPSParser.token
    module Lexer = MIPSLexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic MIPSParser.main
  end
  module MIPSS = MIPSSem.Make(Conf)(MIPSValue)
  module MIPSM = MemWithCav12.Make(ModelConfig)(MIPSS)
  module P = GenParser.Make (Conf) (MIPS) (MIPSLexParse)
  module X = RunTest.Make (MIPSS) (P) (MIPSM) (Conf)
  let run = X.run
end

