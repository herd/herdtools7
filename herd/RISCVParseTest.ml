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

module Make(Conf:RunTest.Config)(ModelConfig:MemCat.Config) = struct
  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end
  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)
  module RISCVValue = Int64Value.Make(RISCVBase.Instr)
  module RISCV = RISCVArch_herd.Make(ArchConfig)(RISCVValue)
  module RISCVLexParse = struct
    type instruction = RISCV.parsedPseudo
    type token = RISCVParser.token
    module Lexer = RISCVLexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic RISCVParser.main
  end
  module RISCVS = RISCVSem.Make(Conf)(RISCVValue)
  module RISCVM = MemCat.Make(ModelConfig)(RISCVS)
  module P = GenParser.Make (Conf) (RISCV) (RISCVLexParse)
  module X = RunTest.Make (RISCVS) (P) (RISCVM) (Conf)
  let run = X.run
end
