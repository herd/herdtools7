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
  module ARMValue = Int32Value.Make(ARMBase.Instr)
  module ARM = ARMArch_herd.Make(ArchConfig)(ARMValue)
  module ARMLexParse = struct
    type instruction = ARM.parsedPseudo
    type token = ARMParser.token
    module Lexer = ARMLexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = MiscParser.mach2generic ARMParser.main
  end
  module ARMS = ARMSem.Make(Conf)(ARMValue)
  module ARMM = MemWithCav12.Make(ModelConfig)(ARMS)
  module P = GenParser.Make (Conf) (ARM) (ARMLexParse)
  module X = RunTest.Make (ARMS) (P) (ARMM) (Conf)
  let run = X.run
end

