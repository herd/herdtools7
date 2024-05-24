(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Copyright (c) 2024 Puranjay Mohan <puranjay@kernel.org>                  *)
(*                                                                          *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make (Conf : RunTest.Config) (ModelConfig : MemWithCav12.Config) = struct
  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end

  module ArchConfig = SemExtra.ConfigToArchConfig (Conf)
  module BPFValue = Int64Value.Make (BPFBase.Instr)
  module BPF = BPFArch_herd.Make (ArchConfig) (BPFValue)

  module BPFLexParse = struct
    type instruction = BPF.pseudo
    type token = BPFParser.token

    module Lexer = BPFLexer.Make (LexConfig)

    let lexer = Lexer.token
    let parser = MiscParser.mach2generic BPFParser.main
  end

  module BPFS = BPFSem.Make (Conf) (BPFValue)
  module BPFM = MemWithCav12.Make (ModelConfig) (BPFS)
  module P = GenParser.Make (Conf) (BPF) (BPFLexParse)
  module X = RunTest.Make (BPFS) (P) (BPFM) (Conf)

  let run = X.run
end
