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

  let is_morello = Conf.variant Variant.Morello

  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
    let is_morello = is_morello
  end

  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)

  module AArch64ASLValue =
    AArch64ASLValue.Make
      (struct
        let is_morello = is_morello
      end)

  module AArch64ASLArch =
    AArch64Arch_herd.Make(ArchConfig)(AArch64ASLValue)

  module AArch64ASLLexParse = struct
    type instruction = AArch64ASLArch.parsedPseudo
    type token = AArch64Parser.token
    module Lexer = AArch64Lexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = AArch64Parser.main
  end

  let run dirty start_time name chan env splitted =
    let module SemConf = struct
        module C = Conf
        let dirty = ModelConfig.dirty
        let procs_user = ProcsUser.get splitted.Splitter.info
      end in
    let module AArch64ASLS =
      AArch64ASLSem.Make(SemConf)(AArch64ASLValue) in
    let module
        AArch64ASLM = MemCat.Make(ModelConfig)(AArch64ASLS) in
    let module P =
      GenParser.Make (Conf) (AArch64ASLArch) (AArch64ASLLexParse) in
    let module X =
      RunTest.Make (AArch64ASLS) (P) (AArch64ASLM) (Conf) in
    X.run dirty start_time name chan env splitted
end

