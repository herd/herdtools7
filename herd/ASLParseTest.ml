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

module Make (Conf : RunTest.Config) (ModelConfig : MemCat.Config) = struct
  module ArchConfig = SemExtra.ConfigToArchConfig (Conf)
  module ASLS = ASLSem.Make (Conf)
  module ASLA = ASLS.ASL64AH

  module ASLLexParse = struct
    type instruction = ASLA.parsedPseudo
    type token = Asllib.Parser.token

    let lexer = Asllib.Lexer.token
    let parser = ASLBase.asl_generic_parser
  end

  module ASLM = MemCat.Make (ModelConfig) (ASLS)
  module P = GenParser.Make (Conf) (ASLA) (ASLLexParse)
  module X = RunTest.Make (ASLS) (P) (ASLM) (Conf)

  let run = X.run
end
