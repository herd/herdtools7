(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

module Make (Conf : RunTest.Config) (ModelConfig : MemCat.Config) = struct
  module ArchConfig = SemExtra.ConfigToArchConfig (Conf)
  module Conf = struct
    module C = Conf
    let libfind = Conf.libfind
    let dirty = None
  end
  module ASLS = ASLSem.Make (Conf)
  module ASLA = ASLS.A

  module ASLLexParse = struct
    type instruction = ASLA.parsedPseudo
    type token = Asllib.Tokens.token

    let lexer =
        let module Lexer = Asllib.Lexer.Make(struct
          let allow_double_underscore = false
          let allow_unknown = false
        end) in
        Lexer.token

    let parser =
      let version =
        if Conf.C.variant (Variant.ASLVersion `ASLv0) then `ASLv0 else `ASLv1
      in
      ASLBase.asl_generic_parser version
  end

  module ASLM = MemCat.Make (ModelConfig) (ASLS)
  module P = GenParser.Make (Conf.C) (ASLA) (ASLLexParse)
  module X = RunTest.Make (ASLS) (P) (ASLM) (Conf.C)

  let run = X.run
end
