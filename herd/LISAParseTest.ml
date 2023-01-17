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

module Make(Conf:RunTest.Config)(ModelConfig:BellMem.Config) = struct
  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end
  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)
  module LISAValue = Int64Value.Make(BellBase.Instr)
  module Bell = BellArch_herd.Make(ArchConfig)(LISAValue)
  module BellLexParse = struct
    type instruction = Bell.parsedPseudo
    type token = LISAParser.token
    module Lexer = BellLexer.Make(LexConfig)
    let lexer = Lexer.token
    let parser = LISAParser.main
  end
  module BellS = BellSem.Make(Conf)(LISAValue)
  module BellM = BellMem.Make(ModelConfig)(BellS)
  module BellC =
    BellCheck.Make
      (struct
        let debug = Conf.debug.Debug_herd.barrier
        let compat = Conf.variant Variant.BackCompat
      end)
      (Bell)
      (struct
        let info = Misc.snd_opt Conf.bell_model_info
        let get_id_and_list = Bell.get_id_and_list
        let set_list = Bell.set_list
        let tr_compat = Bell.tr_compat
      end)
  module P =
    struct
      module P =
        GenParser.Make (Conf) (Bell) (BellLexParse)
      type pseudo = P.pseudo
      let parse chan splitted =  BellC.check (P.parse chan splitted)
    end
  module X = RunTest.Make (BellS) (P) (BellM) (Conf)
  let run = X.run
end
