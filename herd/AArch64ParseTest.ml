(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(Conf:RunTest.Config)(ModelConfig:MemCat.Config) = struct

  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)

  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end

  let run dirty start_time name chan env splitted =

    let module Top (MakeSem:AArch64Sig.MakeSemantics) =
      struct
        let is_morello = Conf.variant Variant.Morello
        module ConfMorello = struct let is_morello = is_morello end
        module AArch64Make (V:Value.AArch64) = struct
          module AArch64 = AArch64Arch_herd.Make(ArchConfig)(V)
          module AArch64LexParse = struct
            type instruction = AArch64.parsedPseudo
            type token = AArch64Parser.token
            module Lexer =
              AArch64Lexer.Make
                (struct
                  include LexConfig
                  let is_morello =  Conf.variant Variant.Morello
                end)
            let lexer = Lexer.token
            let parser = (*MiscParser.mach2generic*) AArch64Parser.main
          end
          module AArch64SemConf = struct
            module C = Conf
            let dirty = ModelConfig.dirty
            let procs_user = ProcsUser.get splitted.Splitter.info
          end
          module AArch64S = MakeSem(AArch64SemConf)(V)
          module AArch64M = MemCat.Make(ModelConfig)(AArch64S)
          module P0 =
            GenParser.Make (Conf) (AArch64) (AArch64LexParse)
          module P =
            struct
              type pseudo = AArch64.pseudo
              let parse chan splitted =
                let tst = P0.parse chan splitted in
                let () = AArch64.check tst in
                tst
            end
          module X = RunTest.Make (AArch64S) (P) (AArch64M) (Conf)
        end
(*
 * Markers START/END below are for excluding source
 * when compiling the web interface
 *)

        let run =
(* START NOTWWW *)
          if is_morello then
            let module  AArch64Value = CapabilityValue.Make(ConfMorello) in
            let module X = AArch64Make(AArch64Value) in
            X.X.run
          else if Conf.variant Variant.Neon then
            let module AArch64Value = Uint128Value.Make(ConfMorello) in
            let module X = AArch64Make(AArch64Value) in
            X.X.run
          else
(* END NOTWWW *)
            let module AArch64Value = AArch64Value.Make(ConfMorello) in
            let module X = AArch64Make(AArch64Value) in
            X.X.run
      end in

    let module Run = Top(AArch64Sem.Make) in
    Run.run dirty start_time name chan env splitted
end
