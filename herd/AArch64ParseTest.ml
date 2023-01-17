
module Make(Conf:RunTest.Config)(ModelConfig:MemCat.Config) = struct

  module ArchConfig = SemExtra.ConfigToArchConfig(Conf)

  module LexConfig = struct
    let debug = Conf.debug.Debug_herd.lexer
  end

  let run cache_type dirty start_time name chan env splitted =

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
            let cache_type = ModelConfig.cache_type
            let procs_user = ProcsUser.get splitted.Splitter.info
          end
          module AArch64S = MakeSem(AArch64SemConf)(V)
          module AArch64M = MemCat.Make(ModelConfig)(AArch64S)
          module P =
            GenParser.Make (Conf) (AArch64) (AArch64LexParse)
          module X = RunTest.Make (AArch64S) (P) (AArch64M) (Conf)
        end
(*
 * Markers START/END below are for excluding source
 * when compiling the web interface
 *)

(* START NOTWWW *)
        let run =
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

(* START NOTWWW *)
      if Conf.variant Variant.ASL then
        let module Run =  Top(AArch64ASLSem.Make) in
        Run.run cache_type dirty start_time name chan env splitted
      else
(* END NOTWWW *)
        let module Run = Top(AArch64Sem.Make) in
        Run.run cache_type dirty start_time name chan env splitted
end
