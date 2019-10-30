(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(*******************************)
(* Run a test from source file *)
(*******************************)
module type Config = sig
  val model : Model.t option
  val through : Model.through
  val strictskip : bool
  val cycles : StringSet.t
  val bell_model_info : (string * BellModel.info) option
  val macros : string option
  val check_name : string -> bool
  val check_rename : string -> string option
  val libfind : string -> string
  include GenParser.Config
  include Top_herd.CommonConfig
  include Sem.Config

  val statelessrc11 : bool
  val byte : MachSize.Tag.t
  val precision : bool
end

(**********************)
(* Stuff for non-bell *)
(**********************)

(* do not check *)
module NoCheck = struct
  let check parsed= parsed
end

module Top (Conf:Config) = struct
  module Make
      (S:Sem.Semantics)
      (P:sig
         type pseudo
         val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
       end with type pseudo = S.A.pseudo)
      (Check:
         sig
           val check : S.A.pseudo MiscParser.t -> S.A.pseudo MiscParser.t
         end)
      (M:XXXMem.S with module S = S) =
    struct
      module T = Test_herd.Make(S.A)

      let run start_time filename chan env splitted =
        try
          let parsed = P.parse chan splitted in

          (* Additional checks *)
          let parsed = Check.check parsed in


          let name = splitted.Splitter.name in
          let hash = MiscParser.get_hash parsed in
          let env = match hash with
          | None -> env
          | Some hash ->
              TestHash.check_env env name.Name.name filename hash in
          let test = T.build name parsed in
(* Compute basic machine size *)
          let sz =
            if S.A.is_mixed then begin match Conf.byte with
            | MachSize.Tag.Size sz -> sz
            | MachSize.Tag.Auto ->
              let szs = test.Test_herd.access_size in
              match szs with
              | [] -> MachSize.Byte
              | [sz] -> MachSize.pred sz
              | sz::_ -> sz
            end else MachSize.Byte in
(* And run test *)
          let module T =
            Top_herd.Make(struct include Conf let byte = sz end)(M) in
          T.run start_time test ;
          env
          with TestHash.Seen -> env
    end

  module SP =
    Splitter.Make
      (struct
        let debug = Conf.debug.Debug_herd.lexer
        let check_rename = Conf.check_rename
      end)

  let do_from_file start_time env name chan =
(* First split the input file in sections *)
    let (splitted:Splitter.result) =  SP.split name chan in
    let tname = splitted.Splitter.name.Name.name in
    if Conf.check_name tname then begin
      let arch = splitted.Splitter.arch in
(* Now, we have the architecture, call specific parsers
   generically. *)
      let module LexConfig = struct
        let debug = Conf.debug.Debug_herd.lexer
      end in
      let module ModelConfig = struct
        let bell_model_info = Conf.bell_model_info
        let model =
          let m =
            match Conf.model with
            | None -> Model.get_default_model arch
            | Some m -> m in
          match m with
          | Model.File fname ->
              let module P =
                ParseModel.Make
                  (struct
                    include LexUtils.Default
                    let libfind = Conf.libfind
                   end) in
              let (b,_,_) as r = P.parse fname in
              if b <> ModelOption.default then
                Warn.fatal
                  "default model in \"%s\" does not have default options"
                  fname ;
              Model.Generic r
          | _ -> m
        let showsome =
          begin match Conf.outputdir with PrettyConf.StdoutOutput | PrettyConf.Outputdir _ -> true | _ -> false end
        || Conf.PC.gv || Conf.PC.evince
        let through = Conf.through
        let debug = Conf.debug.Debug_herd.barrier
        let verbose = Conf.verbose
        let skipchecks = Conf.skipchecks
        let strictskip = Conf.strictskip
        let cycles = Conf.cycles
        let optace = Conf.optace
        let libfind = Conf.libfind
        let variant = Conf.variant

        let statelessrc11 = Conf.statelessrc11
      end in
      let module ArchConfig = SemExtra.ConfigToArchConfig(Conf) in
      match arch with
      | `PPC ->
          let module PPC =
            PPCArch_herd.Make(ArchConfig)(Int64Value) in
          let module PPCLexParse = struct
            type instruction = PPC.parsedPseudo
            type token = PPCParser.token
            module Lexer = PPCLexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic PPCParser.main
          end in
          let module PPCS = PPCSem.Make(Conf)(Int64Value) in
          let module PPCBarrier = struct
            type a = PPC.barrier
            type b = SYNC | LWSYNC | ISYNC | EIEIO
            let a_to_b a = match a with
            | PPC.Sync -> SYNC
            | PPC.Lwsync -> LWSYNC
            | PPC.Isync -> ISYNC
            | PPC.Eieio ->  EIEIO
          end in
          let module PPCM = PPCMem.Make(ModelConfig)(PPCS) (PPCBarrier) in
          let module P = GenParser.Make (Conf) (PPC) (PPCLexParse) in
          let module X = Make (PPCS) (P) (NoCheck) (PPCM) in
          X.run start_time name chan env splitted

      | `ARM ->
          let module ARM = ARMArch_herd.Make(ArchConfig)(Int32Value) in
          let module ARMLexParse = struct
            type instruction = ARM.parsedPseudo
            type token = ARMParser.token
            module Lexer = ARMLexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic ARMParser.main
          end in
          let module ARMS = ARMSem.Make(Conf)(Int32Value) in
          let module ARMBarrier = struct
            type a = ARM.barrier
            type b =
              | ISB
              | DMB of ARMBase.barrier_option
              | DSB of ARMBase.barrier_option
            let a_to_b a = match a with
            | ARM.DMB o -> DMB o
            | ARM.DSB o -> DSB o
            | ARM.ISB -> ISB
          end in
          let module ARMM = ARMMem.Make(ModelConfig)(ARMS)(ARMBarrier) in
          let module P = GenParser.Make (Conf) (ARM) (ARMLexParse) in
          let module X = Make (ARMS) (P) (NoCheck) (ARMM) in
          X.run start_time name chan env splitted

      | `AArch64 ->
          let module AArch64Conf = struct
            include ArchConfig
            let moreedges = Conf.moreedges
          end in
          let module AArch64 = AArch64Arch_herd.Make(AArch64Conf)(Int64Value) in
          let module AArch64LexParse = struct
            type instruction = AArch64.parsedPseudo
            type token = AArch64Parser.token
            module Lexer = AArch64Lexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic AArch64Parser.main
          end in
          let module AArch64S = AArch64Sem.Make(Conf)(Int64Value) in
          let module AArch64Barrier = struct
            type a = AArch64.barrier
            type b =
              | ISB
              | DMB of AArch64Base.mBReqDomain * AArch64Base.mBReqTypes
              | DSB of AArch64Base.mBReqDomain * AArch64Base.mBReqTypes
            let a_to_b a = match a with
            | AArch64.DMB(d,t) -> DMB(d,t)
            | AArch64.DSB(d,t) -> DSB(d,t)
            | AArch64.ISB -> ISB
          end in
          let module AArch64M =
            AArch64Mem.Make(ModelConfig)(AArch64S) (AArch64Barrier) in
          let module P = GenParser.Make (Conf) (AArch64) (AArch64LexParse) in
          let module X = Make (AArch64S) (P) (NoCheck) (AArch64M) in
          X.run start_time name chan env splitted

      | `X86 ->
          let module X86 = X86Arch_herd.Make(ArchConfig)(Int32Value) in
          let module X86LexParse = struct
            type instruction = X86.pseudo
            type token = X86Parser.token
            module Lexer = X86Lexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic X86Parser.main
          end in
          let module X86S = X86Sem.Make(Conf)(Int32Value) in
          let module X86Barrier = struct
            type a = X86.barrier
            type b = MFENCE|LFENCE|SFENCE
            let a_to_b a = match a with
            | X86.Mfence -> MFENCE
            | X86.Sfence -> SFENCE
            | X86.Lfence -> LFENCE
          end in
          let module X86M = X86Mem.Make(ModelConfig)(X86S) (X86Barrier) in
          let module P = GenParser.Make (Conf) (X86) (X86LexParse) in
          let module X = Make (X86S) (P) (NoCheck) (X86M) in
          X.run start_time name chan env splitted

      | `MIPS ->
          let module MIPS = MIPSArch_herd.Make(ArchConfig)(Int64Value) in
          let module MIPSLexParse = struct
            type instruction = MIPS.pseudo
            type token = MIPSParser.token
            module Lexer = MIPSLexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic MIPSParser.main
          end in
          let module MIPSS = MIPSSem.Make(Conf)(Int64Value) in
          let module MIPSBarrier = struct
            type a = MIPS.barrier
            type b = SYNC
            let a_to_b a = match a with
            | MIPS.Sync -> SYNC
          end in
          let module MIPSM = MIPSMem.Make(ModelConfig)(MIPSS)(MIPSBarrier) in
          let module P = GenParser.Make (Conf) (MIPS) (MIPSLexParse) in
          let module X = Make (MIPSS) (P) (NoCheck) (MIPSM) in
          X.run start_time name chan env splitted

      | `RISCV ->
          let module RISCV = RISCVArch_herd.Make(ArchConfig)(Int64Value) in
          let module RISCVLexParse = struct
            type instruction = RISCV.parsedPseudo
            type token = RISCVParser.token
            module Lexer = RISCVLexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic RISCVParser.main
          end in
          let module RISCVS = RISCVSem.Make(Conf)(Int64Value) in
          let module RISCVM = RISCVMem.Make(ModelConfig)(RISCVS) in
          let module P = GenParser.Make (Conf) (RISCV) (RISCVLexParse) in
          let module X = Make (RISCVS) (P) (NoCheck) (RISCVM) in
          X.run start_time name chan env splitted
      | `C ->
        let module C = CArch_herd.Make(ArchConfig)(Int64Value) in
        let module CLexParse = struct
          (* Parsing *)
          type pseudo = C.pseudo
          type token = CParser.token
          module Lexer = CLexer.Make(LexConfig)
          let shallow_lexer = Lexer.token false
          let deep_lexer = Lexer.token true
          let shallow_parser = CParser.shallow_main
          let deep_parser = CParser.deep_main

          (* Macros *)
          type macro = C.macro
          let macros_parser = CParser.macros
          let macros_expand = CBase.expand
        end in
        let module CS = CSem.Make(Conf)(Int64Value) in
        let module CM = CMem.Make(ModelConfig)(CS) in
        let module P = CGenParser_lib.Make (Conf) (C) (CLexParse) in
        let module X = Make (CS) (P) (NoCheck) (CM) in
        X.run start_time name chan env splitted
      | `CPP as arch -> Warn.fatal "no support for arch '%s'" (Archs.pp arch)
      | `LISA ->
        let module Bell = BellArch_herd.Make(ArchConfig)(Int64Value) in
        let module BellLexParse = struct
          type instruction = Bell.parsedPseudo
          type token = LISAParser.token
          module Lexer = BellLexer.Make(LexConfig)
          let lexer = Lexer.token
          let parser = LISAParser.main
        end in

        let module BellS = BellSem.Make(Conf)(Int64Value) in
        let module BellM = BellMem.Make(ModelConfig)(BellS) in
        let module BellC =
          BellCheck.Make
            (struct
              let debug = Conf.debug.Debug_herd.barrier
              let compat = Conf.variant Variant.BackCompat
            end)
            (Bell)
            (struct
              let info = Misc.app_opt (fun (_,y) -> y) Conf.bell_model_info
              let get_id_and_list = Bell.get_id_and_list
              let set_list = Bell.set_list
              let tr_compat = Bell.tr_compat
             end) in
        let module P = GenParser.Make (Conf) (Bell) (BellLexParse) in
        let module X = Make (BellS) (P) (BellC) (BellM) in
        X.run start_time name chan env splitted

    end else env

(* Enter here... *)
  let from_file name env =
    let start_time = Sys.time () in
    Misc.input_protect (do_from_file start_time env name) name
end
