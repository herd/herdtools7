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
  val archcheck : bool
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
end

(**********************)
(* Stuff for non-bell *)
(**********************)

(* do not check *)
module NoCheck = struct
  let check parsed= parsed
end

module Top (TopConf:Config) = struct

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

      let run dirty start_time filename chan env splitted =
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
            if S.A.is_mixed then begin match TopConf.byte with
            | MachSize.Tag.Size sz -> sz
            | MachSize.Tag.Auto ->
                let szs = test.Test_herd.access_size in
                match szs with
                | [] -> MachSize.Byte
                | [sz] -> MachSize.pred sz
                | sz::_ -> sz
            end else begin
              (* Cannot that easily check the test not to mix sizes,
                 as there are several locations in test that may be of
                 different sizes *)
              MachSize.Byte
            end in
(* And run test *)
          let module T =
            Top_herd.Make
              (struct
                include TopConf
                let byte = sz
                let dirty = dirty
              end)(M) in
          T.run start_time test ;
          env
        with TestHash.Seen -> env
    end

  module SP =
    Splitter.Make
      (struct
        let debug = TopConf.debug.Debug_herd.lexer
        let check_rename = TopConf.check_rename
      end)

  let check_arch_model a m =
    if TopConf.archcheck then match m with
    | Model.Generic (o,_,_) ->
        begin match o.ModelOption.arch with
        | None -> m
        | Some b ->
            if a = b then m
            else
              Warn.user_error
                "Architecture mismatch between test and model (%s vs. %s)"
                (Archs.pp a)  (Archs.pp b)
        end
    | m -> m
    else m

  let do_from_file start_time env name chan =
    if TopConf.debug.Debug_herd.files then MyLib.pp_debug name ;
(* First split the input file in sections *)
    let (splitted:Splitter.result) =  SP.split name chan in
    let tname = splitted.Splitter.name.Name.name in
    let module Conf = struct (* override the precision and variant fields *)
      (* Modify variant with the 'Variant' field of test *)
      module TestConf =
        TestVariant.Make
          (struct
            module Opt = Variant
            let set_precision = Variant.set_precision
            let info = splitted.Splitter.info
            let precision = TopConf.precision
            let variant = TopConf.variant
          end)
      (* Override *)
      include TopConf
      let precision = TestConf.precision
      let variant = TestConf.variant
    end in
    if Conf.check_name tname then begin
    (* Get arch *)
      let arch = splitted.Splitter.arch in
(* Now, we have the architecture, call specific parsers
   generically. *)
      let module LexConfig = struct
        let debug = Conf.debug.Debug_herd.lexer
      end in
      let model =
        let m = match Conf.model with
        | None -> Model.get_default_model Conf.variant arch
        | Some m -> m in
        let m = match m with
        | Model.File fname ->
            let module P =
              ParseModel.Make
                (struct
                  include LexUtils.Default
                  let libfind = Conf.libfind
                end) in
            Model.Generic (P.parse fname)
        | _ -> m in
        check_arch_model arch m in

      let dirty = DirtyBit.get splitted.Splitter.info in

      let module ModelConfig = struct
        let bell_model_info = Conf.bell_model_info
        let model = model
        let showsome =
          begin match Conf.outputdir with
          | PrettyConf.StdoutOutput | PrettyConf.Outputdir _ -> true
          | _ -> false
          end || Conf.PC.gv || Conf.PC.evince || Conf.variant Variant.MemTag
              || Conf.variant Variant.Morello
        let through = Conf.through
        let debug = Conf.debug.Debug_herd.barrier
        let debug_files = Conf.debug.Debug_herd.files
        let verbose = Conf.verbose
        let skipchecks = Conf.skipchecks
        let strictskip = Conf.strictskip
        let cycles = Conf.cycles
        let optace = Conf.optace
        let libfind = Conf.libfind
        let variant = Conf.variant
        let dirty = dirty
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
          let module PPCM = PPCMem.Make(ModelConfig)(PPCS) in
          let module P = GenParser.Make (Conf) (PPC) (PPCLexParse) in
          let module X = Make (PPCS) (P) (NoCheck) (PPCM) in
          X.run dirty start_time name chan env splitted
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
          let module ARMM = ARMMem.Make(ModelConfig)(ARMS) in
          let module P = GenParser.Make (Conf) (ARM) (ARMLexParse) in
          let module X = Make (ARMS) (P) (NoCheck) (ARMM) in
          X.run dirty start_time name chan env splitted
      | `AArch64 ->
         let module
             AArch64Make(V:Value.AArch64) = struct
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

             module AArch64S = AArch64Sem.Make(AArch64SemConf)(V)

             module AArch64C = (* TODO: Checking something? *)
               BellCheck.Make
                 (struct
                   let debug = Conf.debug.Debug_herd.barrier
                   let compat = Conf.variant Variant.BackCompat
                 end)
                 (AArch64)
                 (struct
                   let info = Misc.snd_opt Conf.bell_model_info
                   let get_id_and_list _ = raise Not_found
                   let set_list _ _ = assert false
                   let tr_compat i = i
                 end)

             module AArch64M = AArch64Mem.Make(ModelConfig)(AArch64S)

             module P = GenParser.Make (Conf) (AArch64) (AArch64LexParse)

             module X = Make (AArch64S) (P) (AArch64C) (AArch64M)

             let run = X.run
         end in
(* Markers START/END below are for excluding source when compiling
   the web interface *)
(* START NOTWWW *)
         if Conf.variant Variant.Morello then
           let module X = AArch64Make(CapabilityValue) in
           X.run dirty start_time name chan env splitted
         else if Conf.variant Variant.Neon then
           let module X = AArch64Make(Uint128Value) in
           X.run dirty start_time name chan env splitted
         else
(* END NOTWWW *)
           let module X = AArch64Make(AArch64Value) in
           X.run dirty start_time name chan env splitted
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
          let module X86M = X86Mem.Make(ModelConfig)(X86S) in
          let module P = GenParser.Make (Conf) (X86) (X86LexParse) in
          let module X = Make (X86S) (P) (NoCheck) (X86M) in
          X.run dirty start_time name chan env splitted

      | `X86_64 ->
          let module X86_64 = X86_64Arch_herd.Make(ArchConfig)(Int64Value) in
          let module X86_64LexParse = struct
            type instruction = X86_64.pseudo
            type token = X86_64Parser.token
            module Lexer = X86_64Lexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic X86_64Parser.main
          end in
          let module X86_64S = X86_64Sem.Make(Conf)(Int64Value) in
          let module X86_64M = X86_64Mem.Make(ModelConfig)(X86_64S) in
          let module P = GenParser.Make(Conf)(X86_64)(X86_64LexParse) in
          let module X = Make(X86_64S)(P)(NoCheck)(X86_64M) in
          X.run dirty start_time name chan env splitted


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
          let module MIPSM = MIPSMem.Make(ModelConfig)(MIPSS) in
          let module P = GenParser.Make (Conf) (MIPS) (MIPSLexParse) in
          let module X = Make (MIPSS) (P) (NoCheck) (MIPSM) in
          X.run dirty start_time name chan env splitted

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
          X.run dirty start_time name chan env splitted
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
          X.run dirty start_time name chan env splitted
      | `CPP as arch -> Warn.fatal "no support for arch '%s'" (Archs.pp arch)

      | `JAVA -> 
        let module Java = JavaArch_herd.Make(ArchConfig)(Int64Value) in
        let module JavaLexParse = struct
          type pseudo     = Java.pseudo
          type token      = JavaParser.token
          module Lexer    = JavaLexer.Make(LexConfig)
          let lexer       = Lexer.token
          let parser      = JavaParser.main
        end in 
        let module JavaS  = JavaSem.Make(Conf)(Int64Value) in
        let module JavaM  = JavaMem.Make(ModelConfig)(JavaS) in
        let module P      = JavaGenParser_lib.Make (Conf) (Java) (JavaLexParse) in
        let module X      = Make (JavaS) (P) (NoCheck) (JavaM) in
        
        X.run dirty start_time name chan env splitted

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
                let info = Misc.snd_opt Conf.bell_model_info
                let get_id_and_list = Bell.get_id_and_list
                let set_list = Bell.set_list
                let tr_compat = Bell.tr_compat
              end) in
          let module P = GenParser.Make (Conf) (Bell) (BellLexParse) in
          let module X = Make (BellS) (P) (BellC) (BellM) in
          X.run dirty start_time name chan env splitted
    end else env

(* Enter here... *)

  let from_file name env =
(* START NOTWWW *)
(* Interval timer will be stopped just before output, see top_herd *)
    Itimer.start name TopConf.timeout ;
(* END NOTWWW *)
    let start_time = Sys.time () in
    Misc.input_protect (do_from_file start_time env name) name
end
