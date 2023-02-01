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
(* Authors:                                                                 *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)



module Top (TopConf:RunTest.Config) = struct

  module SP =
    Splitter.Make
      (struct
        let debug = TopConf.debug.Debug_herd.lexer
        let check_rename = TopConf.check_rename
      end)

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
      let model =
        GetModel.parse
          Conf.archcheck arch Conf.libfind Conf.variant Conf.model in

      let cache_type = CacheType.get splitted.Splitter.info in
      let dirty = DirtyBit.get splitted.Splitter.info in

      let module ModelConfig = struct
        let bell_model_info = Conf.bell_model_info
        let model = model
        let showsome =
          begin match Conf.outputdir with
          | PrettyConf.StdoutOutput | PrettyConf.Outputdir _ -> true
          | _ -> false
          end || Misc.is_some Conf.PC.view || Conf.variant Variant.MemTag
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
        let cache_type = cache_type
        let statelessrc11 = Conf.statelessrc11
      end in
      let module ArchConfig = SemExtra.ConfigToArchConfig(Conf) in
      match arch with
      | `PPC ->
         let module X = PPCParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `ARM ->
         let module X = ARMParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `AArch64 ->
         let module X = AArch64ParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted

      | `X86 ->
         let module X = X86ParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `X86_64 ->
         let module X = X86_64ParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `MIPS ->
         let module X = MIPSParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `RISCV ->
         let module X = MIPSParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `C ->
         let module X = CParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `JAVA ->
         let module X = CParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `LISA ->
         let module X = LISAParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | `ASL ->
         let module X = ASLParseTest.Make(Conf)(ModelConfig) in
         X.run cache_type dirty start_time name chan env splitted
      | arch -> Warn.fatal "no support for arch '%s'" (Archs.pp arch)
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
