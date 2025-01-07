(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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
            let info = splitted.Splitter.info
            let variant = TopConf.variant
            let mte_precision = TopConf.mte_precision
            let fault_handling = TopConf.fault_handling
            let sve_vector_length = TopConf.sve_vector_length
            let sme_vector_length = TopConf.sme_vector_length
          end)
      (* Override *)
      include TopConf
      let unroll =
        Option.map
          (fun s ->
             try int_of_string s
             with Failure _ ->
               Warn.user_error "unroll exects an integer argument")
          (MiscParser.get_info_on_info MiscParser.unroll_key
             splitted.Splitter.info)
        |>
        (function | None -> unroll | Some _ as o -> o)
      let fault_handling = TestConf.fault_handling
      let mte_precision = TestConf.mte_precision
      let sve_vector_length = TestConf.sve_vector_length
      let sme_vector_length = TestConf.sme_vector_length
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
      let variant_patched_with_cache_type =
         let dic_pred, idc_pred =
            let open CacheType in
               match cache_type with
               | None ->
                  (fun _ -> false), (fun _ -> false)
               | Some cache_type ->
                  cache_type.dic, cache_type.idc in
         Misc.(|||) Conf.variant (function
            | Variant.DIC -> dic_pred 0
            | Variant.IDC -> idc_pred 0
            | _ -> false) in
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
        let profile = Conf.debug.Debug_herd.profile_cat
        let verbose = Conf.verbose
        let skipchecks = Conf.skipchecks
        let strictskip = Conf.strictskip
        let cycles = Conf.cycles
        let optace = Conf.optace
        let libfind = Conf.libfind
        let variant = variant_patched_with_cache_type
        let dirty = dirty
        let statelessrc11 = Conf.statelessrc11
      end in
      let module ArchConfig = SemExtra.ConfigToArchConfig(Conf) in
      match arch with
      | `PPC ->
         let module X = PPCParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `ARM ->
         let module X = ARMParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `BPF ->
         let module X = BPFParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `AArch64 ->
         if Conf.variant Variant.ASL then
           let module X =
             AArch64ASLParseTest.Make(Conf)(ModelConfig) in
           X.run dirty start_time name chan env splitted
         else
           let module X = AArch64ParseTest.Make(Conf)(ModelConfig) in
           X.run dirty start_time name chan env splitted

      | `X86 ->
         let module X = X86ParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `X86_64 ->
         let module X = X86_64ParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `MIPS ->
         let module X = MIPSParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `RISCV ->
         let module X = RISCVParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `C ->
         let module X = CParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `JAVA ->
         let module X = JAVAParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
      | `LISA ->
         let module X = LISAParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
(* START NOTWWW *)
      | `ASL ->
         let module X = ASLParseTest.Make(Conf)(ModelConfig) in
         X.run dirty start_time name chan env splitted
(* END NOTWWW *)
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
