(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***********************************************)
(* Parse a source file for the needs of litmus *)
(* (And then compile test)                     *)
(***********************************************)

open Answer

module type CommonConfig = sig
  val verbose : int
  val limit : bool
  val timeloop : int
  val stride : Stride.t
  val avail : int option
  val runs : int
  val size : int
  val noccs : int
  val timelimit : float option
  val isync : bool
  val speedcheck : Speedcheck.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val memory : Memory.t
  val alloc : Alloc.t
  val doublealloc : bool
  val threadstyle : ThreadStyle.t
  val launch : Launch.t
  val barrier : Barrier.t
  val linkopt : string
  val logicalprocs : int list option
  val affinity : Affinity.t
  val targetos : TargetOS.t
  val is_out : bool
  val exit_cond : bool
  val sleep : int
  val driver : Driver.t
  val crossrun : Crossrun.t
  val adbdir : string
  val makevar : string list
  val gcc : string
  val c11 : bool
  val c11_fence : bool
  val ascall : bool
  val variant : Variant_litmus.t -> bool
  val stdio : bool
  val xy : bool
  val pldw : bool
  val cacheflush : bool
  val morearch : MoreArch.t
  val carch : Archs.System.t option
  val syncconst : int
  val numeric_labels : bool
  val kind : bool
  val force_affinity : bool
  val smtmode : Smt.t
  val smt : int
  val nsockets : int
  val contiguous : bool
  val noalign : Align.t option
  val syncmacro : int option
  val collect : Collect.t
  val hexa : bool
  val verbose_barrier : bool
  val verbose_prelude : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val check_nstates : string -> int option
  val cross : bool
  val tarname : string
  val hint : string option
  val no : string option
  val index : string option
end

module type TopConfig = sig
  include CommonConfig
  val platform : string
  val check_name : string -> bool
  val check_rename : string -> string option
  (* Arch dependent options *)
  val mkopt : Option.opt -> Option.opt
  (* Mode *)
  val mode : Mode.t
  (* usearch *)
  val usearch : UseArch.t
  (* Hum *)
  val asmcomment : string option
  val asmcommentaslabel : bool
end

module type Config = sig
  include GenParser.Config
  include Compile.Config
  val asmcommentaslabel : bool
  (* Additions for Presi *)
  val line : int
  val noccs : int
  val timelimit : float option
  val check_nstates : string -> int option
  (* End of additions *)
  include Skel.Config
  include Run_litmus.Config
  val limit : bool
  val sysarch : Archs.System.t
  val word : Word.t
end

module Top (OT:TopConfig) (Tar:Tar.S) : sig
  val from_files : string list -> unit
end = struct

  let check_variant v a =
    if OT.variant v && not (Variant_litmus.ok v a) then
      Warn.user_error
        "variant %s does not apply to arch %s"
        (Variant_litmus.pp v)
        (Archs.pp a)


  (************************************************************)
  (* Some configuration dependent stuff, to be performed once *)
  (************************************************************)

  (* Avoid cycles *)
  let read_no fname =
    Misc.input_protect
      (fun chan -> MySys.read_list chan (fun s -> Some s))
      fname

  let avoid_cycle =
    let xs = match OT.no with
      | None -> []
      | Some fname -> read_no fname in
    let set = StringSet.of_list xs in
    fun cy -> StringSet.mem cy set

  (* hints *)
  let hint = match OT.hint with
    | None -> Hint.empty
    | Some fname -> Hint.read fname

  module W = Warn.Make(OT)


  module Utils (O:Config) (A':Arch_litmus.Base)
           (Lang:Language.S
            with type arch_reg = A'.Out.arch_reg
             and type t = A'.Out.t
             and module RegMap = A'.RegMap)
           (Pseudo:PseudoAbstract.S) =
    struct
      module T = Test_litmus.Make(O)(A')(Pseudo)
      module R = Run_litmus.Make(O)(Tar)(T.D)
      module H = LitmusUtils.Hash(O)

      let get_cycle t =
        let info = t.MiscParser.info in
        List.assoc "Cycle" info

      let cycle_ok avoid t =
        try
          let cy = get_cycle t in
          not (avoid cy)
        with Not_found -> true


      let change_hint hint name t =
        try
          let more_info = Hint.get hint name in
          let info =
            more_info @
              List.filter
                (fun (k,_) ->
                  try
                    let _ = List.assoc k more_info in
                    false
                  with Not_found -> true)
                t.MiscParser.info in
          { t with MiscParser.info = info; }
        with Not_found -> t

      let dump source doc compiled =
        let outname = Tar.outname source in
        try
          Misc.output_protect
            (fun chan ->
              let module Out =
                Indent.Make(struct let hexa = O.hexa let out = chan end) in
              let dump =
                match OT.mode with
                | Mode.Std ->
                    let module S = Skel.Make(O)(Pseudo)(A')(T)(Out)(Lang) in
                    S.dump
                | Mode.PreSi|Mode.Kvm ->
                    let module O =
                      struct
                        include O
                        let is_kvm = match OT.mode with
                        | Mode.Kvm -> true
                        | Mode.PreSi -> false
                        | Mode.Std -> assert false
                        let is_tb = match OT.barrier with
                        | Barrier.TimeBase  -> true
                        | _ -> false
                      end  in
                    let module S = PreSi.Make(O)(Pseudo)(A')(T)(Out)(Lang) in
                    S.dump in
              dump doc compiled)
            outname
        with e ->
          begin try Sys.remove outname with _ -> () end ;
          raise e

      let limit_ok nprocs = match O.avail with
        | None|Some 0 -> true
        | Some navail -> not O.limit || nprocs <= navail

      let warn_limit name nprocs = match O.avail with
        | None|Some 0 -> ()
        | Some navail ->
           if nprocs > navail then
             Warn.warn_always
               "%stest with more threads (%i) than available (%i) is compiled"
               (Pos.str_pos0 name.Name.file) nprocs navail

      let compile
            parse count_procs compile allocate
            cycles hash_env
            name in_chan out_chan splitted =
        try begin
            check_variant Variant_litmus.Self splitted.Splitter.arch ;
            let parsed = parse in_chan splitted in
            let doc = splitted.Splitter.name in
            let tname = doc.Name.name in
            close_in in_chan ;
            let nprocs = count_procs parsed.MiscParser.prog in
            let hash =  H.mk_hash_info name parsed.MiscParser.info in
            let cycle_ok = cycle_ok avoid_cycle parsed
            and hash_ok = H.hash_ok hash_env tname hash
            and limit_ok = limit_ok nprocs in
            if
              cycle_ok && hash_ok && limit_ok
            then begin
                warn_limit doc nprocs ;
                let hash_env = StringMap.add tname hash hash_env in
                let parsed = change_hint hint doc.Name.name parsed in
                let allocated = allocate parsed in
                let compiled = compile doc allocated in
                let source = MyName.outname name ".c" in
                dump source doc compiled;
                if not OT.is_out then begin
                    let _utils =
                      let module OO = struct
                        include OT
                        let arch = A'.arch
                        let cached =
                          match threadstyle with
                          | ThreadStyle.Cached -> true
                          | _ -> false
                      end in
                      let module Obj = ObjUtil.Make(OO)(Tar) in
                      Obj.dump () in
                    ()
                  end ;
                R.run name out_chan doc allocated source ;
                Completed (A'.arch,doc,source,cycles,hash_env)
              end else begin
                let cause = if limit_ok then "" else " (too many threads)" in
                W.warn "%s test not compiled%s"
                  (Pos.str_pos0 doc.Name.file) cause ;
                Absent A'.arch
              end
          end with e -> Interrupted (A'.arch,e)
    end


  module Make
           (O:Config)
           (A:Arch_litmus.S)
           (L:GenParser.LexParse with type instruction = A.parsedPseudo)
           (XXXComp : XXXCompile_litmus.S with module A = A) =
    struct
      module Pseudo = LitmusUtils.Pseudo(A)
      module ALang = struct
        include A.I
        module RegSet = A.Out.RegSet
        module RegMap = A.Out.RegMap
      end
      module Lang = ASMLang.Make(O)(ALang)(A.Out)(A)
      module Utils = Utils(O)(A)(Lang)(Pseudo)
      module P = GenParser.Make(O)(A) (L)
      module Comp = Compile.Make (O)(A)(Utils.T)(XXXComp)

      module AllocArch = struct
        include A
        type v = A.V.v
        let maybevToV c =
          let open Constant in
          match c with
          | Tag _|Symbolic _|Label _|PteVal _ as sym -> sym
          | Concrete i -> Concrete (A.V.Scalar.of_string i)
        type global = Global_litmus.t
        let maybevToGlobal = A.tr_global
      end

      let compile =
        let allocate parsed =
          let module Alloc = SymbReg.Make(AllocArch) in
          Alloc.allocate_regs parsed in
        Utils.compile P.parse List.length Comp.compile allocate
    end


  module Make'
           (O:Config)
           (A:sig val comment : string end) =
    struct
      module L = struct
        type token = CParser.token
        module CL = CLexer.Make(struct let debug = false end)
        let lexer = CL.token false
        let parser = CParser.shallow_main
      end

      module A' = CArch_litmus.Make(O)

      module Pseudo =
        struct
          include DumpCAst
          let find_offset _ _ _ = Warn.user_error "No label value in C"
        end

      module Lang =
        CLang.Make
          (struct
            let comment = A.comment
            let memory = O.memory
            let mode = O.mode
            let asmcommentaslabel = O.asmcommentaslabel
          end)
          (struct
            let verbose = O.verbose
            let noinline = true
            let simple = false
            let out_ctx = Misc.identity
          end)
      module Utils = Utils(O)(A')(Lang)(Pseudo)
      module P = CGenParser_litmus.Make(O)(Pseudo)(A')(L)
      module Comp =
        CCompile_litmus.Make
          (struct include O let kernel = false let rcu = false end)(Utils.T)

      let compile =
        let allocate parsed =
          let module Alloc = CSymbReg.Make(A') in
          Alloc.allocate_regs parsed in
        Utils.compile P.parse A'.count_procs Comp.compile allocate
    end

  let debuglexer =  OT.verbose > 2

  module LexConfig =
    struct
      let debug = debuglexer
      let check_rename = OT.check_rename
    end


  module SP = Splitter.Make(LexConfig)

  let from_chan cycles hash_env name in_chan out_chan =
    (* First split the input file in sections *)
    let { Splitter.arch=arch ; _ } as splitted =
      SP.split name in_chan in
    let tname = splitted.Splitter.name.Name.name in
    if OT.check_name tname then begin
        (* Then call appropriate compiler, depending upon arch *)
        let opt = OT.mkopt (Option.get_default arch) in
        let word = Option.get_word opt in
        let module ODep = struct
            let word = word
            let line = Option.get_line opt
            let delay = Option.get_delay opt
            let gccopts = Option.get_gccopts opt
          end in
        (* Compile configuration, must also be used to configure arch modules *)
        let module OC = struct
          let verbose = OT.verbose
          let word = word
          let syncmacro =OT.syncmacro
          let syncconst = OT.syncconst
          let memory = OT.memory
          let morearch = OT.morearch
          let cautious = OT.cautious
          let asmcomment = OT.asmcomment
          let hexa = OT.hexa
          let mode = OT.mode
        end in
        let module OX = struct
            include OT
            include ODep
            let debuglexer = debuglexer
          let sysarch =
            match Archs.get_sysarch arch  OT.carch with
            | Some a -> a
            | None -> begin match arch with
              | `C ->
                  Warn.fatal "Test %s not performed because -carch is not given but required while using C arch" tname
              | _ ->
                Warn.fatal "no support for arch '%s'" (Archs.pp arch)
            end
          end in
        let module Cfg = OX in
        let aux = function
          | `PPC ->
             begin match OT.usearch with
             | UseArch.Trad ->
                let module V = Int64Constant in
                let module Arch' = PPCArch_litmus.Make(OC)(V) in
                let module LexParse = struct
                    type instruction = Arch'.parsedPseudo
                    type token = PPCParser.token
                    module Lexer = PPCLexer.Make(LexConfig)
                    let lexer = Lexer.token
                    let parser = MiscParser.mach2generic PPCParser.main
                  end in
                let module Compile = PPCCompile_litmus.Make(V)(OC) in
                let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
                X.compile
             | UseArch.Gen ->
                assert false
             (*
  let module Arch' = PPCGenArch.Make(OC)(V) in
  let module LexParse = struct
  type instruction = Arch'.pseudo
  type token = PPCGenParser.token
  module Lexer = PPCGenLexer.Make(LexConfig)
  let lexer = Lexer.token
  let parser = PPCGenParser.main
  end in
  let module Compile = PPCGenCompile.Make(V)(OC) in
  let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
  X.compile
              *)
             end
          | `X86 ->
             let module V = Int32Constant in
             let module Arch' = X86Arch_litmus.Make(OC)(V) in
             let module LexParse = struct
                 type instruction = Arch'.pseudo
                 type token = X86Parser.token
                 module Lexer = X86Lexer.Make(LexConfig)
                 let lexer = Lexer.token
                 let parser = MiscParser.mach2generic X86Parser.main
               end in
             let module Compile = X86Compile_litmus.Make(V)(OC) in
             let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
             X.compile
          | `X86_64 ->
             let module V = Int64Constant in
             let module Arch' = X86_64Arch_litmus.Make(OC)(V) in
             let module LexParse = struct
                 type instruction = Arch'.pseudo
                 type token = X86_64Parser.token
                 module Lexer = X86_64Lexer.Make(LexConfig)
                 let lexer = Lexer.token
                 let parser = MiscParser.mach2generic X86_64Parser.main
               end in
             let module Compile = X86_64Compile_litmus.Make(V)(OC) in
             let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
             X.compile
          | `ARM ->
             let module V = Int32Constant in
             let module Arch' = ARMArch_litmus.Make(OC)(V) in
             let module LexParse = struct
                 type instruction = Arch'.parsedPseudo
                 type token = ARMParser.token
                 module Lexer = ARMLexer.Make(LexConfig)
                 let lexer = Lexer.token
                 let parser = MiscParser.mach2generic ARMParser.main
               end in
             let module Compile = ARMCompile_litmus.Make(V)(OC) in
             let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
             X.compile
          | `AArch64 ->
             begin match OT.usearch with
             | UseArch.Trad ->
                let module V = Int64Constant in
                let module Arch' = AArch64Arch_litmus.Make(OC)(V) in
                let module LexParse = struct
                  type instruction = Arch'.parsedPseudo
                  type token = AArch64Parser.token
                  module Lexer = AArch64Lexer.Make(LexConfig)
                  let lexer = Lexer.token
                  let parser = (*MiscParser.mach2generic*) AArch64Parser.main
                end in
                let module Compile = AArch64Compile_litmus.Make(V)(OC) in
                let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
                X.compile
             | UseArch.Gen ->
                assert false
             end
          (*
  let module Arch' = AArch64GenArch.Make(OC)(V) in
  let module LexParse = struct
  type instruction = Arch'.pseudo
  type token = AArch64GenParser.token
  module Lexer = AArch64GenLexer.Make(LexConfig)
  let lexer = Lexer.token
  let parser = AArch64GenParser.main
  end in
  let module Compile = AArch64GenCompile.Make(V)(OC) in
  let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
  X.compile
           *)
          | `MIPS ->
             let module V = Int64Constant in
             let module Arch' = MIPSArch_litmus.Make(OC)(V) in
             let module LexParse = struct
                 type instruction = Arch'.pseudo
                 type token = MIPSParser.token
                 module Lexer = MIPSLexer.Make(LexConfig)
                 let lexer = Lexer.token
                 let parser = MiscParser.mach2generic MIPSParser.main
               end in
             let module Compile = MIPSCompile_litmus.Make(V)(OC) in
             let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
             X.compile
          | `RISCV ->
             let module V = Int64Constant in
             let module Arch' = RISCVArch_litmus.Make(OC)(V) in
             let module LexParse = struct
                 type instruction = Arch'.parsedPseudo
                 type token = RISCVParser.token
                 module Lexer = RISCVLexer.Make(LexConfig)
                 let lexer = Lexer.token
                 let parser = MiscParser.mach2generic RISCVParser.main
               end in
             let module Compile = RISCVCompile_litmus.Make(V)(OC) in
             let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
             X.compile
          | `C ->
             let module Arch' = struct
                 let comment =  match OT.asmcomment with
                   | Some c -> c
                   | None ->
                      begin match OX.sysarch with
                      | `PPC -> PPCArch_litmus.comment
                      | `X86 -> X86Arch_litmus.comment
                      | `X86_64 -> X86_64Arch_litmus.comment
                      | `ARM -> ARMArch_litmus.comment
                      | `AArch64 -> AArch64Arch_litmus.comment
                      | `MIPS -> MIPSArch_litmus.comment
                      | `RISCV -> RISCVArch_litmus.comment
                      end
               end in
             let module X = Make'(Cfg)(Arch') in
             X.compile
          | `CPP | `LISA -> assert false
        in
        aux arch cycles hash_env name in_chan out_chan splitted
      end else begin (* Excluded explicitely, (check_tname), do not warn *)
        Absent arch
      end

  let from_file cycles hash_env name out_chan =
    Misc.input_protect
      (fun in_chan -> from_chan cycles hash_env name in_chan out_chan)
      name

  (* Call generic tar builder/runner *)
  module DF = DumpRun.Make (OT)(Tar) (struct let from_file = from_file end)

  let from_files = DF.from_files
end
