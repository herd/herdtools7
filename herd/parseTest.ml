(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*******************************)
(* Run a test from source file *)
(*******************************)
module type Config = sig
  val model : Model.t option
  val through : Model.through
  val skipchecks : StringSet.t
  val strictskip : bool
  val check_name : string -> bool
  val check_rename : string -> string option
  include GenParser.Config
  include Top.Config
  include Sem.Config
end

module Top (C:Config) = struct
  module Make
      (S:Sem.Semantics)
      (P:sig 
         type pseudo
         val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
       end with type pseudo = S.A.pseudo)
      (M:XXXMem.S with module S = S) =
    struct
      module T = Test.Make(S.A) 

      let run filename chan env splitted =
        try
          let parsed = P.parse chan splitted in
          let name = splitted.Splitter.name in
          let hash = MiscParser.get_hash parsed in
          let env = match hash with
          | None -> env
          | Some hash ->
              TestHash.check_env env name.Name.name filename hash in
          let test = T.build name parsed in
          let module T = Top.Make(C)(M) in
          T.run test ;
          env
          with TestHash.Seen -> env
    end

  module SP =
    Splitter.Make
      (struct
        let debug = C.debug.Debug.lexer
        let check_rename = C.check_rename
      end)

  let do_from_file env name chan =
(* First split the input file in sections *)
    let (splitted:Splitter.result) =  SP.split name chan in
    let tname = splitted.Splitter.name.Name.name in
    if C.check_name tname then begin
      let arch = splitted.Splitter.arch in
(* Now, we have the architecture, call specific parsers
   generically. *)
      let module LexConfig = struct
        let debug = C.debug.Debug.lexer
      end in
      let module ModelConfig = struct
        let model =
          match C.model with None -> Model.get_default_model arch
          | Some m -> m
        let through = C.through
        let debug = C.debug.Debug.barrier
        let verbose = C.verbose
        let skipchecks = C.skipchecks
        let strictskip = C.strictskip
        let optace = C.optace
      end in
      match arch with
      | `PPC ->
	  let module PPC = PPCArch.Make(C.PC)(SymbValue) in
	  let module PPCLexParse = struct
	    type instruction = PPC.pseudo
	    type token = PPCParser.token
            module Lexer = PPCLexer.Make(LexConfig)
	    let lexer = Lexer.token
	    let parser = PPCParser.main
	  end in
          let module PPCS = PPCSem.Make(C)(SymbValue) in
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
          let module P = GenParser.Make (C) (PPC) (PPCLexParse) in
          let module X = Make (PPCS) (P) (PPCM) in 
          X.run name chan env splitted

      | `ARM ->
	  let module ARM = ARMArch.Make(C.PC)(SymbValue) in
	  let module ARMLexParse = struct
	    type instruction = ARM.pseudo
	    type token = ARMParser.token
            module Lexer = ARMLexer.Make(LexConfig)
	    let lexer = Lexer.token
	    let parser = ARMParser.main
	  end in
          let module ARMS = ARMSem.Make(C)(SymbValue) in
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
          let module ARMM = ARMMem.Make(ModelConfig)(ARMS) (ARMBarrier) in
          let module P = GenParser.Make (C) (ARM) (ARMLexParse) in
          let module X = Make (ARMS) (P) (ARMM) in 
          X.run name chan env splitted

      | `X86 ->
          let module X86 = X86Arch.Make(C.PC)(SymbValue) in
          let module X86LexParse = struct
	    type instruction = X86.pseudo
	    type token = X86Parser.token
            module Lexer = X86Lexer.Make(LexConfig)
	    let lexer = Lexer.token
	    let parser = X86Parser.main
	  end in
          let module X86S = X86Sem.Make(C)(SymbValue) in
          let module X86Barrier = struct
            type a = X86.barrier
            type b = MFENCE|LFENCE|SFENCE
            let a_to_b a = match a with
            | X86.Mfence -> MFENCE
            | X86.Sfence -> SFENCE
            | X86.Lfence -> LFENCE
          end in
          let module X86M = X86Mem.Make(ModelConfig)(X86S) (X86Barrier) in
          let module P = GenParser.Make (C) (X86) (X86LexParse) in
          let module X = Make (X86S) (P) (X86M) in 
          X.run name chan env splitted

      | `MIPS ->
          let module MIPS = MIPSArch.Make(C.PC)(SymbValue) in
          let module MIPSLexParse = struct
	    type instruction = MIPS.pseudo
	    type token = MIPSParser.token
            module Lexer = MIPSLexer.Make(LexConfig)
	    let lexer = Lexer.token
	    let parser = MIPSParser.main
	  end in
          let module MIPSS = MIPSSem.Make(C)(SymbValue) in
          let module MIPSBarrier = struct
            type a = MIPS.barrier
            type b = SYNC
            let a_to_b a = match a with
            | MIPS.Sync -> SYNC
          end in
          let module MIPSM = MIPSMem.Make(ModelConfig)(MIPSS)(MIPSBarrier) in
          let module P = GenParser.Make (C) (MIPS) (MIPSLexParse) in
          let module X = Make (MIPSS) (P) (MIPSM) in
          X.run name chan env splitted

      | `C ->
        let module CPP11 = CPP11Arch.Make(C.PC)(SymbValue) in
        let module CPP11LexParse = struct
    	  type pseudo = CPP11.pseudo
	  type token = CParser.token
          module Lexer = CLexer.Make(LexConfig)
	  let shallow_lexer = Lexer.token false
	  let deep_lexer = Lexer.token true
	  let shallow_parser = CParser.shallow_main
	  let deep_parser = CParser.deep_main
        end in
        let module CPP11S = CPP11Sem.Make(C)(SymbValue) in
        let module  CPP11Barrier = struct
          type a = CPP11.barrier
          type b = unit
          let a_to_b _ = ()
        end in
        let module CPP11M = CPP11Mem.Make(ModelConfig)(CPP11S) (CPP11Barrier) in
        let module P = CGenParser.Make (C) (CPP11) (CPP11LexParse) in
        let module X = Make (CPP11S) (P) (CPP11M) in
        X.run name chan env splitted

      | `OpenCL ->
        let module OpenCL = OpenCLArch.Make(C.PC)(SymbValue) in
        let module OpenCLLexParse = struct
  	  type pseudo = OpenCL.pseudo
	  type token = OpenCLParser.token
          module Lexer = OpenCLLexer.Make(LexConfig)
	  let shallow_lexer = Lexer.token false
	  let deep_lexer = Lexer.token true
	  let shallow_parser = OpenCLParser.shallow_main
	  let deep_parser = OpenCLParser.deep_main
        end in
        let module OpenCLS = OpenCLSem.Make(C)(SymbValue) in
        let module OpenCLBarrier = struct
          type a = OpenCL.barrier
          type b = unit
          let a_to_b _ = ()	    
        end in
        let module OpenCLM = OpenCLMem.Make(ModelConfig)(OpenCLS) (OpenCLBarrier) in
        let module P = CGenParser.Make (C) (OpenCL) (OpenCLLexParse) in
        let module X = Make (OpenCLS) (P) (OpenCLM) in 
        X.run name chan env splitted

      | `GPU_PTX ->
        let module GPU_PTX = GPU_PTXArch.Make(C.PC)(SymbValue) in
        let module GPU_PTXLexParse = struct
  	  type instruction = GPU_PTX.pseudo
	  type token = GPU_PTXParser.token
          module Lexer = GPU_PTXLexer.Make(LexConfig)
	  let lexer = Lexer.token
	  let parser = GPU_PTXParser.main
        end in
        let module GPU_PTXS = GPU_PTXSem.Make(C)(SymbValue) in
        let module GPU_PTXBarrier = struct
          type a = GPU_PTX.barrier
          type b = Membar of GPU_PTX.bar_scope
          let a_to_b a = match a with
            | GPU_PTX.Membar (scope) -> Membar (scope)
	    
        end in
        let module GPU_PTXM = GPU_PTXMem.Make(ModelConfig)(GPU_PTXS) (GPU_PTXBarrier) in
        let module P = GenParser.Make (C) (GPU_PTX) (GPU_PTXLexParse) in
        let module X = Make (GPU_PTXS) (P) (GPU_PTXM) in 
        X.run name chan env splitted

    end else env
(* Forgive fatal errors *)
  let from_file  name env =
    Misc.input_protect (do_from_file env name) name
end
