(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***************************************)
(* Apply a function (zyva) to one test *)
(***************************************)

module Top
    (T:sig type t end) (* Return type, must be abstracted *)
    (B: functor(A:ArchBase.S)->
      (sig val zyva : Name.t -> A.pseudo MiscParser.t -> T.t end)) :
sig
  val from_file : string -> T.t
end = struct

  module Make
      (A:ArchBase.S) 
      (L:GenParser.LexParse with type instruction = A.parsedPseudo) =
    struct
      module P = GenParser.Make(GenParser.DefaultConfig)(A)(L)
      module X = B(A)


      let zyva chan splitted =
        let name = splitted.Splitter.name in
        let parsed = P.parse chan splitted in
        X.zyva name parsed
    end

  module LexConf = Splitter.Default

  let from_chan chan splitted = 
    match splitted.Splitter.arch with
    | `PPC ->
        let module PPC = PPCBase in
        let module PPCLexParse = struct
	  type instruction = PPC.parsedPseudo
	  type token = PPCParser.token

          module L = PPCLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic PPCParser.main
        end in
        let module X = Make (PPC) (PPCLexParse) in
        X.zyva chan splitted
    | `X86 ->
        let module X86 = X86Base in
        let module X86LexParse = struct
	  type instruction = X86.parsedPseudo
	  type token = X86Parser.token

          module L = X86Lexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic X86Parser.main
        end in
        let module X = Make (X86) (X86LexParse) in
        X.zyva chan splitted
    | `X86_64 ->
        let module X86_64 = X86_64Base in
        let module X86_64LexParse = struct
	  type instruction = X86_64.parsedPseudo
	  type token = X86_64Parser.token

          module L = X86_64Lexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic X86_64Parser.main
        end in
        let module X = Make (X86_64) (X86_64LexParse) in
        X.zyva chan splitted
    | `ARM ->
        let module ARM = ARMBase in
        let module ARMLexParse = struct
	  type instruction = ARM.parsedPseudo
	  type token = ARMParser.token

          module L = ARMLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic ARMParser.main
        end in
        let module X = Make (ARM) (ARMLexParse) in
        X.zyva chan splitted
    | `AArch64 ->
        let module AArch64 = AArch64Base in
        let module AArch64LexParse = struct
	  type instruction = AArch64.parsedPseudo
	  type token = AArch64Parser.token

          module L = AArch64Lexer.Make(LexConf)
	  let lexer = L.token
	  let parser = (*MiscParser.mach2generic*) AArch64Parser.main
        end in
        let module X = Make (AArch64) (AArch64LexParse) in
        X.zyva chan splitted
    | `MIPS ->
        let module MIPS = MIPSBase in
        let module MIPSLexParse = struct
	  type instruction = MIPS.parsedPseudo
	  type token = MIPSParser.token

          module L = MIPSLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic MIPSParser.main
        end in
        let module X = Make (MIPS) (MIPSLexParse) in
        X.zyva chan splitted
    | `RISCV ->
        let module RISCV = RISCVBase in
        let module RISCVLexParse = struct
	  type instruction = RISCV.parsedPseudo
	  type token = RISCVParser.token

          module L = RISCVLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MiscParser.mach2generic RISCVParser.main
        end in
        let module X = Make (RISCV) (RISCVLexParse) in
        X.zyva chan splitted
    | `LISA ->
        let module Bell = BellBase in
        let module BellLexParse = struct
	  type instruction = Bell.parsedPseudo
	  type token = LISAParser.token

          module L = BellLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = LISAParser.main
        end in
        let module X = Make (Bell) (BellLexParse) in
        X.zyva chan splitted
    | `CPP as a -> Warn.fatal "no support for arch '%s'" (Archs.pp a)
    | `C ->
        let module C = CBase in
        let module L = struct
          type pseudo = C.pseudo
	  type token = CParser.token
          module Lexer = CLexer.Make(LexConf)
	  let shallow_lexer = Lexer.token false
	  let deep_lexer = Lexer.token true
	  let shallow_parser = CParser.shallow_main
	  let deep_parser = CParser.deep_main
(* No macros *)
          type macro = unit
          let macros_parser _ _ = assert false
          let macros_expand _ i = i
        end in
        let module P = CGenParser_lib.Make(CGenParser_lib.DefaultConfig)(C)(L) in
        let module X = B(C) in
        let name =  splitted.Splitter.name in
        let parsed = P.parse chan splitted in
        X.zyva name parsed

  module SP = Splitter.Make(LexConf)


  let from_file name =
    Misc.input_protect
      (fun chan ->
        let (splitted:Splitter.result) = SP.split name chan in
        from_chan chan splitted) name

end

module Tops
    (T:sig type t end) (* Return type, must be abstracted *)
    (B: functor(A:ArchBase.S)->
      (sig val zyva : (Name.t * A.pseudo MiscParser.t) list -> T.t end)) :
    sig
      val from_files : string list -> T.t
    end = struct

      module LexConf = Splitter.Default
      module SP = Splitter.Make(LexConf)

(* Code shared between mach argch's and C *)
      module Util
          (Arg:sig
            val arch : Archs.t
            type parsed
            val parse : in_channel -> Splitter.result -> parsed
            val zyva : (Name.t * parsed) list -> T.t
          end) =
        struct

          let justparse chan sp =
            let parsed = Arg.parse chan sp
            and doc = sp.Splitter.name in
            doc,parsed


          let from_chan name chan =
            let { Splitter.arch=arch;_ } as splitted = SP.split name chan in
            if arch <> Arg.arch then
              Warn.fatal
                "Arch mismatch on %s (%s <-> %s)"
                name (Archs.pp Arg.arch)  (Archs.pp arch) ;
            justparse chan splitted

          let from_name name = Misc.input_protect (from_chan name) name

          let rec from_names ns = match ns with
          | [] -> []
          | n::ns ->
              let dt = from_name n in
              dt::from_names ns

          let zyva names =
            let dts = from_names names in
            Arg.zyva dts
        end

(* Code shared for machine arch's *)
      module Make
          (A:ArchBase.S) 
          (L:GenParser.LexParse with type instruction = A.parsedPseudo) =
        struct
          module P = GenParser.Make(GenParser.DefaultConfig)(A)(L)
          module X = B(A)

          include
            Util
              (struct
                let arch = A.arch
                type parsed = P.pseudo MiscParser.t
                let parse = P.parse
                let zyva = X.zyva
              end)
        end

      let from_arch arch = 
        match arch with
        | `PPC ->
            let module PPC = PPCBase in
            let module PPCLexParse = struct
	      type instruction = PPC.parsedPseudo
	      type token = PPCParser.token

              module L = PPCLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MiscParser.mach2generic PPCParser.main
            end in
            let module X = Make (PPC) (PPCLexParse) in
            X.zyva 
        | `X86 ->
            let module X86 = X86Base in
            let module X86LexParse = struct
	      type instruction = X86.parsedPseudo
	      type token = X86Parser.token

              module L = X86Lexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MiscParser.mach2generic X86Parser.main
            end in
            let module X = Make (X86) (X86LexParse) in
            X.zyva
        | `X86_64 ->
            let module X86_64 = X86_64Base in
            let module X86_64LexParse = struct
	      type instruction = X86_64.parsedPseudo
	      type token = X86_64Parser.token

              module L = X86_64Lexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MiscParser.mach2generic X86_64Parser.main
            end in
            let module X = Make (X86_64) (X86_64LexParse) in
            X.zyva
        | `ARM ->
            let module ARM = ARMBase in
            let module ARMLexParse = struct
	      type instruction = ARM.parsedPseudo
	      type token = ARMParser.token

              module L = ARMLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MiscParser.mach2generic ARMParser.main
            end in
            let module X = Make (ARM) (ARMLexParse) in
            X.zyva
        | `AArch64 ->
            let module AArch64 = AArch64Base in
            let module AArch64LexParse = struct
	      type instruction = AArch64.parsedPseudo
	      type token = AArch64Parser.token

              module L = AArch64Lexer.Make(LexConf)
	      let lexer = L.token
	      let parser =  (*MiscParser.mach2generic*) AArch64Parser.main
            end in
            let module X = Make (AArch64) (AArch64LexParse) in
            X.zyva
        | `MIPS ->
            let module MIPS = MIPSBase in
            let module MIPSLexParse = struct
	      type instruction = MIPS.parsedPseudo
	      type token = MIPSParser.token

              module L = MIPSLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MiscParser.mach2generic MIPSParser.main
            end in
            let module X = Make (MIPS) (MIPSLexParse) in
            X.zyva
        | `LISA ->
            let module Bell = BellBase in
            let module BellLexParse = struct
	      type instruction = Bell.parsedPseudo
	      type token = LISAParser.token
              module L = BellLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = LISAParser.main
            end in
            let module X = Make (Bell) (BellLexParse) in
            X.zyva
        | `CPP as a -> Warn.fatal "no support for arch '%s'" (Archs.pp a)
        | `C ->
            let module C = CBase in
            let module L = struct
              type pseudo = C.pseudo
	      type token = CParser.token
              module Lexer = CLexer.Make(LexConf)
	      let shallow_lexer = Lexer.token false
	      let deep_lexer = Lexer.token true
	      let shallow_parser = CParser.shallow_main
	      let deep_parser = CParser.deep_main
(* No macros *)
              type macro = unit
              let macros_parser _ _ = assert false
              let macros_expand _ i = i
            end in

            let module P = CGenParser_lib.Make(CGenParser_lib.DefaultConfig)(C)(L) in
            let module X = B(C) in

            let module U =
              Util
                (struct
                  let arch = `C
                  type parsed =  P.pseudo MiscParser.t
                  let parse = P.parse
                  let zyva = X.zyva
                end) in
            U.zyva

        | _ -> assert false



      let from_files names = match names with
      | [] -> assert false
      | n::_ ->
          let arch =
            Misc.input_protect
              (fun chan ->
                let (splitted:Splitter.result) =  SP.split n chan in
                splitted.Splitter.arch) n in
          from_arch arch names

    end

