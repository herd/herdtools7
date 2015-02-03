(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***************************************)
(* Apply a function (zyva) to one test *)
(***************************************)

open Archs

module Top
    (T:sig type t end) (* Return type, must be abstracted *)
    (B: functor(A:ArchBase.S)->
      (sig val zyva : Name.t -> A.pseudo MiscParser.t -> T.t end)) :
sig
  val from_file : string -> T.t
end = struct

  module Make
      (A:ArchBase.S) 
      (L:GenParser.LexParse with type instruction = A.pseudo) =
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
    | PPC ->
        let module PPC = PPCBase in
        let module PPCLexParse = struct
	  type instruction = PPC.pseudo
	  type token = PPCParser.token

          module L = PPCLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = PPCParser.main
        end in
        let module X = Make (PPC) (PPCLexParse) in
        X.zyva chan splitted
    | X86 ->
        let module X86 = X86Base in
        let module X86LexParse = struct
	  type instruction = X86.pseudo
	  type token = X86Parser.token

          module L = X86Lexer.Make(LexConf)
	  let lexer = L.token
	  let parser = X86Parser.main
        end in
        let module X = Make (X86) (X86LexParse) in
        X.zyva chan splitted
    | ARM ->
        let module ARM = ARMBase in
        let module ARMLexParse = struct
	  type instruction = ARM.pseudo
	  type token = ARMParser.token

          module L = ARMLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = ARMParser.main
        end in
        let module X = Make (ARM) (ARMLexParse) in
        X.zyva chan splitted
    | MIPS ->
        let module MIPS = MIPSBase in
        let module MIPSLexParse = struct
	  type instruction = MIPS.pseudo
	  type token = MIPSParser.token

          module L = MIPSLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = MIPSParser.main
        end in
        let module X = Make (MIPS) (MIPSLexParse) in
        X.zyva chan splitted
    | C -> Warn.fatal "No C arch in toolParse.ml"

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


      module Make
          (A:ArchBase.S) 
          (L:GenParser.LexParse with type instruction = A.pseudo) =
        struct
          module P = GenParser.Make(GenParser.DefaultConfig)(A)(L)
          module X = B(A)

          let justparse chan splitted =
            let parsed = P.parse chan splitted
            and doc = splitted.Splitter.name in
            doc,parsed

          let from_chan name chan =
            let { Splitter.arch=arch;_ } as splitted = SP.split name chan in
            if arch <> A.arch then
              Warn.fatal
                "Arch mismatch on %s (%s <-> %s)"
                name (Archs.pp A.arch)  (Archs.pp arch) ;
            justparse chan splitted

          let from_name name = Misc.input_protect (from_chan name) name

          let rec from_names ns = match ns with
          | [] -> []
          | n::ns ->
              let dt = from_name n in
              dt::from_names ns

          let zyva names =
            let dts = from_names names in
            X.zyva dts
        end

      let from_arch arch = 
        match arch with
        | PPC ->
            let module PPC = PPCBase in
            let module PPCLexParse = struct
	      type instruction = PPC.pseudo
	      type token = PPCParser.token

              module L = PPCLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = PPCParser.main
            end in
            let module X = Make (PPC) (PPCLexParse) in
            X.zyva 
        | X86 ->
            let module X86 = X86Base in
            let module X86LexParse = struct
	      type instruction = X86.pseudo
	      type token = X86Parser.token

              module L = X86Lexer.Make(LexConf)
	      let lexer = L.token
	      let parser = X86Parser.main
            end in
            let module X = Make (X86) (X86LexParse) in
            X.zyva
        | ARM ->
            let module ARM = ARMBase in
            let module ARMLexParse = struct
	      type instruction = ARM.pseudo
	      type token = ARMParser.token

              module L = ARMLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = ARMParser.main
            end in
            let module X = Make (ARM) (ARMLexParse) in
            X.zyva
        | MIPS ->
            let module MIPS = MIPSBase in
            let module MIPSLexParse = struct
	      type instruction = MIPS.pseudo
	      type token = MIPSParser.token

              module L = MIPSLexer.Make(LexConf)
	      let lexer = L.token
	      let parser = MIPSParser.main
            end in
            let module X = Make (MIPS) (MIPSLexParse) in
            X.zyva
        | C -> Warn.fatal "No C arch in toolParse.ml"

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
