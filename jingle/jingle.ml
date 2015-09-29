(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf

let verbose = ref false
let map = ref None
let outdir = ref None
let args = ref []
let prog =
  if Array.length Sys.argv > 0 
  then Sys.argv.(0)
  else "jingle"

exception Error of string

let get_arch =
  let open Arch in
  function
  | `AArch64 ->
     let module AArch64LexParse = struct
       type instruction = AArch64Arch.parsedPseudo
       type token = AArch64Parser.token
       module Lexer = AArch64Lexer.Make(struct let debug = false end)
       let lexer = Lexer.token
       let parser = MiscParser.mach2generic AArch64Parser.main
       let instr_parser = AArch64Parser.instr_option_seq
     end in (module struct
	       include AArch64Arch
	       module Parser = MakeParser(AArch64Arch)(AArch64LexParse)
	       module Dumper = DefaultDumper(AArch64Arch)
	     end : Arch.S)
   | `ARM ->
      let module ARMLexParse = struct
	type instruction = ARMArch.parsedPseudo
	type token = ARMParser.token
        module Lexer = ARMLexer.Make(struct let debug = false end)
	let lexer = Lexer.token
	let parser = MiscParser.mach2generic ARMParser.main
       let instr_parser = ARMParser.instr_option_seq
      end in (module struct
		include ARMArch
		module Parser = MakeParser(ARMArch)(ARMLexParse)
		module Dumper = DefaultDumper(ARMArch)
	      end : S)
  | `Bell ->
     let module BellLexParse = struct
       type instruction = BellArch.parsedPseudo
       type token = LISAParser.token
       module Lexer = BellLexer.Make(struct let debug = false end)
       let lexer = Lexer.token
       let parser = LISAParser.main
       let instr_parser = LISAParser.instr_option_seq
     end in (module struct 
	       include BellArch
	       module Parser = MakeParser(BellArch)(BellLexParse)
	       module Dumper = DefaultDumper(BellArch)
	     end : S)
  | `C ->
     let module CLexParse = struct
       type pseudo = CArch.parsedPseudo
       type token = CParser.token
       module Lexer = CLexer.Make(struct let debug = false end)
       let shallow_lexer = Lexer.token false
       let deep_lexer = Lexer.token true
       let shallow_parser = CParser.shallow_main
       let deep_parser = CParser.deep_main
       let instr_parser = CParser.pseudo_seq
     end in (module struct 
	       include CArch
	       module Parser = struct
		 include CGenParser.Make(CGenParser.DefaultConfig)
					(CArch)(CLexParse)
		 type parsedPseudo = CArch.parsedPseudo
		 let instr_from_string s =
		   CGenParser.call_parser "themes" (Lexing.from_string s) 
					 CLexParse.deep_lexer 
					 CLexParse.instr_parser
	       end
	       module Dumper = CDumper
	     end : S)
	      
  | `PPC ->
     let module PPCLexParse = struct
       type instruction = PPCArch.parsedPseudo
       type token = PPCParser.token
       module Lexer = PPCLexer.Make(struct let debug = false end)
       let lexer = Lexer.token
       let parser = MiscParser.mach2generic PPCParser.main
       let instr_parser = PPCParser.instr_option_seq
     end in (module struct 
	       include PPCArch
	       module Parser = MakeParser(PPCArch)(PPCLexParse)
	       module Dumper = DefaultDumper(PPCArch)
	     end : S)

    
let () = Arg.parse
           ["-v",Arg.Unit (fun () -> verbose := true),
	    "- be verbose";
	    "-theme",Arg.String (fun s -> map := Some s),
	    "<name> - give the theme file <name>";
	    "-o",Arg.String (fun s -> outdir := Some s),
	    "<name> - directory for output files"]
	   (fun s -> args := s :: !args)
	   (sprintf "Usage: %s [option]* -theme <file> [test]*" prog)

let parsed = match !map with
  | None -> raise (Error "No map file provided.")
  | Some s -> Misc.input_protect ParseMap.parse s

let () = if !verbose then
	   (eprintf "Reading theme file :\n";
	    List.iter (fun (s,t) ->
		       eprintf "\"%s\" -> \"%s\"\n" s t)
		      parsed.ParseMap.conversions)
	     
module Source = (val get_arch parsed.ParseMap.source)
module Target = (val get_arch parsed.ParseMap.target)

module Trad = Mapping.Make(struct
			    module Source = Source
			    module Target = Target
			    let conversions = parsed.ParseMap.conversions
			  end)
			    
let do_trans file = 
  let fin chin =
    let sres = let module SP = Splitter.Make(Splitter.Default) in
	       SP.split (Filename.basename file) chin in
    let fout out = try
	let trans_test = Trad.translate chin sres in
	Target.Dumper.dump_info out sres.Splitter.name trans_test
      with e -> eprintf "Error in test %s.\n" (Filename.basename file);
		raise e
    in match !outdir with
       | None -> fout stdout
       | Some s -> 
	  Misc.output_protect fout (Filename.concat s 
					     (Filename.basename file))
  in Misc.input_protect fin file

let () = 
  let open Misc in
  match !args with
  | [] -> iter_stdin do_trans
  | tests -> iter_argv do_trans tests

