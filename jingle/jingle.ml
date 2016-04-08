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

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> verbose := true),
     "- be verbose";
     "-theme",Arg.String (fun s -> map := Some s),
     "<name> - give the theme file <name>";
     "-o",Arg.String (fun s -> outdir := Some s),
     "<name> - directory for output files"]
    (fun s -> args := s :: !args)
    (sprintf "Usage: %s [option]* -theme <file> [test]*" prog)


let map = !map
let verbose = !verbose
let outdir = !outdir
let args = !args

let parsed = match map with
| None -> raise (Error "No map file provided.")
| Some s -> Misc.input_protect ParseMap.parse s

let () =
  if verbose then begin
   eprintf "Reading theme file :\n";
    List.iter (fun (s,t) ->
      eprintf "\"%s\" -> \"%s\"\n" s t)
      parsed.ParseMap.conversions
  end

module Source = (val get_arch parsed.ParseMap.source)
module Target = (val get_arch parsed.ParseMap.target)

module Trad =
  Mapping.Make
    (struct
      let verbose = verbose
      module Source = Source
      module Target = Target
      let conversions = parsed.ParseMap.conversions
    end)


module Top(Out:OutTests.S) = struct			    

  let idx_out = Out.open_all ()

  let do_trans file k = 

    let fin chin =
      let sres =
        let module SP = Splitter.Make(Splitter.Default) in
        SP.split (Filename.basename file) chin in
      let tgt_test =
        try Trad.translate chin sres
        with  Mapping.Error msg -> Warn.fatal "File \"%s\":%s" file msg in
        
      let dump out =
        let out = Out.chan out in
        Target.Dumper.dump_info out sres.Splitter.name tgt_test in
      

      let base = Filename.basename file in
      let out = Out.open_file base in
      Misc.output_protect_close Out.close dump out ;
      Out.fprintf idx_out "%s\n" base in
      
        

    try Misc.input_protect fin file ; k+1
    with
    | Misc.Exit -> k
    | Misc.Fatal msg ->
        if verbose then eprintf "%s\n" msg ;
        k


  let zyva () = 
    let nout =
      match args with
      | [] -> Misc.fold_stdin do_trans 0
      | tests -> Misc.fold_argv do_trans tests 0 in
    Out.tar() ;
    eprintf "Generated %i tests\n" nout
end

let () = match outdir with
| None ->
    let module X = Top(OutStd) in
    X.zyva()
| Some _ ->
    let module Out =
      OutTar.Make
        (struct
          let verbose = 0
          let outname = outdir
        end) in
    let module X = Top(Out) in
    X.zyva ()

