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

module type Parser = sig
  include GenParser.S
	    
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
  end

module type Dumper = sig
    type pseudo
    val dump_info : 
      out_channel -> Name.t ->
      (MiscParser.state, (int * pseudo list) list,
       MiscParser.constr, MiscParser.location)
        MiscParser.result
      -> unit
end

module type S = sig
    include ArchBase.S

    type substitution = 
      | Reg of string * reg
      | Cst of string * int
      | Lab of string * string
      | Addr of string * string
      | Code of string * pseudo list


    val dump_pseudos : pseudo list -> string

    val match_instruction : substitution list -> 
			    parsedPseudo -> pseudo ->
			    substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   pseudo list
   
    module Parser : Parser with type parsedPseudo = parsedPseudo
			    and type pseudo = pseudo

    module Dumper : Dumper with type pseudo = pseudo
  end

module MakeParser
	 (A:ArchBase.S)
	 (P:sig
	      include GenParser.LexParse 
		      with type instruction = A.parsedPseudo
	      val instr_parser : 
		(Lexing.lexbuf -> token) -> Lexing.lexbuf ->
		A.parsedPseudo list
	    end) = struct
  include GenParser.Make(GenParser.DefaultConfig)(A)(P)
			       
  type parsedPseudo = A.parsedPseudo
  let instr_from_string s =
    GenParser.call_parser "themes" (Lexing.from_string s) 
			  P.lexer P.instr_parser
		
end

module DefaultDumper(A:ArchBase.S) = struct 
  type pseudo = A.pseudo
  include SimpleDumper.Make(struct
	    module A = A

            let dump_loc = MiscParser.dump_location

            let dump_state_atom a =
              MiscParser.dump_state_atom dump_loc SymbConstant.pp_v a

            type state = MiscParser.state

            let dump_state st =
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom a))
                   st)

                
            type constr = MiscParser.constr
            let dump_atom a =
              let open ConstrGen in
              match a with
              | LV (loc,v) -> dump_state_atom (loc,(MiscParser.TyDef,v))
              | LL (loc1,loc2) ->
                  sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)
end

    
let get_arch = function
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
		module Parser = MakeParser(ARMBase)(ARMLexParse)
		module Dumper = DefaultDumper(ARMBase)
	      end : S)
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
	       module Parser = MakeParser(AArch64Base)(AArch64LexParse)
	       module Dumper = DefaultDumper(AArch64Base)
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
	       module Parser = MakeParser(BellBase)(BellLexParse)
	       module Dumper = DefaultDumper(BellBase)
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
	       module Parser = MakeParser(PPCBase)(PPCLexParse)
	       module Dumper = DefaultDumper(PPCBase)
	     end : S)
