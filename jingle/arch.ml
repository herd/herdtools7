module type Parser = sig
  include GenParser.S
	    
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
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
			    string option * substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   parsedPseudo list
   
    module Parser : Parser with type parsedPseudo = parsedPseudo
			    and type pseudo = pseudo

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

    
let get_arch = function
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
	     end : S)
	      
