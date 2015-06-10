module type Parser = sig
  include GenParser.S
	    
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
  end

module type S = sig
    include ArchBase.S
    
    type mcst

    type substitution = 
      | Reg of string * reg
      | Cst of string * int

    val match_instruction : substitution list -> 
			    parsedInstruction -> parsedInstruction ->
			    substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   parsedPseudo list
   
    module Parser : Parser with type parsedPseudo = parsedPseudo

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
