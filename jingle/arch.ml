module type S = sig
    include ArchBase.S
    
    type substitution = reg * reg

    val match_instruction : substitution list -> 
			    instruction -> instruction ->
			    substitution list option

  end

module type Parser = sig
  include GenParser.S
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
end

module Make
	 (A:S)
	 (P:sig
	      include GenParser.LexParse with type instruction = A.parsedPseudo
	      val instr_parser : 
		(Lexing.lexbuf -> token) -> Lexing.lexbuf ->
		A.parsedPseudo list
	    end) = struct
  include GenParser.Make(GenParser.DefaultConfig)(A)(P)

  type 	parsedPseudo = A.parsedPseudo
  let instr_from_string s =
    GenParser.call_parser "themes" (Lexing.from_string s) 
			  P.lexer P.instr_parser
		
end

    
let get_arch = function
    | `AArch64 -> (module AArch64Arch : S)
    | `Bell -> (module BellArch : S)


let get_parser = function
  | `AArch64 -> 
     let module AArch64LexParse = struct
       type instruction = AArch64Arch.parsedPseudo
       type token = AArch64Parser.token
       module Lexer = AArch64Lexer.Make(struct let debug = false end)
       let lexer = Lexer.token
       let parser = MiscParser.mach2generic AArch64Parser.main
       let instr_parser = AArch64Parser.instr_option_list
     end in (module Make(AArch64Arch)(AArch64LexParse) : Parser)
 | `Bell ->
    let module BellLexParse = struct
      type instruction = BellArch.parsedPseudo
      type token = LISAParser.token
      module Lexer = BellLexer.Make(struct let debug = false end)
      let lexer = Lexer.token
      let parser = LISAParser.main
      let instr_parser = LISAParser.instr_option_list
    end in (module Make(BellArch)(BellLexParse) : Parser)
