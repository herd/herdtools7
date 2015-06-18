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

    val match_instruction : substitution list -> 
			    parsedPseudo -> pseudo ->
			    substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   parsedPseudo list
   					 
    module Parser : Parser with type parsedPseudo = parsedPseudo
			    and type pseudo = pseudo

  end

module MakeParser : 
functor(A:ArchBase.S)
	 (P:sig
	      include GenParser.LexParse 
		      with type instruction = A.parsedPseudo
	      val instr_parser : 
	     (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	     A.parsedPseudo list
	    end)
       -> Parser
	    
val get_arch : Archs.t -> (module S)
