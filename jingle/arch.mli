module type S = sig
    include ArchBase.S

    type substitution = reg * reg

    val match_instruction : substitution list -> 
			    instruction -> instruction ->
			    substitution list option

  end

module type Parser = sig
    include GenParser.S
    val instr_from_string : string -> pseudo list
  end

val get_arch : Archs.t -> (module S)

val get_parser : Archs.t -> (module Parser)
