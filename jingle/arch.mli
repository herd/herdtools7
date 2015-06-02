module type S = sig
    include ArchBase.S
  end

module type Parser = sig
    include GenParser.S
    val instr_from_string : string -> pseudo list
  end

val get_arch : Archs.t -> (module S)

val get_parser : Archs.t -> (module Parser)
