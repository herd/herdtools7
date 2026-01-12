type test = AArch64Base.pseudo MiscParser.t

val prog : test -> (MiscParser.proc * AArch64Base.instruction list) list

val run_herd :
  libdir:string option -> herd_path:string option -> string -> Execution.t list

module MakeArch (C : sig
  val is_morello : bool
end) : sig
  type instruction = AArch64Base.instruction

  val parse_from_file : string -> test
  val show_instruction : latex:bool -> instruction -> string
end
