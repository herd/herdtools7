open Printf 

exception Error of string

module type Config = sig
    module Source : Arch.S
    module Target : Arch.S
    module SParser : Arch.Parser
    module TParser : Arch.Parser
    val conversions : (string * string) list 
  end

module Make(C:Config) = struct
	   
  module Source = C.Source
  module Target = C.Target

  let conversions = 
    List.map
      (fun (s,t) ->
       let s = 
	 try C.SParser.instr_from_string s with
	 | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" s;
		raise e in 
       let t = 
	 try C.TParser.instr_from_string t with
	 | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" t;
		raise e in
       (s,t)
      )
      C.conversions

  let translate src = failwith "meh"

end
