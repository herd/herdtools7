module type Config = sig
    module Source : Arch.S
    module Target : Arch.S
    module SParser : Arch.Parser
    module TParser : Arch.Parser
    val conversions : (string * string) list 
  end

module Make : functor (C:Config)->
   sig
     module Source : Arch.S
     module Target : Arch.S
		       
     val translate : 
       Source.nice_prog -> Target.nice_prog
   end
