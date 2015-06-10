module type Config = sig
    module Source : Arch.S
    module Target : Arch.S
    val conversions : (string * string) list 
  end

module Make : functor (C:Config)->
   sig
     module Source : Arch.S
     module Target : Arch.S
     module Env : sig
       type t
       val init : t
       val get_concrete_register : t -> Source.reg -> (Target.reg * t)
     end
		       
     val translate : 
       Source.nice_prog -> Target.nice_prog
   end
