module Make :
  functor (_ : sig val debug : bool end) ->
    sig
      val check : string -> string list
    end
