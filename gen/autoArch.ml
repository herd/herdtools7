module type S = sig
  module A : Arch.S
  module E : Edge.S  with type fence = A.fence
  module R : Relax.S with type edge = E.edge and type fence = A.fence
  module L : LogRelax.S with type relax = R.relax
end

module Make(A:Arch.S) : S
= struct
  module A = A
  module E = Edge.Make(A)
  module R = Relax.Make(A) (E)

  module LogInput = struct
    type relax = R.relax
    let parse = R.parse_relax
  end


  module  L = LogRelax.Make(LogInput)
          
end
