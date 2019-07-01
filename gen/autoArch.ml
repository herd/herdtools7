(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module type S = sig
  module A : Arch_gen.S
  module E : Edge.S  with type fence = A.fence
  module R : Relax.S with type edge = E.edge and type fence = A.fence
  module L : LogRelax.S with type relax = R.relax
end

module Make(A:Arch_gen.S) : S
= struct
  module A = A
  module E = Edge.Make(Edge.Config)(A)
  module R = Relax.Make(A) (E)

  module LogInput = struct
    type relax = R.relax
    let parse = R.parse_relax
  end


  module  L = LogRelax.Make(LogInput)
          
end
