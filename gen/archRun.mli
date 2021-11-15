(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Argument for run module *)
module type S = sig
  module A : ArchLoc.S
  module E : Edge.S
  with type fence = A.fence
  and type dp = A.dp
  and type atom = A.atom
  and type rmw = A.rmw
  module R : Relax.S
  with type fence = A.fence
  and type dp = A.dp
  and type edge = E.edge
  module C : Cycle.S
         with type edge=E.edge
          and type atom = A.atom
          and module PteVal = A.PteVal
end
