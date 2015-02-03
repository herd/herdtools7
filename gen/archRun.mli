(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Argument for run module *)
module type S = sig
  module A : ArchLoc.S
  module E : Edge.S
  with type fence = A.fence
  and type dp = A.dp
  and type atom = A.atom
  module R : Relax.S
  with type fence = A.fence
  and type dp = A.dp
  and type edge = E.edge
  module C : Cycle.S with type edge=E.edge and type atom = A.atom
end
