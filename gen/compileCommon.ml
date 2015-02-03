(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


module type Config = sig
  val verbose : int
  val list_edges : bool
  val coherence_decreasing : bool
  val same_loc : bool
  val sta : bool
  val unrollatomic : int option
  val allow_back : bool
end

module type S = sig
  module A : Arch.S

  module E : Edge.S
  with type fence = A.fence
  and type dp = A.dp
  and type atom = A.atom

  type check = E.edge list list -> bool

  module R : Relax.S
  with type fence = A.fence
  and type dp = A.dp
  and type edge = E.edge
  
  module C : Cycle.S with type edge=E.edge and type atom = A.atom
end

open Printf

module Make(C:Config) (A:Arch.S) =
struct
  module A = A 

  module E =  Edge.Make(A)
  type check = E.edge list list -> bool

  let () =
    if C.list_edges then begin
      eprintf "Edges:" ;
      let es = E.fold_pp_edges (fun s k -> s::k) [] in
      let es = List.sort String.compare es in
      List.iter (eprintf " %s") es ;        
      eprintf "\n%!" ;
      exit 0
    end


  module R = Relax.Make(A) (E)
  module C = Cycle.Make(C)(E)
end

