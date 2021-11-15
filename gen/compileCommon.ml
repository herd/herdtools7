(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val verbose : int
  val show : ShowGen.t option
  val same_loc : bool
  val unrollatomic : int option
  val allow_back : bool
  val typ : TypBase.t
  val hexa : bool
  val moreedges : bool
  val variant : Variant_gen.t -> bool
end

module type S = sig
  module A : Arch_gen.S

  module E : Edge.S
  with type fence = A.fence
  and type dp = A.dp
  and module SIMD = A.SIMD
  and type atom = A.atom
  and type rmw = A.rmw

  type check = E.edge list list -> bool

  module R : Relax.S
  with type fence = A.fence
  and type dp = A.dp
  and type edge = E.edge

  module C : Cycle.S
   with type fence = A.fence
   and type edge=E.edge
   and module SIMD = A.SIMD
   and type atom = A.atom
   and module PteVal = A.PteVal
end

module Make(C:Config) (A:Arch_gen.S) = struct
  module A = A

  module E =  Edge.Make(C)(A)

  type check = E.edge list list -> bool

  let () = match C.show with
  | Some s -> begin
      try E.show s ; exit 0
      with e -> Printexc.print_backtrace stderr ;
        flush stderr ; raise e
  end
  | None -> ()

  module R = Relax.Make(A) (E)
  module Conf = struct
    include C
    let naturalsize = TypBase.get_size C.typ
  end
  module C = Cycle.Make(Conf)(E)
(* Big constant *)
  let kbig = 128
(* Postlude *)

  let mk_postlude emit_store_reg st p init cs =
    if A.get_noks st > 0  then
      let ok,st = A.ok_reg st in
(* Add explict `int` type for `ok<n>` variables *)
      let ok_loc = Code.as_data (Code.myok_proc p) in
      let st = A.add_type (A.Loc ok_loc) TypBase.Int st in
      let init,cs_store,st = emit_store_reg st p init ok_loc ok in
      let csok = A.Label (Label.last p,A.Nop)::cs_store in
(* Add explict initialvalue of zero for `ok<n>` variables *)
      (A.Loc ok_loc,Some (A.S "0"))::init,cs@csok,st
    else
      init,cs,st

end
