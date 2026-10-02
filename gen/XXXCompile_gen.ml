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

open Gen_common 

module type Config = sig
  val debug : Debug_gen.t
  val show : ShowGen.t option
  val same_loc : bool
  val unrollatomic : int option
  val allow_back : bool
  val typ : TypBase.t
  val hexa : bool
  val moreedges : bool
  val variant : Variant_gen.set
  val wildcard : bool
end

module type Common = sig
  module A : Arch_gen.S
  module E : Edge.S
  with type fence = A.fence
  and type dp = A.dp
  and module SIMD = A.SIMD
  and type atom = A.atom
  and module Value = A.Value
  and module RMW = A.RMW
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
  and module Value = A.Value
  and module RMW = A.RMW
end

module Make(C:Config)(A:Arch_gen.S) = struct
  module A = A

  module E =
    Edge.Make
      (struct
        let variant = C.variant
        let naturalsize = TypBase.get_size C.typ
        let wildcard = C.wildcard
        let debug = C.debug
      end)
      (A)(A)

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

  let kbig = 128

  let mk_postlude emit_store_reg st p init cs =
    if A.get_noks st > 0  then
      let ok,st = A.ok_reg st in
      let ok_loc = Code.as_data (Code.myok_proc p) in
      let st = A.add_type (A.Location.Location_global ok_loc) TypBase.Int st in
      let init,cs_store,st = emit_store_reg st p init ok_loc ok in
      let csok = A.Label (Label.last p,A.Nop)::cs_store in
      (A.Location.Location_global ok_loc,Some (A.S "0"))::init,cs@csok,st
    else
      init,cs,st

  (* Architecture-specific backends may shadow this default. *)
  let get_archinfo _ = []
end

module type S = sig
  include Common

  val ppo : (R.relax -> 'a -> 'a) -> 'a -> 'a

  val emit_load :
      A.st -> Code.proc -> A.init -> string ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_obs :
       A.SIMD.atom Code.bank -> A.st -> Code.proc -> A.init -> string ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_obs_not_zero :
      A.st -> Code.proc -> A.init -> string ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_one :
      A.st -> Code.proc -> A.init -> string ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_obs_not_eq :
      A.st -> Code.proc -> A.init -> string -> A.reg ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_obs_not_value :
      A.st -> Code.proc -> A.init -> string -> int ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_access : A.st -> Code.proc -> A.init -> C.event ->
    A.reg option * A.init * A.pseudo list * A.st

  val emit_rmw : A.RMW.rmw -> A.st -> Code.proc -> A.init -> C.event ->  C.event ->
    A.reg option * A.init * A.pseudo list * A.st

  val emit_access_dep :
      A.st -> Code.proc -> A.init -> C.event -> A.dp ->
        A.reg -> C.node -> A.reg option * A.init * A.pseudo list * A.st

  val emit_rmw_dep :
      A.RMW.rmw -> A.st -> Code.proc -> A.init -> C.event ->  C.event ->
        A.dp -> A.reg -> C.node ->
          A.reg option * A.init * A.pseudo list * A.st

  val emit_fence : A.st -> Code.proc -> A.init -> C.node -> A.fence ->
    A.init * A.pseudo list * A.st

  val emit_fence_dp : A.st -> Code.proc -> A.init -> C.node -> A.fence -> A.dp ->
    A.reg -> C.node -> A.reg option * A.init * A.pseudo list * A.st
  val stronger_fence : A.fence

  val check_load :
      Code.proc -> A.reg -> C.event -> A.init -> A.st ->
        A.init * (A.pseudo list -> A.pseudo list) * A.st

  val postlude : A.st -> Code.proc -> A.init -> A.pseudo list ->
    A.init * A.pseudo list * A.st

  val get_xstore_results : A.pseudo list -> (A.reg * int) list
  val get_archinfo : C.node -> (string * string) list
end
