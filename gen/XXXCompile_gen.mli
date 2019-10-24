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

module type S = sig
  include CompileCommon.S

  val ppo : (R.relax -> 'a -> 'a) -> 'a -> 'a

(* Accesses *)
  val emit_load :
      A.st -> Code.proc -> A.init -> string ->
        A.reg * A.init * A.pseudo list * A.st

(* Load for observation *)
  val emit_obs :
       A.st -> Code.proc -> A.init -> string ->
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

  val emit_rmw : A.rmw -> A.st -> Code.proc -> A.init -> C.event ->  C.event ->
    A.reg option * A.init * A.pseudo list * A.st

  val emit_access_dep :
      A.st -> Code.proc -> A.init -> C.event -> A.dp ->
        A.reg -> Code.v -> A.reg option * A.init * A.pseudo list * A.st

  val emit_rmw_dep :
      A.rmw -> A.st -> Code.proc -> A.init -> C.event ->  C.event ->
        A.dp -> A.reg ->
          A.reg option * A.init * A.pseudo list * A.st

(* Fences *)
  val emit_fence : Code.proc -> A.init -> C.node -> A.fence -> A.pseudo list
  val full_emit_fence : A.st -> Code.proc -> A.init -> C.node -> A.fence ->
    A.init * A.pseudo list * A.st

  val stronger_fence : A.fence

(* Code additions *)
  val check_load :
      Code.proc -> A.reg -> C.event -> A.init -> A.st ->
        A.init * (A.pseudo list -> A.pseudo list) * A.st

  val postlude : A.st -> Code.proc -> A.init -> A.pseudo list ->
    A.init * A.pseudo list * A.st

  val get_xstore_results : A.pseudo list -> (A.reg * int) list

end
