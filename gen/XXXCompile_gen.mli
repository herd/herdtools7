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
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_zero :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_one :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_eq :
      A.st -> Code.proc -> A.init -> Code.loc -> A.reg ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_value :
      A.st -> Code.proc -> A.init -> Code.loc -> int ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_access : A.st -> Code.proc -> A.init -> C.event ->
    A.reg option * A.init * A.pseudo list * A.st

  val emit_exch : A.st -> Code.proc -> A.init -> C.event ->  C.event ->
    A.reg * A.init * A.pseudo list * A.st

  val emit_access_dep :
      A.st -> Code.proc -> A.init -> C.event -> A.dp ->
        A.reg -> Code.v -> A.reg option * A.init * A.pseudo list * A.st

  val emit_exch_dep :
      A.st -> Code.proc -> A.init -> C.event ->  C.event ->
        A.dp -> A.reg ->
          A.reg * A.init * A.pseudo list * A.st

(* Fences *)
  val emit_fence : A.fence -> A.pseudo

  val stronger_fence : A.fence

(* Code additions *)
  val check_load :
      Code.proc -> A.reg -> C.event -> A.pseudo list -> A.pseudo list

  val postlude : A.st -> Code.proc -> A.init -> A.pseudo list ->
    A.init * A.pseudo list * A.st

  val get_xstore_results : A.pseudo list -> (A.reg * int) list

end
