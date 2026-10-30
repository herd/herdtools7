(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Apply change in test first section *)

module type Config =
sig
  type state
  val verbose : int
  val split : string -> in_channel -> Splitter.result
  val rewrite_fst :
    state -> Name.t -> Lexing.lexbuf -> string option * state
  end


module Make :
functor (O : Config) ->
functor (Out:OutTests.S) ->
  CopyTest.S with type out = Out.t and type state = O.state
