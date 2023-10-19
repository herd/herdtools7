(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Run a test from source file, arch being abstracted *)

module type Config = sig
  val model : Model.t option
  val archcheck : bool
  val through : Model.through
  val strictskip : bool
  val cycles : StringSet.t
  val bell_model_info : (string * BellModel.info) option
  val macros : string option
  val check_name : string -> bool
  val check_rename : string -> string option
  val libfind : string -> string
  include GenParser.Config
  include Top_herd.CommonConfig
  include Sem.Config

  val statelessrc11 : bool
  val dumpallfaults : bool
  val byte : MachSize.Tag.t
end

type runfun =
  DirtyBit.t option ->
  float (* start time *) ->
  string (* file name *) ->
  in_channel (* source channel *) ->
  TestHash.env ->
  Splitter.result ->
  TestHash.env

module Make :
functor(S:Sem.Semantics) ->
  functor
  (P:sig
    type pseudo
    val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
  end with type pseudo = S.A.pseudo) ->
    functor (M:XXXMem.S with module S = S) ->
      functor (C:Config) -> sig
        val run : runfun
      end
