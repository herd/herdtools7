(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Run a test from source file *)

module type Config = sig
  val model : Model.t option
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
  val byte : MachSize.Tag.t
end

module Top :
  functor (C : Config) ->
  sig
    val from_file : string -> TestHash.env -> TestHash.env
  end
