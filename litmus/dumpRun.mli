(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(*********************************)
(* Dump or run a series of tests *)
(*********************************)

open Answer

module type Config = sig
  val carch : Archs.System.t option
  val platform : string
  val makevar : string list
  val gcc : string
  val stdio : bool
  val index : string option
  val crossrun : Crossrun.t
  val adbdir : string
  val sleep : int
  val tarname : string
  val driver : Driver.t
  val cross : bool
  val hexa : bool
  val threadstyle : ThreadStyle.t
  val asmcommentaslabel : bool
  include RunUtils.CommonConfig
  val mkopt : Option.opt -> Option.opt
  val variant : Variant_litmus.t -> bool
end


module type OneTest = sig
  val from_file :
      StringSet.t -> hash_env-> string -> out_channel -> answer
end

module Make :
  functor (O:Config) -> functor(Tar : Tar.S) -> functor (CT : OneTest) ->
  sig val from_files : string list -> unit end   
