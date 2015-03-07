(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*********************************)
(* Dump or run a series of tests *)
(*********************************)

open Answer

module type Config = sig
  val carch : Archs.System.t option
  val platform : string
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
  include RunUtils.CommonConfig
  val mkopt : Option.opt -> Option.opt
end


module type OneTest = sig
  val from_file :
      StringSet.t -> Answer.info  StringMap.t ->
        string -> out_channel -> answer
end

module Make :
  functor (O:Config) -> functor(Tar : Tar.S) -> functor (CT : OneTest) ->
  sig val from_files : string list -> unit end   
