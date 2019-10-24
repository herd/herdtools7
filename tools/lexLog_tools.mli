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
  val rename : string -> string
  val ok : string -> bool
  val hexa : bool
  val int32 : bool
  val acceptBig : bool
end

module Make(O:Config) : sig
  val read_chan : string -> in_channel ->  LogState.t
  val read_chan_simple : string -> in_channel ->  LogState.simple_t
  val read_name : string ->  LogState.t
  val read_names : string list -> LogState.t list
  val read_names_simple : string list -> LogState.simple_t list
end
