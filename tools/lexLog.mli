(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val verbose : int
  val rename : string -> string
  val ok : string -> bool
end

module Make(O:Config) : sig
  val read_chan : string -> in_channel ->  LogState.t
  val read_name : string ->  LogState.t
  val read_names : string list -> LogState.t list
  val read_names_simple : string list -> LogState.simple_t list
end
