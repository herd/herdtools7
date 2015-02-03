(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type S = sig
  module A : Arch.S

  val extract_addrs : A.instruction -> StringSet.t
  val stable_regs : A.instruction -> A.RegSet.t
  val emit_loop : A.Out.ins list -> A.Out.ins list
  val compile_ins :
      (Label.t -> string) ->
        A.instruction ->  A.Out.ins list -> A.Out.ins list

(* For time base barrier *)
  val emit_tb_wait :  A.Out.ins list ->  A.Out.ins list
end
