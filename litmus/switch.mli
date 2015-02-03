(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Hashconsed switch trees *)

exception Cannot

module Make : functor (O:Indent.S) -> functor (I:CompCondUtils.I) ->
  sig
    type switch
    val compile :  (I.Loc.t,I.V.t) ConstrGen.prop -> switch
    val dump : Indent.t -> switch -> unit
  end
