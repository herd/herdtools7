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

module Make: functor (O:Indent.S) -> functor (I:CompCondUtils.I) ->
  sig
    val fundef :
        (I.Loc.t -> string) -> (* For types *)
          (I.Loc.t,I.V.t) ConstrGen.cond -> unit 

    val fundef_onlog :
          (I.Loc.t,I.V.t) ConstrGen.cond -> unit

    val funcall :
        I.C.constr ->
          (I.Loc.t -> string) -> (string -> string) -> string
  end
