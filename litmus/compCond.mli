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

module Make: functor (O:Indent.S) -> functor (I:CompCondUtils.I) ->
  sig

      val fundef_prop :
          string ->
            (I.Loc.t -> string * bool) -> (* For types *)
              I.C.prop -> (I.Loc.t -> string) -> unit

      val fundef :
          (I.Loc.t -> string * bool) -> (* For types *)
            I.C.cond -> (I.Loc.t -> string) -> unit

      val fundef_onlog_prop : string -> I.C.prop -> (I.Loc.t -> string) -> unit

      val fundef_onlog : I.C.cond -> (I.Loc.t -> string) -> unit

      val funcall_prop :
        string -> I.C.prop ->
          (I.Loc.t -> string) -> (string -> string) -> string

      val funcall :
          I.C.cond ->
            (I.Loc.t -> string) -> (string -> string) -> string
  end
