(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* Tyler Sorensen, University College London, UK.                    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Check code w.r.t. bell definitions *)

module Make :
  functor (C:sig val bell_model_info : Bell_info.model option end) ->
    functor (A:Arch.S) ->
      sig
        val check : A.pseudo MiscParser.t -> unit
      end
