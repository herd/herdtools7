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
functor (O:sig val debug : bool end) ->
  functor (A:Arch.S) ->
      functor
        (C:sig
          val info : BellModel.info option
          val get_id_and_list : A.instruction -> string * string list
          val set_list : A.instruction -> string list -> A.instruction
        end) ->
  sig
    val check : A.pseudo MiscParser.t -> A.pseudo MiscParser.t
  end
