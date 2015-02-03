(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Simple utilities used in many models *)

module  Make : functor (O:Model.Config) -> functor (S:SemExtra.S) -> sig
(*******************************************)
(* Complete re-computation of dependencies *)
(*******************************************)
    
  val make_procrels : (S.event -> bool) -> S.concrete -> S.procrels
  val pp_procrels : string -> S.procrels -> S.rel_pp

(*************************)
(* Some checks factorized *)
(*************************)

  val pp_failure : S.test -> S.concrete -> string -> S.rel_pp -> unit

  val check_through : bool -> bool

(* Includes pretty printing of failures *)
  val check_uniproc :
      S.test ->
        S.concrete -> 
          S.event_rel (* rf *)->
            S.event_rel (* fr *)->
              S.event_rel (* co *) -> bool

  val check_atom :
      S.test ->
        S.concrete -> 
            S.event_rel (* fr *)->
              S.event_rel (* co *) -> bool
end
