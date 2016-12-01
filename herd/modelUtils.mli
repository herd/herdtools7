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

(** Simple utilities used in many models *)

module Make : functor (O:Model.Config) -> functor (S:SemExtra.S) -> sig
(*******************************************)
(* Complete re-computation of dependencies *)
(*******************************************)
    
  val make_procrels : (S.event -> bool) -> S.concrete -> S.procrels
  val pp_procrels : string option -> S.procrels -> S.rel_pp


(***************************)
(* Draw execution diagrams *)
(***************************)

  val pp : S.test -> S.concrete -> string -> S.rel_pp -> unit
  val pp_failure : S.test -> S.concrete -> string -> S.rel_pp -> unit


(*************************)
(* Some checks factorized *)
(*************************)

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
