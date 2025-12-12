(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Herd_core

type branch_type = Bcc | Pred

module Make (E : Event.S) : sig
  type access = {
    dir : Dir.dirn;
    loc : E.Act.A.location;
    value : E.Act.A.V.v;
    is_implicit : bool;
  }

  type action_view = Access of access | Branching of branch_type | Barrier

  val action_view : E.event -> action_view option
  val as_access : E.event -> access option
  val is_initial : E.event -> bool

  val value_of_exn : E.event -> E.A.V.v
  (** Get the value of an event.
      @raise Invalid_argument if the input event has no value. *)

  val location_of_exn : E.event -> E.A.location
  (** Get the location of an event.
      @raise Invalid_argument if the input event has no location. *)

  val is_global_access : E.event -> bool
  val instruction_of : E.event -> string option
  val rel_succs : E.event -> E.event_rel -> E.event_set

  val rel_find_unique_succ : E.event -> E.EventRel.t -> E.event
  (** [rel_find_unique_succ e r] returns the unique successor of [e] in [r].
      @raise Invalid_argument if [e] has zero or more than one successors. *)

  val rel_to_list : E.EventRel.t -> (E.event * E.event) list
end
