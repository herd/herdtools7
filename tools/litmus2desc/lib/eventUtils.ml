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

module Make (E : Event.S) = struct
  module Act = E.Act
  module A = E.A

  type access = {
    dir : Dir.dirn;
    loc : Act.A.location;
    value : Act.A.V.v;
    is_implicit : bool;
  }

  type action_view = Access of access | Branching of branch_type | Barrier

  let action_view (e : E.event) : action_view option =
    let action = e.E.action in
    if E.Act.is_load action || E.Act.is_store action then
      let dir = if E.Act.is_load action then Dir.R else W in
      let loc =
        match E.Act.location_of action with
        | Some loc -> loc
        | None -> invalid_arg "register or memory action without location"
      in
      let value =
        match E.Act.value_of action with
        | Some v -> v
        | None -> invalid_arg "register or memory action without value"
      in
      Some
        (Access { dir; loc; value; is_implicit = E.Act.is_not_explicit action })
    else if E.Act.is_barrier action then Some Barrier
    else if E.Act.is_commit action then
      let ty =
        if E.Act.is_bcc action then Bcc
        else if E.Act.is_pred action then Pred
        else invalid_arg "invalid commit action"
      in
      Some (Branching ty)
    else None

  let as_access ev =
    match action_view ev with Some (Access acc) -> Some acc | _ -> None

  let is_initial ev = match ev.E.iiid with E.IdInit -> true | _ -> false

  let value_of_exn ev =
    match E.value_of ev with Some v -> v | None -> invalid_arg "No value"

  let location_of_exn ev =
    match E.location_of ev with
    | Some loc -> loc
    | None -> invalid_arg "No location"

  let is_global_access ev = Option.is_some (E.global_loc_of ev)

  let instruction_of ev =
    match ev.E.iiid with
    | E.IdSome ii -> Some (A.pp_instruction PPMode.Ascii ii.A.inst)
    | E.IdInit | E.IdSpurious -> None

  let rel_succs x r = E.EventRel.succs r x

  let rel_find_unique_succ x r =
    match E.EventSet.as_singleton (E.EventRel.succs r x) with
    | Some x -> x
    | None -> invalid_arg "Successor is not unique"

  let rel_to_list r = E.EventRel.fold List.cons r []
end
