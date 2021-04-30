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

(** Utilities for pretty-printing *)

module Make : functor (S:SemExtra.S) -> sig

  (* Organize events, first by proc, then by po *)
  val make_by_proc_and_poi :
    S.event_structure -> S.event_set list list

  (* Extract spurious events *)
  val spurious_events : S.event_structure -> S.event_set

  (* Observed read events *)
  val observed : S.test -> S.event_structure -> S.event_set

  (* All registers that read from memory *)
  val all_regs_that_read : S.event_structure -> S.loc_set
end
