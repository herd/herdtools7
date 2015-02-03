(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Utilities for pretty-printing *)

module Make : functor (S:SemExtra.S) -> sig
  (* Organize events, first by proc, then by po *)
  val make_by_proc_and_poi : S.event_structure -> S.event_set list list

  (* Observed read events *)
  val observed : S.test -> S.event_structure -> S.event_set

  (* All registers that read from memory *)
  val all_regs_that_read : S.event_structure -> S.loc_set
end
