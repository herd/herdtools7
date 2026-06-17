(****************************************************************************)
(*                           the diy toolsuite                              *)
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

(** Timers for global profiling, timers are re-entrant. *)

module type S = sig
  (** [create key] Create and register timer,
      must be called at most once for a given key. *)
  val create : string -> unit

  (** [start key] Start the timer [key].
      If the timer is already running,
      do not restart timer.
      Alqways push a [key] marker. *)
  val start : string -> unit

(** [stop key] Stop the timer [key].
    Always pop a [key] marker, if no [key] marker are pending,
    accumulate the time spent. *)
  val stop : string -> unit

  (** Print all timers *)
  val pp : Format.formatter -> unit

end

val sem_key : string
val model_key : string
val run_key : string

module Ok : S
module No : S

