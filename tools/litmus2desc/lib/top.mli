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

type error = string

val explain_test :
  ?libdir:string ->
  ?latex_compat:bool ->
  ?describe_dep_path:bool ->
  string ->
  (string list, error) result
(** [explain_test contents] explains the litmus test [contents].

    On success, returns one formatted prose description per unique candidate
    execution satisfying the post-condition.

    Candidate executions with identical prose descriptions are reported only
    once.

    [libdir] overrides herd's library directory, [latex_compat] renders each
    execution description as LaTeX, and [describe_dep_path] includes
    intermediate instructions when describing dependency paths.

    Returns [Error reason] if the test in [contents] is not supported. *)
