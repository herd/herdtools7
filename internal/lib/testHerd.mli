(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** [herd_command herd libdir litmus] returns the command line that [run_herd]
  * would run. *)
val herd_command: string -> string -> string -> string

type stdout_lines = string list
type stderr_lines = string list

(** [run_herd herd libdir litmus] runs the binary [herd] with a custom [libdir]
  * on a [litmus] file, and returns the stdout with unstable lines removed (e.g.
  * Time) and stderr *)
val run_herd : string -> string -> string -> stdout_lines * stderr_lines

(** [herd_output_matches_expected herd libdir litmus expected expected_failure] runs the binary
  * [herd] with a custom [libdir] on a [litmus] file, and compares the output
  * with an [expected] file. If the run writes to stderr then we check [expected_failure].
  * If the contents of [expected_failure] match then it is an expected failure, otherwise
  * it is an unexpected failure and will raise an Error. *)
val herd_output_matches_expected : string -> string -> string -> string -> string -> bool

(** [is_litmus filename] returns whether the [filename] is a .litmus file. *)
val is_litmus : string -> bool

(** [is_expected filename] returns whether [filename] is a .litmus.expected file. *)
val is_expected : string -> bool

(** [expected_of_litmus filename] returns the .litmus.expected name for a given .litmus [filename]. *)
val expected_of_litmus : string -> string

(** [litmus_of_expected filename] returns the .litmus name for a given .litmus.expected [filename]. *)
val litmus_of_expected : string -> string

(** [expected_failure_of_litmus filename] returns the .litmus.expected-failure name for a given .litmus [filename]. *)
val expected_failure_of_litmus : string -> string

(** [litmus_of_expected_failure filename] returns the .litmus name for a given .litmus.expected-failure [filename]. *)
val litmus_of_expected_failure : string -> string
