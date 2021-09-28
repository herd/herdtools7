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

(* Utilities for running Herd binaries in tests. *)

type path = string

type stdout_lines = string list
type stderr_lines = string list

(** [herd_command ~bell ~cat ~conf ~variants ~libdir herd litmuses] returns the
 *  command line that [run_herd] would run. *)
val herd_command :
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
    path -> path list -> string

(** [run_herd ~bell ~cat ~conf ~variants ~libdir herd litmuses] runs the
 *  binary [herd] with a custom [libdir] on list of litmus files [litmuses],
 *  and returns the stdout with unstable lines removed (e.g. Time) and stderr.
 *  Paths to [cat], [bell], and [conf] files, as well as [variants], can also
 *  be passed in. *)
val run_herd :
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
    path -> path list -> stdout_lines * stdout_lines

(** [herd_output_matches_expected ~bell ~cat ~conf ~variants ~libdir herd
 *  litmus expected expected_failure expected_warn] runs the binary
 *  [herd] with a custom [libdir] on a [litmus] file,
 *  and compares the output with an [expected] file.
 *  If the run writes to stderr then we check [expected_failure]. If the
 *  contents of [expected_failure] match then it is an expected failure,
 *  otherwise it is an unexpected failure and will raise an Error.
 *  If the run writes to both stdout and stderr, stdout is checked
 *  against the [expected] file, while stderr is checked against
 *  the [expected_warn] file. If any file is missing or differs,
 *  an Error is raised. 
 *  Paths to [cat], [bell], and [conf] files, as well as [variants], can also
 *  be passed in. *)
val herd_output_matches_expected :
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
    path -> path -> path -> path -> path  -> bool

(** [is_litmus filename] returns whether the [filename] is a .litmus file. *)
val is_litmus : path -> bool

(** [is_expected filename] returns whether [filename] is a .litmus.expected file. *)
val is_expected : path -> bool

(** [expected_of_litmus filename] returns the .litmus.expected name for a given .litmus [filename]. *)
val expected_of_litmus : path -> path

(** [litmus_of_expected filename] returns the .litmus name for a given .litmus.expected [filename]. *)
val litmus_of_expected : path -> path

(** [expected_failure_of_litmus filename] returns the .litmus.expected-failure name for a given .litmus [filename]. *)
val expected_failure_of_litmus : path -> path

(** [litmus_of_expected_failure filename] returns the .litmus name for a given .litmus.expected-failure [filename]. *)
val litmus_of_expected_failure : path -> path

(** [expected_warn_of_litmus filename] returns the .litmus.expected-warn name for a given .litmus [filename]. *)
val expected_warn_of_litmus : path -> path
