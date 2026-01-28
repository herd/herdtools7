(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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

(** Type for speedcheck argument *)
type speedcheck = [`True | `False | `Fast]

(** Systematic file names for standard output and standard error , from test file name *)
val outname : string -> string
val errname : string -> string

(** [read_file name] returns the contents of file [name] as a list
 * of lines. Returns the empty list when file is absent *)
val read_file : string -> string list

(** [is_stable line] returns [true] when line from output is to be kept *)
val is_stable : string -> bool

(** Format herd command-line options as a list *)
val herd_args :
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
  timeout  : float option ->
  speedcheck : speedcheck option ->
  checkfilter : bool option ->
  string list

(** [apply_args herd j args] Format mapply command-line options as a list,
 *  where [herd] is path to herd command, [j] is concurrency leval and
 *  [args] is the list of [herd] command-line options. *)
val apply_args : string -> int -> string list -> string list

(** Same as above, with additional redirection of output channels
 *  to conventional files. *)
val apply_redirect_args : ?verbose:bool -> string -> int -> string list -> string list

(** [herd_command ~bell ~cat ~conf ~variants ~libdir herd ?j litmuses] returns the
 *  command line that [run_herd] would run. *)
val herd_command :
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
  path ->
  ?j:int -> ?timeout:float -> ?speedcheck:speedcheck -> ?checkfilter:bool ->
  path list -> string

(** [check_tags line] Checks that a line is a verbose diagnostic. *)
val check_tags : string -> bool

(** [run_herd ~bell ~cat ~conf ~variants ~libdir herd ?j litmuses] runs the
 *  binary [herd] with a custom [libdir] on list of litmus files [litmuses],
 *  and returns the stdout with unstable lines removed (e.g. Time) and stderr.
 *  Paths to [cat], [bell], and [conf] files, as well as [variants], can also
 *  be passed in.
 * If argument [j] is present, at most [j] tests are run concurrently *)
val run_herd :
  ?verbose:bool ->
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
  path ->
  ?j:int -> ?timeout:float -> ?speedcheck:speedcheck -> ?checkfilter:bool ->
  path list -> int * string list * string list

(** [run_herd_args herd args litmus] similar in functionality  to
  * [run_herd] above but different as regards interface:
  *   1. Command-line options are given as a list of strings;
  *   2. One litmus test only is given as argument.
  *)
val run_herd_args :
  ?verbose:bool -> path -> string list -> path ->
    int * string list * string list

(** [run_herd_concurrent ~bell ~cat ~conf ~variants ~libdir herd j litmuses]
 *  Similar to [run_herd] except that output is stored into files specific
 *  to each test: [litmus].out and [litmus].err. *)
val run_herd_concurrent :
  ?verbose:bool ->
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
     path -> j:int-> path list -> int

(** Type of comparison for stdout logs *)
type check =
  | All (** Complete, except non stable item such as time *)
  | Obs (** Observation, _i.e._ Newver/Sometimes/Always *)
  | Sta (** Final states *)

val pp_check : check -> string

(** [herd_output_matches_expected check nohash litmus expected] returns true when
 * the output file produced by running [litmus] matches reference
 *  [expected]. If argument [nohash] is true, hashes are not compared.
 * If argument [check] specifies the valididy check
 * (see type check above). *)
val output_matches_expected :
  ?check:check -> ?nohash:bool -> path -> path -> bool

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
  ?verbose : bool ->
  ?check   : check ->
  ?nohash  : bool ->
  bell     : path option ->
  cat      : path option ->
  conf     : path option ->
  variants : string list ->
  libdir   : path ->
    path -> path -> path -> path -> path  -> bool

(** [herd_args_output_mathes_expected herd args litmus
  *  expected expected_failure expected_warn] has the same functionality
  *  as [herd_output_matches_expected] above but a different interface,
  *   as command line options are given as the list [args]. *)
val herd_args_output_matches_expected :
  ?verbose:bool -> ?check:check -> ?nohash:bool ->
  path -> string list -> path -> path -> path -> path  -> bool

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

(** [promote  litmus result] it is assumed that result is the result of running the test [litmus].
  * Promote [result] as the reference for test [litmus]. If anyrging is wrong, return [false].
  *)
val promote : path -> (int * string list * string list) -> bool
