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

(** Utilities for running commands. *)

exception Error of string

(** [command bin args] returns a fully escaped command line for running the
 *  binary [bin] with arguments [args]. *)
val command : string -> string list -> string

(** [run bin args] runs the binary [bin] with arguments [args].
 *  It raises Error on error or non-zero exit code. *)
val run : string -> string list -> unit

(** [run_with_stdout bin args f] runs the binary [bin] with arguments [args], and
 *  applies function [f] to the open in_channel, returning the result. *)
val run_with_stdout : string -> string list -> (in_channel -> 'a) -> 'a

(** [run_with_stdout_and_stdin_lines bin args in_lines] runs the binary [bin]
  * with arguments [args], pipes [in_lines] into the process's stdin, and returns
  * the process's stdout as a string list. *)
val run_with_stdin_and_stdout : string -> string list -> (out_channel -> unit) -> (in_channel -> 'a) -> 'a
