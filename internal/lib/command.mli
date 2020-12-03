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

(** [run ~stdin ~stdout ~stderr bin args] runs the binary [bin] with arguments [args].
 *  The optional parameters [~stdin], [~stdout], and [~stderr] are functions
 *  that are applied to the stdin, stdout, and stderr of the command, in that
 *  order.
 *  It raises [Error] on error or non-zero exit code. *)
val run :
  ?stdin:(out_channel -> unit) ->
  ?stdout:(in_channel -> unit) ->
  ?stderr:(in_channel -> unit) -> string -> string list -> unit

(** [run_with_stdout ~stdin bin args f] runs the binary [bin] with arguments
 *  [args], optionally applies [~stdin] to the process's stdin, and applies
 *  function [f] the process's stdout, returning the result.
 *  It raises [Error] on error or non-zero exit code.
 *  This function is DEPRECATED: please use [run]. *)
val run_with_stdout :
  ?stdin:(out_channel -> unit) ->
    string -> string list -> (in_channel -> 'a) -> 'a

(** [run_with_stdin_and_stdout bin args f g] runs the binary [bin] with
 *  arguments [args], applies [f] to the process's stdin, returns the result of
 *  applying [g] to the process's stdout.
 *  This function is DEPRECATED: please use [run]. *)
val run_with_stdin_and_stdout : string -> string list -> (out_channel -> unit) -> (in_channel -> 'a) -> 'a
