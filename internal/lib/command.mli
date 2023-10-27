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

(** Utilities for running commands. *)

type error = {
  binary : string ;
  args   : string list ;
  status : Unix.process_status ;
}

exception Error of error

(** [string_of_error e] returns a human-readable representation of an error
 *  [e]. *)
val string_of_error : error -> string

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

(** Same as [run] above, does not raise [Error] on non-zero exit
  * code. Returns exit code *)
val run_status :
  ?stdin:(out_channel -> unit) ->
  ?stdout:(in_channel -> unit) ->
  ?stderr:(in_channel -> unit) -> string -> string list -> int

module NonBlock : sig

(** Non blocking execution of command:
 * input (stdin) and outputs (stdout and stderr) are seen as non-blocking
 * line-oriented channels. This assumes that the underlying stdlib
 * functions behave properly as regards their internal structures
 * when would-block conditions are met. *)

(** [run ~stdin ~stdout ~stderr bin args] runs the binary [bin] with arguments [args].
 *  The optional parameters [~stdin], [~stdout], and [~stderr] are functions
 *  that handle channels as suites of lines.
 *  It raises [Error] on error or non-zero exit code. *)
val run :
  ?stdin:(unit -> string option) ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) -> string -> string list -> unit

(** Same as [run] above, does not raise [Error] on non-zero exit
  * code. Returns exit code *)
val run_status :
  ?stdin:(unit -> string option) ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) -> string -> string list -> int

end
