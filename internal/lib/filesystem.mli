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

(** Filesystem and file utilities. *)

(** [read_file path f] opens a file for reading at [path], and calls [f] on the
 *  open channel. The file is closed after [f] returns. If an exception is
 *  raised, the file is closed before re-raising the exception. *)
val read_file : string -> (in_channel -> 'a) -> 'a

(** [write_file path f] opens a file for writing at [path], and calls [f] on
 *  the open channel. The file is closed after [f] returns. If an exception is
 *  raised, the file is closed before re-raising the exception. *)
val write_file : string -> (out_channel -> 'a) -> 'a

(** [remove_recursive path] removes [path] and all of its children, a la `rm -rf`. *)
val remove_recursive : string -> unit

(** [new_temp_dir ()] creates a new temporary directory, and returns the path. *)
val new_temp_dir : unit -> string
