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

(** Utilities for handling in_channel and out_channel. *)

(* in_channel utilities *)

(** [iter_lines f chan] applies function [f] in turn to each line of in_channel [chan]. *)
val iter_lines : (string -> unit) -> in_channel -> unit

(** [map_lines f chan] applies function [f] to each line of in_channel [chan],
 *  and returns the list [f l1; ...; f ln]. *)
val map_lines : (string -> 'a) -> in_channel -> 'a list

(** [map_opt_lines f chan] applies function [f] to each line of in_channel [chan],
 *  and returns the list [r1; ...; rm], where the [rj]'s are the successul
 * results of applying [f] to a line in channnel, _i.e._ [f li] returns
 * [Some rj].  Line order is preserved. *)
val map_opt_lines : (string -> 'a option              ) -> in_channel -> 'a list

(** [read_lines chan] reads all of in_channel [chan] as lines into a string list. *)
val read_lines : in_channel -> string list


(* out_channel utilities *)

(** [write_lines chan lines] writes every line in [lines] to out_channel [chan]. *)
val write_lines : out_channel -> string list -> unit
