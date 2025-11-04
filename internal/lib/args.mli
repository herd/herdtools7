(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities for using the built-in Arg module. *)

type spec = Arg.key * Arg.spec * Arg.doc

(* Specs. *)

(** [append_string r] builds an Arg.spec that appends to the string list
 *  referenced by [r]. *)
val append_string : string list ref -> Arg.spec

(** [set_string_option r] builds an Arg.spec that, for argument value [v], sets
 *  the string option referenced by [r] to [Some v]. *)
val set_string_option : string option ref -> Arg.spec


(** Common options *)

(** [npar j] Build an Arg.spec for setting j, with documentation as
 *  setting the parallelism level. *)

val npar : int option ref -> spec

(** [nohash b] Build an Arg.spec for setting b to true, with documentation
    as not checking hashes *)

val nohash : bool ref -> spec

(** [checkobs b] Build an Arg.spec for setting b to Obs, with documentation
    as limited test success check. *)

val checkobs : TestHerd.check ref -> spec

(** [checkstates b] Build an Arg.spec for setting b to Sta, with documentation
    as test success check limited to final states equality. *)

val checkstates : TestHerd.check ref -> spec

(** [verbose b] Build an Arg.spec for setting b to true, with documentation
    as enable some diagnostics. *)

val verbose : bool ref -> spec


(** Validators. *)

(** [test_file name] tests that [s] is not a valid path to a file *)
val test_file : string -> bool

(** [is_file (k, s, d)] returns [k, s', d], where [s'] wraps [s] with an
 *  Arg.spec that raises Arg.Bad if the argument is not a valid path to a file. *)
val is_file : spec -> spec

(** [is_dir (k, s, d)] returns [k, s', d], where [s'] wraps [s] with an
 *  Arg.spec that raises Arg.Bad if the argument is not a valid path to a
 *  directory. *)
val is_dir : spec -> spec
