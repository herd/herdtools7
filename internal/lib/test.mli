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

(** Unit-testing utilities. *)

(** Raising [AssertionFailure msg] causes a test to fail, printing the reason
 *  for failing [msg]. *)
exception AssertionFailure of string

(** [run tests] runs every named test in [tests], printing an error message if
 *  a test fails. *)
val run : (string * (unit -> unit)) list -> unit

(** [fail msg] raises an AssertionFailure with error message [msg], causing
 *  the test to fail. *)
val fail : string -> unit
