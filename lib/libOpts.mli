(****************************************************************************)
(*                           The Diy Toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Command line options common to many *)

(* Define options "-q" (quiet) and "-v",
   The int reference passed as argument records a verbosity level *)
val parse_verbose : int ref ->  (Arg.key * Arg.spec * Arg.doc) list


(* Option "-o", sets output to directory or tar file. *)
val parse_dest : string option ref -> Arg.key * Arg.spec * Arg.doc
