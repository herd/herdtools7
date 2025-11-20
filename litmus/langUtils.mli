(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

val start_label : int -> string
val start_comment : string -> int -> string
val end_label : int -> string
val end_comment : string -> int -> string
val code_fun : int -> string
val code_fun_cpy : int -> string
val code_fun_type : int -> string
val dump_code_def : out_channel -> bool -> bool -> Mode.t -> int -> string -> unit
val dump_code_call : out_channel -> string -> string -> string -> unit
