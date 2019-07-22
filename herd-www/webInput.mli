(****************************************************************************)
(*                        the herdtools7 toolsuite                          *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

val webpath : string

(* Register command-line file contents as pseudo files.
   Returns file name *)
val set_bell_str : string -> string
val set_cat_str : string -> string
val set_cfg_str : string -> string
val set_litmus_str : string -> string

(* Initialise pseudo file-system *)
val register_autoloader : unit -> unit
