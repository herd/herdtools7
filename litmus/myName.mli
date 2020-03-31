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

(*************************************)
(* Extended Filename like facilities *)
(*************************************)

(* Chop ".litmus" extension, if present *)
val chop_litmus : string -> string

(* outname base ext returns output file name *)
val outname : string -> string -> string

(* Open litmus own files *)
val open_lib : string -> string * in_channel

(* Get litmus own file complete name *)
val name_lib : string -> string

(* Read config file *)
val read_cfg : string -> unit

(******************)
(* Legal C symbol *)
(******************)
val name_as_symbol : string -> string
val as_symbol : Name.t -> string
