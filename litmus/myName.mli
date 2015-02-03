(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
val as_symbol : Name.t -> string
