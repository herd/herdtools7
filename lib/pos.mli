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


(** Positions in a file *)

open Lexing

type pos2 = position * position
(*********************)
(* Position printing *)   
(*********************)

val debug_pos : out_channel -> position -> unit

(* Understood by emacs *)
val pp_pos0 : out_channel -> string -> unit   (* no position available *)
val str_pos0 : string -> string
val pp_pos : out_channel -> position -> unit  (* one *)
val pp_lnum : out_channel -> position -> unit  (* one, line number only *)
val pp_pos2 : out_channel -> (position * position) -> unit (* two *)


