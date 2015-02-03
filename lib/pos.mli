(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


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


