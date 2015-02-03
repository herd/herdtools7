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

(** Show dot files as Postscript, controlled by '-gv' option *)

module Make : functor (O:PrettyConf.S)  -> sig
(* Fork a gv window to show that file *)
val show_file : string -> unit

(* Idem, but show the graph produced by the argument function *)
val show : (out_channel -> unit) -> unit

end
