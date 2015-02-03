(*********************************************************************)
(*                          DIY/Litmus                               *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*     Luc Maranget, INRIA Paris-Rocquencourt, France.               *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


(* Dumping litmus-style internal representation of C code *)

type code = string CAst.t

val dump_prog_lines :  code list -> string list
val dump_prog : code -> string list
val print_prog : out_channel -> code list -> unit
