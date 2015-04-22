(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*     Luc Maranget, INRIA Paris-Rocquencourt, France.               *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

val start_comment : string -> int -> string
val end_comment : string -> int -> string
val dump_code_def : out_channel -> int -> string -> unit
val dump_code_call : out_channel -> string -> int -> string -> unit

