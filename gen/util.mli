(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


type spec = string * Arg.spec * string

val parse_tag :
    string ->
      (string -> bool) -> string list -> string ->
          string * Arg.spec * string

val arch_opt : Archs.t ref -> spec


val parse_cmdline : spec list -> (string -> unit) -> unit

