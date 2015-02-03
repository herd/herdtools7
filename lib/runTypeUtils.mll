(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{

}

rule mutex_inside = parse
| "mutex" { true }
| _       { mutex_inside lexbuf }
| eof     { false }


{
let mutex_is_substring s = mutex_inside (Lexing.from_string s)
}
