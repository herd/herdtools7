(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


{
type t = No | Now | Next
}


rule see = parse
| _ * "START" { Now }
| _ * "_litmus_P" ['0'-'9']+ '_' ['0'-'9']+ { Next }
| "" { No }

{
let see s = see (Lexing.from_string s)
}
