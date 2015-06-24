(*********************************************************************)
(*                          DIY                                      *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
exception Error
}

let blank = [' ''\n']
let digit = ['0'-'9']
let loc = [^'='' '',']+
rule main m = parse
| ',' | blank+ { main m lexbuf }
| (digit+ as proc ':' loc as key)
    blank* '=' blank*  (loc as v)
   {
    let m = StringMap.add key (Printf.sprintf "%s:%s" proc v) m in
    main m lexbuf
   }
| eof { m }
| "" { raise Error }

{
let parse s = main StringMap.empty (Lexing.from_string s)
}
