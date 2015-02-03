(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

{

}

let digit = [ '0'-'9' ]
let num = digit+
let hexa = ['0'-'9' 'a'-'f' 'A'-'F' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name = alpha (alpha|digit)*
let blank = [' ' '\t']
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+' | '[' | ']')+

rule main add env = parse
| eof { env }
|
  ("Cycle=" ([^'\n']+ as cycle) '\n') ?
  "Relax" blank+
  (testname as name) blank+
  ("Ok"|"No" as v) blank+
    ([^'\n']* as rem) '\n'
  ("Safe" blank* '=' blank* ([^'\n']* as safes) '\n') ?
    {
     let v =
       match v with
       | "Ok" -> true | "No" -> false
       | _ -> assert false in
     let relaxs = LexUtil.split rem in
     let safes = match safes with
     | None -> []
     | Some rem ->  LexUtil.split rem in
     let cycle = match cycle with
     | None -> ""
     | Some cy -> cy in
     main add (add env name v relaxs safes cycle) lexbuf
   }
| [^'\n']* '\n' { main add env lexbuf }
| "" { env }

{

let tokens add env lexbuf = main add env lexbuf

}
