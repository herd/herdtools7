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
     let name = Misc.clean_name name in
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
