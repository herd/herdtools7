(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
exception Error
}

let blank = [' ''\n''\r']
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
