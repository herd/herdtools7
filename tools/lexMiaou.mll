(****************************************************************************)
(*                           The diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
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

let alpha = ['a'-'z''A'-'Z']

rule main m = parse
| "\\newcommand{\\" (alpha+ as name) '}'
  '[' (['1'-'9']['0'-'9']* as nargs) ']' [^'\n']+ '\n'
    {
     let ty =
       match nargs with
       | "1" -> Some AST.SET
       | "2" -> Some AST.RLN
       | _ -> None in
     main (StringMap.add name ty m) lexbuf }
| "\\new" "is"? "relation" "rev"? "{" (alpha+ as name) "}" [^'\n']* '\n'
    { main (StringMap.add name (Some AST.RLN)  m) lexbuf }
|  [^'\n']* '\n'
    { main m lexbuf }
|  [^'\n']* eof
    { m }

{

 let csnames fname =
   Misc.input_protect
     (fun chan -> main StringMap.empty (Lexing.from_channel chan))
     fname
}
