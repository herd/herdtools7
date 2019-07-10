(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Replace -O... by -O0 *)

{

}

rule zyva buf = parse
| "-O" ['0'-'9']+ { Buffer.add_string buf "-O0" ; zyva buf lexbuf }
| _ as c         { Buffer.add_char buf c ; zyva buf lexbuf }
| eof            { Buffer.contents buf }

{
 let tr s = zyva (Buffer.create 16) (Lexing.from_string s)      
}
