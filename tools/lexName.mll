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

(* Lex conventional names *)
{
open EdgeName

exception Error of string

let error msg = raise (Error msg)

}

let sys = "W"|"RR"|"RW"|"WR"|"WW"
let alpha = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let word = (alpha|digit|'-')+

rule name = parse 
| [^'+'' ']+
| "2+2W"
| "WRR+WR"
| "WRW+WR"
| "WRR+2W"
| "WRW+2W"
| "W+RWC"
| (sys ('+' sys)+) as lxm
  { lxm }
| "" { error "No name" }

and custom = parse
| word as lxm { Custom lxm }
| "" { error "edge syntax" }

and parse_atoms = parse
| 'A' { AA :: parse_atoms lexbuf }
| 'P' { PP :: parse_atoms lexbuf }
| ""  { [] }

and atoms = parse
| ['A''P']+ as lxm { Atom (parse_atoms (Lexing.from_string lxm)) }
| "" { custom lexbuf }

and nedge = parse
| "po" { Po }
| "pos" { PoS }
| "addr" { Addr }
| "data" { Data }
| "ctrl" { Ctrl }
| "ctrlisync" { CtrlISync }
| "ctrlisb" { CtrlISB }
| "rfi" { Rfi }
| "fri" { Fri }
| "wsi" { Wsi }
| "isync" { ISync }
| "isb" { ISB }
| "eieio" { Eieio }
| "lwsync" { LwSync }
| "sync"   { Sync }
| "dmb.st" { DMBST }
| "dmb" { DMB }
| "dsb.st" { DSBST }
| "dsb" { DSB }
| ""  { atoms lexbuf }

and atom = parse
| ""|"p" { P }
| "r"    { R }
| "a"    { A }

and plural = parse
| ""  { N }
| "s" { S }


and edge = parse
""
{
 let e = nedge lexbuf in
 let a1 = atom lexbuf in
 let a2 = atom lexbuf in
 let p = plural lexbuf in
 e,a1,a2,p
}

and edges = parse
| '+' 
   {
    let e = edge lexbuf in
    e::edges lexbuf }
| ""        { [] }

{
let read s =
  let lexbuf = Lexing.from_string s in
  let name = name lexbuf in
  let es = edges lexbuf in
  name,es
}
