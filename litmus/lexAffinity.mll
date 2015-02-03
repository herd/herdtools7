(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
open Printf
open Affi

type coms = Affi.com list
type affinity = int list list * (int * int) list

exception Error of string

let error tag = raise (Error (sprintf "Lex Affinity: %s" tag))

let to_int s =
  try int_of_string s
  with Failure _ -> assert false
}

let space = [' ''\t']
let non_space = [^' ''\t']
let digit = ['0'-'9']
let num = digit+


rule coms = parse
| space+ { coms lexbuf }
| "Fr"   { Fr :: coms lexbuf }
| "Rf"   { Rf :: coms lexbuf }
| "Ws"   { Ws :: coms lexbuf }
| "Hat"  { Hat :: coms lexbuf }
| eof    { [] }
| non_space* as lxm   { error (sprintf "'%s'" lxm) }
   
and  affinity = parse
| ""
  { let cs = colors lexbuf in
    let ne = diffs lexbuf in
    cs,ne }

and colors = parse
| space* '['
  { let c = color lexbuf in
    c :: colors lexbuf }
| space* ';'
  { [] }
| "" { error "colors" }

and color = parse
| space+|',' { color lexbuf }
| num as x { to_int x::color lexbuf }
| ']' { [] }
| ""  { error "color" }

and diffs = parse
| space+ { diffs lexbuf }
| (num as x) ',' (num as y)
   { (to_int x,to_int y) :: diffs lexbuf }
| eof { [] }
| "" { error "diffs" }

{

 let coms s = coms (Lexing.from_string s)

 let affinity s = affinity  (Lexing.from_string s)

}
