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
let blank = [' ''\t''\r']
let not_blank = [^' ''\t''\n''\r']
let alpha = ['A'-'Z' 'a'-'z']
let opt = '-' (('_'|alpha)+)
 
rule main = parse
| eof { [] }
| (opt as opt) blank* '\n' {opt :: main lexbuf}
| (opt as opt) blank+
  ((not_blank [^'\n']* not_blank | not_blank) as arg)
   blank* 
 {opt :: arg :: main lexbuf}
| blank* '\n' {main lexbuf}
| '#' [^'\n']* '\n' {main lexbuf}
| "" {Warn.fatal "Bad conf file"}

{

let conf chan = main (Lexing.from_channel chan)

}
