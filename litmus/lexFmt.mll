(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


{
open Fmt

let tr_prec = function
  | None -> 0
  | Some s -> try int_of_string s with _ -> assert false 
}

let digit = ['0'-'9']
let num = '-'? digit+
let sp = ' '*
rule main k = parse
| "%%" { main (Percent::k) lexbuf }
| '%'
  { 
    let pad = lexpad lexbuf in
    let conv = lexconv pad lexbuf in
    main (Conv conv::k) lexbuf }
| [^'%']+ as tok { main (Lit tok::k) lexbuf }
| eof { List.rev k }
| "" { failwith "lexFmt: main" }

and lexpad = parse 
| '-' (num as x)? { Some_padding (tr_prec x,Left) }
| '0' (num as x)? { Some_padding (tr_prec x,Zeros) }
| num as x        { Some_padding (tr_prec (Some x),Right) }
| ""              { No_padding }

and lexconv pad = parse
| 'c'  { Char }
| 's'  { String }
| 'f'  { Float }
| 'i'|'d' { Int (pad,Int_i,I) }
| 'x'  { Int (pad,Int_x,I) }
| 'u'  { Int (pad,Int_u,I) }
| "PCTR" { Int (pad,Int_u,CTR) }
(* 8 *)
|  ("PRIi8"|"PRId8")  { Int (pad,Int_i,I8) }
| "PRIx8" { Int (pad,Int_x,I8) }
| "PRIu8" { Int (pad,Int_u,I8) }
(* 16 *)
|  ("PRIi16"|"PRId16")  { Int (pad,Int_i,I16) }
| "PRIx16" { Int (pad,Int_x,I16) }
| "PRIu16" { Int (pad,Int_u,I16) }
(* 32 *)
|  ("PRIi32"|"PRId32")  { Int (pad,Int_i,I32) }
| "PRIx32" { Int (pad,Int_x,I32) }
| "PRIu32" { Int (pad,Int_u,I32) }
(* 64 *)
|("PRIi64"|"PRId64") { Int (pad,Int_i,I64) }
|"PRIx64" { Int (pad,Int_x,I64) }
|"PRIu64" { Int (pad,Int_u,I64) }
| "" { failwith ("lexFmt: lexconv") }

{
let lex s = main [] (Lexing.from_string s)
}
