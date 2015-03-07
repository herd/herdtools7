(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Gabriel Kerneis, University of Cambridge, UK.              *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


(*****************************)
(* Very simple pre-processor *)
(*****************************)
{
open Printf

let sz = 2048
let dump_line name no = printf "# %i \"%s\"\n" no name

let _echo name =
  let fname =
    if Sys.file_exists name then name
    else Filename.concat ".." name in
  dump_line name 1 ;
  Misc.input_protect
    (fun chan ->
      let buff = Bytes.create sz in
      let rec do_rec () =
        let n = input chan buff 0 sz in
        if n > 0 then begin
          output stdout buff 0 n ;
          do_rec ()
        end in
      do_rec ())
    fname

let echo tr name =
  let fname =
    if Sys.file_exists name then name
    else Filename.concat ".." name in
  dump_line name 1 ;
  Misc.input_protect
    (fun chan -> tr (Lexing.from_channel chan))
    fname
}

  rule main name no = parse 
| [^'\n']* "#include" ' '* '"' ([^'"']* as fname) '"' [^'\n']* '\n'
(* '"' *) 
    { echo tr fname ;
      dump_line name (no+1) ;
      main name (no+1) lexbuf }
|  [^'\n']* '\n' as lxm
    { output_string stdout lxm ; main name (no+1) lexbuf }
|  ([^'\n']* as lxm) eof {  output_string stdout lxm }

and tr = parse
| "AArch64Base" { output_string stdout "AArch64GenBase" ; tr lexbuf }
| _ as lxm { output_char stdout lxm ; tr lexbuf }
| eof { () }
    
{
 let name = Sys.argv.(1) 

let () =
  Misc.input_protect
    (fun chan ->
      printf "# 1 \"%s\"\n" name ; 
      main name 1 (Lexing.from_channel chan))
    name
   }
