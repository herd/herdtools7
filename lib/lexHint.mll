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
open Lexing
open LexMisc

let fconcat d1 d2 = match d1 with
| "." -> d2
| _ ->
    if Filename.is_relative d2 then
      Filename.concat d1 d2
    else
      d2

let do_include lex fname =
  Misc.input_protect 
    (fun chan ->
      try
        let lexbuf = Lexing.from_channel chan in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname;};
        lex lexbuf
      with Error (msg,pos) ->
        Printf.eprintf
	"%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
      raise Misc.Exit)
    fname

}

let space = [' ''\t']
let non_space = [^' ''\t''\n']
let non_space_eq = [^'='' ''\t''\n']

rule main dir get acc = parse
| "#include" space+ (non_space+ as fname) '\n'
    {
     let dir0 = Filename.dirname fname in
     let d = fconcat dir dir0 in
     let n = fconcat dir fname in
     let acc = do_include (main d get acc) n in
     incr_lineno lexbuf;
     main dir get acc lexbuf
   }
| '#' [^'\n']* '\n'
   { incr_lineno lexbuf; main dir get acc lexbuf }
  
| (non_space_eq+ as name) space+
  (non_space_eq+ as key) (space+|(space* '=' space*))
  ("" | (non_space_eq [^'\n']+) as v)
  '\n'
{ incr_lineno lexbuf; main dir get (get acc name key v) lexbuf }

| eof { acc }

| "" { error "lexHint" lexbuf }

{
 let read fname get acc =
   let dir = Filename.dirname fname in
   do_include (main dir get acc) fname
}
