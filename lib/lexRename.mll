(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
open Printf
open Lexing
open LexMisc

exception Error

module type Config  = sig
  val verbose : int
end

module Make(O:Config) = struct
}

let blank = [' ''\t']
let name = [^' ''\t''\n' '%' '#''"']+
let comment = ('#'|'%') [^'\n']*
rule main parse_value t idx = parse 
| blank* (name as key)
 (blank+
 ((name as value_pp)|('"' ([^'"']* as value_pp) '"'))  (* '"' *)
 |("" as value_pp))
blank* comment? '\n'
 {
  let value = match parse_value value_pp with
  | None -> error (sprintf "%s is not a valid value" value_pp) lexbuf
  | Some v -> v in
  if O.verbose > 1 then
    eprintf "LexRename: %s -> (%s,%d)\n" key value_pp idx ;
  let t = TblRename.add_binding t key idx value in
  incr_lineno lexbuf ;
  main parse_value t (idx+1) lexbuf
  }
| (comment | blank*) '\n'
  { incr_lineno lexbuf ; main parse_value t idx lexbuf }
| eof { idx,t }
| "" { error "LexRename" lexbuf }

{

let read_from idx fname chan t parse_value =
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <-
    {pos_fname = fname; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
  try
    main  parse_value t idx lexbuf
  with LexMisc.Error (msg,loc) ->    
    Printf.eprintf "%a: error in rename map, %s\n"
      Pos.pp_pos loc msg ;
    raise Misc.Exit (* silent, message printed above *)

let read_from_files fnames parse_value =
  let _,tbl  =
    List.fold_right
      (fun name (idx,t) ->
        try
          Misc.input_protect
            (fun chan -> read_from idx name chan t parse_value)
            name
        with Exit -> raise Error)
      fnames (0,TblRename.empty) in
  tbl

let read_from_file fname  = read_from_files [fname]

let read  fname chan t parse_value =
  let _,r = read_from 0  fname chan t parse_value in
  r
end
}
