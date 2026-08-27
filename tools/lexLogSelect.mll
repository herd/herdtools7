(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extract records from herd/litmus/msum logs *)

(*
 * The extraction is a bit complicated by the variety of log formats...
 *  + Fortunately all records starts with `Test `<name>, where <name>
 *    is the name of the test.
 *  + Unfortunately, the format of record end differ:
 *     - The records produced by herd and msum end at the first empty line.
 *     - The records produced by litmus include an empty line that'
 *       follows the validation tag (Ok/No). The end at the "Time"
 *       information that always follow the Hash metadata.
 * The output of mlogselect adopts the  convention of herd and msum.
 *)
{
open LexMisc

let outline = print_endline

type st =  { inside:bool; hash_seen:bool; }
let st_false = { inside=false; hash_seen=false; }
let st_true = { inside=true; hash_seen=false; }
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name = (alpha|'_'|'.'|'$') (alpha|digit|'_'| '.')*
let blank = [' ' '\t']
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+' | '[' | ']' | ':')+
let nl = '\r'?'\n'
let validation = "Ok"|"No"

rule main ok st = parse
| "Test" blank+ (testname as t)
  (blank+ name)? as line nl
  { incr_lineno lexbuf ;
    let st = if ok t then st_true else st_false in
    if st.inside then outline line ;
    main ok st lexbuf }
| nl (* An empty line ends one test log in herd/msum logs *)
  { incr_lineno lexbuf ;
    if st.inside then  outline "" ;
    main ok st_false lexbuf }
| validation as line nl nl
(* Unless the empty line follows a validation tag,
   as in some litmus logs *)
  { incr_lineno lexbuf ; incr_lineno lexbuf ;
    if st.inside then outline line ;
    main ok st lexbuf }
| ['h''H']['a''A']['s''S']['h''H']
    blank* '=' blank* [^' ''\t''\n''\r']+ blank*  as line nl
(* Detect hash metadata *)
  { incr_lineno lexbuf ;
    if st.inside then outline line ;
    main ok {st with hash_seen=true; } lexbuf }
| "Time" blank+ testname blank+ (digit|'.')+ blank* as line nl
(* Detect timing information that ends records in litmus logs *)
  { incr_lineno lexbuf ;
    if st.inside then outline line ;
    if st.hash_seen then begin (* End of record, if follows hash metadata *)
      if st.inside then outline "" ; (* Add empty line *)
      main ok st_false lexbuf
    end else
      main ok st lexbuf }
| [^'\n''\r']+ as line nl
  { incr_lineno lexbuf ;
    if st.inside then outline line ;
    main ok st lexbuf }
|  [^'\n''\r']* as line eof
  { if st.inside && line <> "" then outline line }

{

let from_chan ok chan = main ok st_false @@ Lexing.from_channel chan

}
