(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Output a lexbuf *)

{
}

rule doecho put = parse
| _ as c { put c ; doecho put lexbuf }
| eof    { () }

{

let echo lexbuf chan = doecho (output_char chan) lexbuf

let get lexbuf =
  let buff = Buffer.create 32 in
  doecho (Buffer.add_char buff) lexbuf ;
  Buffer.contents buff

let echo_fun lexbuf put = doecho put lexbuf

}
