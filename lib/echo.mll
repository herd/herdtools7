(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
