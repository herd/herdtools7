(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Find header files *)

{
open Printf
let verbose = false
}

rule main = parse
| ' '* '\n' { main lexbuf }
| "%{" { main lexbuf }
| "(*"|"/*"   as lxm
    { if verbose then eprintf "%s" lxm ; inside false false lexbuf }
| "" { false }


and inside inria cecill = parse
| "CeCILL-B" as lxm
 { if verbose then eprintf "%s" lxm ; inside inria true lexbuf }
| ('i'|'I')('n'|'N')('r'|'R')('i'|'I')('a'|'A') as lxm
 { if verbose then eprintf "%s" lxm ; inside true cecill lexbuf }
| "*)"|"*/" as lxm { if verbose then eprintf "%s\n" lxm ; outside inria cecill lexbuf }
| _   as lxm { if verbose then eprintf "%c" lxm ; inside inria cecill lexbuf }

and outside inria cecill = parse
| ' '* '\n'         { outside inria cecill lexbuf }
| "(*"|"/*" as lxm  { if verbose then eprintf "%s" lxm ; inside inria cecill lexbuf }
| "" { inria && cecill }

{

let zyva chan = main (Lexing.from_channel chan)

let zyva fname =  Misc.input_protect zyva fname

let () =
  for i = 1 to Array.length Sys.argv-1 do
    let fname = Sys.argv.(i) in
    if not (zyva fname) then printf "%s\n" fname
  done
}
