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

open Lexing

type pos2 = position * position

let debug_pos chan pos =
  Printf.fprintf chan
    "{fname=\"%s\", lnum=%i, bol=%i, cnum=%i}"
    pos.pos_fname
    pos.pos_lnum
    pos.pos_bol
    pos.pos_cnum


let pp_pos0 chan filename =
  Printf.fprintf chan  "File \"%s\", " filename

let str_pos0  filename =
  Printf.sprintf "File \"%s\", " filename

let pp_pos chan pos =
  Printf.fprintf chan
    "File \"%s\", line %i, character %i"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let pp_lnum chan pos =
  Printf.fprintf chan
    "File \"%s\", line %i"
    pos.pos_fname
    pos.pos_lnum

let pp_pos2 chan (pos1,pos2) =
  Printf.fprintf chan
    "File \"%s\", line %i, characters %i-%i"
    pos1.pos_fname
    pos1.pos_lnum
    (pos1.pos_cnum - pos1.pos_bol)
    (pos2.pos_cnum - pos1.pos_bol)
