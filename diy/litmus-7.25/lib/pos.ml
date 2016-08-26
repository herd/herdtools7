(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
  Printf.fprintf chan  "File \"%s\"" filename

let str_pos0  filename =
  Printf.sprintf "File \"%s\"" filename

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
