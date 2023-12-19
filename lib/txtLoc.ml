(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Locations for AST *)

type t = {
  loc_start : Lexing.position ;
  loc_end : Lexing.position ;
  loc_ghost : bool ;
  }

let in_file name =
  let loc = {
    Lexing.pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let make p1 p2 = { loc_start=p1; loc_end=p2; loc_ghost=false; }

let pp chan p = Pos.pp_pos2 chan (p.loc_start,p.loc_end)

module Extract() =
  struct

    let t = Hashtbl.create 13

    let read_chan chan =
      let len = in_channel_length chan in
      really_input_string chan len

    let read_fname fname =
      try Hashtbl.find t fname
      with Not_found ->
        try Misc.input_protect read_chan  fname
        with Sys_error msg ->
          Warn.fatal "Error %s, while attempting to read %s\n" msg fname

    let extract t =
      if t.loc_ghost then "** ?? **"
      else begin
        let open Lexing in
        let cts = read_fname t.loc_start.pos_fname in
        let n1 = t.loc_start.pos_cnum
        and n2 = t.loc_end.pos_cnum in
        try String.sub cts n1 (n2-n1)
        with Invalid_argument _ -> assert false
     end
end
