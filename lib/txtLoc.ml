(*********************************************************************)
(*                        diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
  { loc_start = loc; loc_end = loc; loc_ghost = false }
;;

let none = let loc = in_file "_none_" in { loc with loc_ghost = true;}

let make p1 p2 = { loc_start=p1; loc_end=p2; loc_ghost=false; }

let pp chan p = 
  if p.loc_ghost then Printf.fprintf chan "File _none_"
  else
    Pos.pp_pos2 chan (p.loc_start,p.loc_end)


