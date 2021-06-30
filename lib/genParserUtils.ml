(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***********************************)
(* Utilities for 'generic' parsers *)
(***********************************)

open Lexing

let call_parser name lexbuf lex parse =
  try parse lex lexbuf
  with
  | LexMisc.Error (msg,pos) ->
      Warn.user_error "%s: Lex error %s (in %s)" (Pos.str_pos pos) msg name
  | Parsing.Parse_error ->
      let lxm = lexeme lexbuf
      and start_loc = lexeme_start_p lexbuf
      and end_loc = lexeme_end_p lexbuf in
      Warn.user_error "%s: unexpected '%s' (in %s)" (Pos.str_pos2 (start_loc, end_loc)) lxm name
  | Misc.UserError msg ->
      let start_loc = lexeme_start_p lexbuf
      and end_loc = lexeme_end_p lexbuf in
      Warn.user_error "%s: %s (in %s)" (Pos.str_pos2 (start_loc, end_loc)) msg name
  | e ->
      Printf.eprintf
        "%a: Uncaught exception %s (in %s)\n"
        Pos.pp_pos lexbuf.lex_curr_p
        (Printexc.to_string e) name ;
      assert false


(************************)
(* Various basic checks *)
(************************)

    let check_one_proc procs p =
      if not (List.mem p procs) then
        Warn.fatal "Bad process P%i" p

    let check_loc procs loc = match loc with
      | MiscParser.Location_reg (p,_) -> check_one_proc procs p
      | _ -> ()

    let check_rloc procs rloc =
      let open ConstrGen in
      match rloc with
      | Loc loc|Deref (loc,_) -> check_loc procs loc

    let check_atom procs a =
      let open ConstrGen in
      match a with
      | LV (loc,_) -> check_rloc procs loc
      | LL (l1,l2) -> check_loc procs l1 ; check_loc procs l2
      | FF ((p,_),_) -> check_one_proc procs p

    let check_regs procs init locs final =
      List.iter (fun (loc,_) -> check_loc procs  loc) init ;
      List.iter (LocationsItem.iter_loc (check_loc procs)) locs ;
      ConstrGen.fold_constr (fun a () -> check_atom procs a) final ()

(* Extract locations final items *)
    let get_locs_atom a =
      let open ConstrGen in
      let open MiscParser in
      match a with
      | LV (loc,_) -> RLocSet.add loc
      | LL (loc1,loc2) ->
          (fun k -> RLocSet.add (Loc loc1) (RLocSet.add (Loc loc2) k))
      | FF (_,x) -> RLocSet.add (Loc (MiscParser.Location_global x))

    let get_visible_locs locs c =
      MiscParser.RLocSet.union
        (MiscParser.RLocSet.of_list
           (List.fold_right
              (let open LocationsItem in
               fun item k ->
               match item with
               | Loc (rloc,_) -> rloc::k
               | Fault _ -> k)
              locs []))
        (ConstrGen.fold_constr get_locs_atom c MiscParser.RLocSet.empty)
