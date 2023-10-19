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

(************************************************************)
(* Show feature, for debug, pops up a window to show graphs *)
(************************************************************)

open Printf

module Generator(O:PrettyConf.S) = struct
  let generator = match O.dotcom with
  | None ->
      begin match O.graph with
      | Graph.Columns -> "neato"
      | Graph.Free|Graph.Cluster -> "dot"
      end
  | Some com -> PrettyConf.pp_dotcom com
end

module Make(O:PrettyConf.S) = struct

  module W = Warn.Make(O)

  let my_remove name =
    if not O.debug then
      try Sys.remove name
      with e ->
        W.warn "remove failed: %s" (Printexc.to_string e)

  let extfile name_dot ext =
    let base =
      try Filename.chop_extension name_dot
      with Invalid_argument _ -> name_dot in
    base ^ "." ^ ext

  module G = Generator(O)
  let generator = G.generator

  let do_show_file name_dot prog ext =
    let name_ps = extfile name_dot ext in
    Handler.push (fun () -> my_remove name_ps) ;
    let cmd =
      sprintf
        "%s -T%s %s > %s 2>/dev/null ; %s %s 2>/dev/null%s"
      generator ext
      name_dot name_ps prog name_ps
        (if O.debug then "" else sprintf " && /bin/rm -f %s" name_ps) in
    let r = Sys.command cmd in
    if O.debug then eprintf "Command: [%s] -> %i\n" cmd r ;
    Handler.pop ()

let show_file_with_view view name_dot =
  let open View in
  match view with
  | GV -> do_show_file name_dot "gv"  "ps"
  | Evince -> do_show_file name_dot "evince" "pdf"
  | Preview -> do_show_file name_dot "open -a Preview" "pdf"

let show_file name_dot =
  match O.view with
  | None -> ()
  | Some v -> show_file_with_view v name_dot

let show ouput_dot =
  match O.view with
  | None -> ()
  | Some view ->
     let name_dot = Filename.temp_file "herd" ".dot" in
     Handler.push (fun () -> my_remove name_dot) ;
     Misc.output_protect ouput_dot name_dot ;
     show_file_with_view view name_dot ;
     my_remove name_dot ;
     Handler.pop ()

end
