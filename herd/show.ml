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

  let psfile name_dot =
    let base =
      try Filename.chop_extension name_dot
      with Invalid_argument _ -> name_dot in
    base ^ ".ps"
             
  module G = Generator(O)
  let generator = G.generator

  let do_show_file name_dot prog =    
    let name_ps = psfile name_dot in
    Handler.push (fun () -> my_remove name_ps) ;
    let cmd =
      sprintf
        "%s -Tps %s > %s 2>/dev/null ; %s %s 2>/dev/null%s"
      generator
      name_dot name_ps prog name_ps
        (if O.debug then "" else sprintf " && /bin/rm -f %s" name_ps) in
    let r = Sys.command cmd in
    if O.debug then eprintf "Command: [%s] -> %i\n" cmd r ;
    Handler.pop ()

let show_file name_dot =
  if O.gv     then do_show_file name_dot "gv"     else
  if O.evince then do_show_file name_dot "evince" else ()

let show ouput_dot =
  (* Perhaps this function needs some work doing, as it
     currently doesn't work with evince, only with gv *)
  if O.gv then begin
    let name_dot = Filename.temp_file "herd" ".dot" in
    Handler.push (fun () -> my_remove name_dot) ;
    Misc.output_protect ouput_dot name_dot ;
    do_show_file name_dot "gv"; 
    my_remove name_dot ;
    Handler.pop ()
  end

end
