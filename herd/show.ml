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

(************************************************************)
(* Show feature, for debug, pops up a window to show graphs *)
(************************************************************)

module Make(O:PrettyConf.S) = struct

  module W = Warn.Make(O)

  let my_remove name =
    try Sys.remove name
    with e ->
      W.warn "remove failed: %s" (Printexc.to_string e)

  let psfile name_dot =
    let base = Filename.chop_extension name_dot in
    base ^ ".ps"
             

  let generator= match O.dotcom with
  | None ->
      begin match O.graph with
      | Graph.Columns -> "neato"
      | Graph.Free|Graph.Cluster -> "dot"
      end
  | Some com -> PrettyConf.pp_dotcom com

  let do_show_file name_dot prog =    
    let name_ps = psfile name_dot in
    Handler.push (fun () -> my_remove name_ps) ;
    let cmd =
      Printf.sprintf
        "%s -Tps %s > %s 2>/dev/null && %s %s 2>/dev/null && /bin/rm -f %s"
        generator
      name_dot name_ps prog name_ps name_ps in
    ignore (Sys.command cmd) ;
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
