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
    try Sys.remove name
    with e -> W.warn "remove failed: %s" (Printexc.to_string e)

  let extfile name_dot ext = (Filename.remove_extension name_dot) ^ "." ^ ext

  module G = Generator(O)
  let generator = G.generator

  let with_temp_file ?(keep = O.debug) name f =
    if keep then (
      if O.debug then
        Printf.eprintf "The file %s will not be removed.\n%!" name;
      f ())
    else (
      if O.debug then Printf.eprintf "Using tempfile %s\n%!" name;
      Handler.push (fun () -> my_remove name);
      f ();
      my_remove name;
      Handler.pop ())

  let suppress_stderr cmd =
    if O.debug then cmd else sprintf "(%s) 2>/dev/null" cmd

  let run_cmd fmt =
    ksprintf
      (fun cmd ->
         let r = Sys.command (suppress_stderr cmd) in
         if O.debug then eprintf "Command: [%s] -> %i\n%!" cmd r)
      fmt

  let do_show_file ?(keep_tmp_pdf = O.debug) name_dot prog ext : unit =
    let name_ps = extfile name_dot ext in
    with_temp_file ~keep:keep_tmp_pdf name_ps @@ fun () ->
    run_cmd "%s -T%s %s -o %s && %s %s" generator ext name_dot name_ps prog
      name_ps

  let show_file_with_view view name_dot : unit =
    let open View in
    match view with
    | GV -> do_show_file name_dot "gv" "ps"
    | Evince -> do_show_file name_dot "evince" "pdf"
    | Preview ->
      run_cmd "%s -Tpdf %s -O && open -a Preview %s.*pdf" generator name_dot
        name_dot

  let show_file name_dot =
    match O.view with None -> () | Some v -> show_file_with_view v name_dot

  let show ouput_dot =
    match O.view with
    | None -> ()
    | Some view ->
        let name_dot = Filename.temp_file "herd" ".dot" in
        with_temp_file name_dot @@ fun () ->
        Misc.output_protect ouput_dot name_dot;
        show_file_with_view view name_dot
end
