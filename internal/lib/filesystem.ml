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

(** Filesystem and file utilities. *)

let read_file path f =
  let chan = open_in path in
  let ret = try
    f chan
  with e -> begin
    close_in chan ;
    raise e
  end in
  close_in chan ; ret

let write_file path f =
  let chan = open_out path in
  let ret = try
    f chan
  with e -> begin
    close_out chan ;
    raise e
  end in
  close_out chan ; ret

let rec remove_recursive path =
  if Sys.file_exists path then begin
    if Sys.is_directory path then begin
      let children = Array.to_list (Sys.readdir path) in
      let child_paths = List.map (Filename.concat path) children in
      List.iter remove_recursive child_paths ;
      Unix.rmdir path
    end else
      Sys.remove path
  end

let new_temp_dir () =
  let path = ref "" in
  Command.run ~stdout:(fun c -> path := input_line c) "mktemp" ["-d"] ;
  !path
