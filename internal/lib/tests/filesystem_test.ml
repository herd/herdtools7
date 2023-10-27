(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Filesystem and file utilities. *)

let pp_string_list = Test.pp_string_list

let tests = [
  "Filesystem.read_file and Filesystem.write_file", (fun () ->
    let tests = [
      [] ;
      ["foo"; "bar"] ;
    ] in

    List.iter (fun lines ->
        let path = Filename.temp_file "" "" in

        Filesystem.write_file path (fun o -> Channel.write_lines o lines) ;

        let actual = Filesystem.read_file path Channel.read_lines in

        (* Clean up. *)
        Sys.remove path ;

        if Test.string_list_compare lines actual <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" (pp_string_list lines) (pp_string_list actual))
      )
      tests
  );

  "Filesystem.new_temp_dir creates directories", (fun () ->
    let path = Filesystem.new_temp_dir () in

    if not (Sys.is_directory path) then
      Test.fail (Printf.sprintf "path %s is not a directory" path)
    else
      (* Cleanup. *)
      Unix.rmdir path
  );
  "Filesystem.new_temp_dir creates unique paths", (fun () ->
    let path1 = Filesystem.new_temp_dir () in
    let path2 = Filesystem.new_temp_dir () in

    if String.compare path1 path2 = 0 then
      Test.fail (Printf.sprintf "paths are the same (%s)" path1)
    else begin
      (* Cleanup. *)
      Unix.rmdir path1 ;
      Unix.rmdir path2
    end
  );

  "Filesystem.remove_recursive does not raise when removing nothing", (fun () ->
    (* Generate a random temporary name that does not exist. *)
    let tmp_file = Filename.temp_file "" "" in
    Sys.remove tmp_file ;

    Filesystem.remove_recursive tmp_file
  );
  "Filesystem.remove_recursive removes a single file", (fun () ->
    let tmp_file = Filename.temp_file "" "" in
    Filesystem.remove_recursive tmp_file ;
    if Sys.file_exists tmp_file then
      Test.fail "File not removed"
  );
  "Filesystem.remove_recursive removes an empty directory", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    Filesystem.remove_recursive tmp_dir ;
    if Sys.file_exists tmp_dir then
      Test.fail "Directory not removed"
  );
  "Filesystem.remove_recursive removes a directory of files", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let touch name = Filesystem.write_file (Filename.concat tmp_dir name) (fun _ -> ()) in
    touch "mew" ;
    touch "purr" ;

    Filesystem.remove_recursive tmp_dir ;

    if Sys.file_exists tmp_dir then
      Test.fail "Directory not removed"
  );
  "Filesystem.remove_recursive removes nested a directory of files", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let mkdir name = Unix.mkdir (Filename.concat tmp_dir name) 0o700 in
    let touch name = Filesystem.write_file (Filename.concat tmp_dir name) (fun _ -> ()) in
    touch "mew" ;
    mkdir "purr" ;
    touch "purr/meow" ;

    Filesystem.remove_recursive tmp_dir ;

    if Sys.file_exists tmp_dir then
      Test.fail "Directory not removed"
  );
]

let () = Test.run tests
