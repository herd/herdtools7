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
          failwith (Printf.sprintf "Expected %s, got %s" (pp_string_list lines) (pp_string_list actual))
      )
      tests
  );
]

let () = Test.run tests
