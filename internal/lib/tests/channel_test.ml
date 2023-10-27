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

(** Tests for the Channel module. *)

let pp_int_list = Test.pp_int_list
let pp_string_list = Test.pp_string_list

let tests = [
  "Channel.read_lines and Channel.write_lines", (fun () ->
    let in_fd, out_fd = Unix.pipe () in
    let in_ch, out_ch = Unix.in_channel_of_descr in_fd, Unix.out_channel_of_descr out_fd in

    let lines = ["mew"; "purr"] in
    Channel.write_lines out_ch lines ;
    close_out out_ch ;

    let actual = Channel.read_lines in_ch in
    close_in in_ch ;

    if Test.string_list_compare lines actual <> 0 then
      Test.fail (Printf.sprintf "Expected %s, got %s" (pp_string_list lines) (pp_string_list actual))
  );

  "Channel.map_lines applies f", (fun () ->
    let in_fd, out_fd = Unix.pipe () in
    let in_ch, out_ch = Unix.in_channel_of_descr in_fd, Unix.out_channel_of_descr out_fd in

    let lines = ["mew"; "purr"] in
    Channel.write_lines out_ch lines ;
    close_out out_ch ;

    let expected = [3; 4] in
    let actual = Channel.map_lines String.length in_ch in
    close_in in_ch ;

    if Test.int_list_compare expected actual <> 0 then
      Test.fail (Printf.sprintf "Expected %s, got %s" (pp_int_list expected) (pp_int_list actual))
  );
]

let () = Test.run tests
