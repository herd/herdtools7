(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs herd and compares its output against reference files *)

let () =
  if false then
    let xs = Array.to_list Sys.argv in
    Printf.eprintf "%s\n%!" (String.concat " " xs)

let litmus = Sys.argv.(Array.length Sys.argv -1)

type flags = { verbose:bool; nohash:bool; check:TestHerd.check; }
let noflags = { verbose=false; nohash=false; check=TestHerd.All; }

let args_own, com, args = Args.split_wrapper_args (Array.to_list Sys.argv)

let flags =
  let rec gather_args flags args =
    match args with
    | [] ->
        flags
    | "-verbose" :: args ->
        gather_args {flags with verbose=true} args
    | "-checkstates" :: args ->
        gather_args {flags with check=TestHerd.Sta} args
    | "-checkobs" :: args ->
        gather_args {flags with check=TestHerd.Obs} args
    | "-nohash" :: args ->
        gather_args {flags with nohash=true} args
    | _ :: args ->
        gather_args flags args
  in
  gather_args noflags args_own

let () =
  let expected = TestHerd.expected_of_litmus litmus
  and expected_failure = TestHerd.expected_failure_of_litmus litmus
  and expected_warn = TestHerd.expected_warn_of_litmus litmus in
  if
    TestHerd.herd_args_output_matches_expected
      ~verbose:flags.verbose ~check:flags.check com ~nohash:flags.nohash args litmus
      expected expected_failure expected_warn
  then
    exit 0
  else begin
    let () =
      if false then
        Printf.printf "Test not ok: %s %s\n%!"
          (String.concat " " (com::args)) litmus in
    exit 1
  end
