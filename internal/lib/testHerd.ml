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

let time_re = Str.regexp "^Time "
let without_unstable_lines lines =
  let not_time l = not (Str.string_match time_re l 0) in
  List.filter not_time lines

let log_compare a b = String.compare (String.concat "\n" a) (String.concat "\n" b)


let herd_command herd libdir litmus =
  Command.command herd ["-set-libdir"; libdir; litmus]

let run_herd herd libdir litmus =
  let lines = Command.run_with_stdout herd ["-set-libdir"; libdir; litmus] Channel.read_lines in
  without_unstable_lines lines

let herd_output_matches_expected herd libdir litmus expected =
  let output = try
    Some (run_herd herd libdir litmus)
  with _ -> None in
  match output with
  | None -> Printf.printf "Failed %s : Herd returned non-zero\n" litmus ; false
  | Some output ->

  let expected_output = try
    Some (Filesystem.read_file expected Channel.read_lines)
  with _ -> None in
  match expected_output with
  | None -> Printf.printf "Failed %s : Missing file %s\n" litmus expected ; false
  | Some expected_output ->

  if log_compare output expected_output <> 0 then begin
    Printf.printf "Failed %s : Logs do not match\n" litmus ;
    false
  end else
    true


let is_litmus path = Filename.check_suffix path ".litmus"
let is_expected path = Filename.check_suffix path ".litmus.expected"

let expected_of_litmus litmus = litmus ^ ".expected"
let litmus_of_expected expected = Filename.chop_suffix expected ".expected"
