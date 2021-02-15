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

(*Test runs output two kinds of information, we should check both *)
type stdout_lines = string list
type stderr_lines = string list

let time_re = Str.regexp "^Time "
let without_unstable_lines lines =
  let not_time l = not (Str.string_match time_re l 0) in
  List.filter not_time lines

let log_compare a b = String.compare (String.concat "\n" a) (String.concat "\n" b)

let herd_args ~bell ~cat ~variants libdir =
  let bells =
    match bell with
    | None -> []
    | Some bell -> ["-bell"; bell]
  in
  let cats =
    match cat with
    | None -> []
    | Some cat -> ["-cat"; cat; "-I"; Filename.dirname cat]
  in
  let variants =
    List.concat (List.map (fun v -> ["-variant"; v]) variants)
  in
  let libdirs = ["-set-libdir"; libdir] in
  List.concat [bells; cats; variants; libdirs]

let herd_command ?bell ?cat ?(variants = []) herd libdir litmuses =
  let args = herd_args ~bell:bell ~cat:cat ~variants:variants libdir in
  Command.command herd (args @ litmuses)

let run_herd ?bell ?cat ?(variants = []) herd libdir litmuses =
  let args = herd_args ~bell:bell ~cat:cat ~variants:variants libdir in
  let litmuses o = Channel.write_lines o litmuses ; close_out o in

  (* Record stdout and stderr to two sources if we need to reason about them separately *)
  let lines = ref [] in
  let err_lines = ref [] in
  let read_lines c = lines := Channel.read_lines c in
  let read_err_lines c = err_lines := Channel.read_lines c in
  Command.run ~stdin:litmuses ~stdout:read_lines ~stderr:read_err_lines herd args ;
  (without_unstable_lines !lines, !err_lines)

let herd_output_matches_expected herd libdir litmus expected expected_failure =
  try
    match run_herd herd libdir [litmus] with
    | [],[] ->
      Printf.printf "Failed %s : Herd finished but returned no output or errors\n" litmus ; false
    | stdout, [] -> (* Herd finished without errors - normal *)
      begin
        let expected_output = try
          Some (Filesystem.read_file expected Channel.read_lines)
        with _ -> None in
        match expected_output with
        | None -> Printf.printf "Failed %s : Missing file %s\n" litmus expected ; false
        | Some expected_output ->

        if log_compare stdout expected_output <> 0 then begin
          Printf.printf "Failed %s : Logs do not match\n" litmus ;
          false
        end else
          true
      end

    | [], stderr -> (* Herd finished with errors - check expected failure *)
      begin
        let expected_failure_output = try
          Some (Filesystem.read_file expected_failure Channel.read_lines)
        with _ -> None in
        match expected_failure_output with
        | None -> Printf.printf "Failed %s : Missing file %s\n" litmus expected_failure ; false
        | Some expected_failure_output ->

        if log_compare stderr expected_failure_output <> 0 then begin
          Printf.printf "Failed %s : Expected Failure Logs do not match\n" litmus ;
          false
        end else
          true
      end

    | _,_ -> (* Herd returned both output and errors *)
      Printf.printf "Failed %s : %s and %s exist, only one expected\n" litmus expected expected_failure ; false
  with
    Command.Error e -> Printf.printf "Failed %s : %s \n" litmus (Command.string_of_error e) ; false


let is_litmus path = Filename.check_suffix path ".litmus"
let is_expected path = Filename.check_suffix path ".litmus.expected"

let expected_of_litmus litmus = litmus ^ ".expected"
let litmus_of_expected expected = Filename.chop_suffix expected ".expected"

let expected_failure_of_litmus litmus = litmus ^ ".expected-failure"
let litmus_of_expected_failure expected = Filename.chop_suffix expected ".expected-failure"
