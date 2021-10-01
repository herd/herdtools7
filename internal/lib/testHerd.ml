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

(* Utilities for running Herd binaries in tests. *)

type path = string

type stdout_lines = string list
type stderr_lines = string list

let time_re = Str.regexp "^Time "
let without_unstable_lines lines =
  let not_time l = not (Str.string_match time_re l 0) in
  List.filter not_time lines

let log_compare a b = String.compare (String.concat "\n" a) (String.concat "\n" b)

let herd_args ~bell ~cat ~conf ~variants ~libdir =
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
  let confs =
    match conf with
    | None -> []
    | Some conf -> ["-conf"; conf]
  in
  let variants =
    List.concat (List.map (fun v -> ["-variant"; v]) variants)
  in
  let libdirs = ["-set-libdir"; libdir] in
  List.concat [["-exit"; "true";]; bells; cats; confs; variants; libdirs]

let apply_args herd j herd_args =
  let herd_args = String.concat "," herd_args in
  ["-com"; herd; "-j" ; Printf.sprintf "%i" j; "-comargs"; herd_args;]

let herd_command ~bell ~cat ~conf ~variants ~libdir herd ?j litmuses =
  let args = herd_args ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir in
  match j with
  | None ->
     Command.command herd (args @ litmuses)
  | Some j ->
     let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
     let args = apply_args  herd j args in
     Command.command mapply  (args @ litmuses)

let run_herd ~bell ~cat ~conf ~variants ~libdir herd ?j litmuses =
  let args = herd_args ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir in
  let litmuses o = Channel.write_lines o litmuses ; close_out o in

  (* Record stdout and stderr to two sources if we need to reason about them separately *)
  let lines = ref [] in
  let err_lines = ref [] in
  let read_lines c = lines := Channel.read_lines c in
  let read_err_lines c = err_lines := Channel.read_lines c in
  let r =
    match j with
    | None ->
       Command.run_status
         ~stdin:litmuses ~stdout:read_lines ~stderr:read_err_lines herd args
    | Some j ->
       let j = max 2 j in
       let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
       let args = apply_args herd j args in
       Command.run_status
         ~stdin:litmuses ~stdout:read_lines ~stderr:read_err_lines mapply args in
  (r,without_unstable_lines !lines, !err_lines)

let read_some_file litmus name =
  try Some (Filesystem.read_file name Channel.read_lines)
  with _ ->
    begin
      Printf.printf "Failed %s : Missing file %s\n" litmus name ;
      None
    end

let herd_output_matches_expected ~bell ~cat ~conf ~variants ~libdir herd litmus expected expected_failure expected_warn =
  try
    match run_herd ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir herd  [litmus] with
    | _,[],[] ->
      Printf.printf "Failed %s : Herd finished but returned no output or errors\n" litmus ; false
    | 0,(_::_ as stdout), [] -> (* Herd finished without errors - normal *)
       begin
         match read_some_file litmus expected with
         | None -> false
         | Some expected_output ->
            if log_compare stdout expected_output <> 0 then begin
              Printf.printf "Failed %s : Logs do not match\n" litmus ;
              false
            end else true
       end

    | r,[], (_::_ as stderr) when r <> 0 -> (* Herd finished with errors - check expected failure *)
       begin
         match read_some_file litmus expected_failure with
         | None -> false
         | Some expected_failure_output ->
            if log_compare stderr expected_failure_output <> 0 then begin
              Printf.printf
                  "Failed %s : Expected Failure Logs do not match\n" litmus ;
              false
            end else true
       end
    | 0,(_::_ as stdout),(_::_ as stderr) ->
       (* Herd returned both output and errors *)
        begin
         match read_some_file litmus expected with
         | None -> false
         | Some expected_output ->
             if log_compare stdout expected_output <> 0 then begin
              Printf.printf "Failed %s : Logs do not match\n" litmus ;
              false
            end else
              match read_some_file litmus expected_warn with
              | None -> false
              | Some expected_warn ->
                 if log_compare stderr expected_warn <> 0 then begin
                   Printf.printf
                     "Failed %s : Warning logs do not match\n" litmus ;
                     false
                   end else true
        end
    | r,stdout,stderr ->
       let some f =
         match f with
         | [] -> "no"
         | _::_ -> "some" in
       Printf.printf
         "Failed %s : unexpeced exit code %i, %s output %s error.\n"
         litmus r (some stdout) (some stderr) ;
       false
  with
  | Command.Error e ->
     Printf.printf "Failed %s : %s \n" litmus
       (Command.string_of_error e) ; false


let is_litmus path = Filename.check_suffix path ".litmus"
let is_expected path = Filename.check_suffix path ".litmus.expected"

let expected_of_litmus litmus = litmus ^ ".expected"
let litmus_of_expected expected = Filename.chop_suffix expected ".expected"

let expected_failure_of_litmus litmus = litmus ^ ".expected-failure"
let litmus_of_expected_failure expected = Filename.chop_suffix expected ".expected-failure"

let expected_warn_of_litmus litmus = litmus ^ ".expected-warn"
