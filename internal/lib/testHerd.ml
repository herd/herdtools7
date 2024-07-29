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

(* Utilities for running Herd binaries in tests. *)

type path = string

type stdout_lines = string list
type stderr_lines = string list

let outname l = l ^ ".out"
and errname l = l ^ ".err"

let read_file name =
  if Sys.file_exists name then
    Filesystem.read_file name Channel.read_lines
  else []


let time_re = Str.regexp "^Time "

let is_stable line = not (Str.string_match time_re line 0)

let without_unstable_lines lines = List.filter is_stable lines

let log_compare a b = String.compare (String.concat "\n" a) (String.concat "\n" b)

let herd_args  ~bell ~cat ~conf ~variants ~libdir ~timeout =
  let timeout =
    match timeout with
    | None -> []
    | Some t -> ["-timeout"; Printf.sprintf "%.2f" t;] in
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
  List.concat [["-exit"; "true";]; timeout; bells; cats; confs; variants; libdirs]

let apply_args herd j herd_args =
  let herd_args = String.concat "," herd_args in
  ["-com"; herd; "-j" ; Printf.sprintf "%i" j; "-comargs"; herd_args;]

let apply_redirect_args herd j herd_args =
  let redirect_args = String.concat "," (herd::herd_args) in
  let redirect =
    Filename.concat (Filename.dirname Sys.argv.(0))
      "herd_redirect.exe" in
  ["-com"; redirect; "-j"; Printf.sprintf "%i" j; "-comargs"; redirect_args;]

let herd_command
      ~bell ~cat ~conf ~variants ~libdir herd ?j ?timeout litmuses =
  let args =
    herd_args
      ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir
      ~timeout:timeout in
  match j with
  | None ->
     Command.command herd (args @ litmuses)
  | Some j ->
     let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
     let args = apply_args  herd j args in
     Command.command mapply  (args @ litmuses)

let do_run_herd_args herd args ?j litmuses =
  let litmuses = Base.Iter.of_list litmuses in
  (*
   * Record stdout and stderr to two sources if we need
   * to reason about them separately.
   *)
  let lines = ref [] in
  let err_lines = ref [] in
  let read_line line = lines := line :: !lines in
  let read_err_line line = err_lines := line :: !err_lines in
  let r =
    match j with
    | None ->
       Command.NonBlock.run_status
         ~stdin:litmuses ~stdout:read_line ~stderr:read_err_line herd args
    | Some j ->
       let j = max 2 j in
       let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
       let args = apply_args herd j args in
       Command.NonBlock.run_status
         ~stdin:litmuses ~stdout:read_line ~stderr:read_err_line mapply args in
  (r,without_unstable_lines (List.rev !lines), (List.rev !err_lines))

let run_herd_args herd args litmus = do_run_herd_args herd args [litmus]

let run_herd ~bell ~cat ~conf ~variants ~libdir herd ?j ?timeout litmuses =
  let args =
    herd_args
      ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir
      ~timeout:timeout in
  do_run_herd_args herd args ?j litmuses

let run_herd_concurrent ~bell ~cat ~conf ~variants ~libdir herd ~j litmuses =
  let args =
    herd_args
      ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir ~timeout:None in
  let litmuses = Base.Iter.of_list litmuses in
  let j = max 2 j in
  let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
  let args = apply_redirect_args herd j args in
  let r = Command.NonBlock.run_status ~stdin:litmuses  mapply args in
  r

let read_some_file litmus name =
  try Some (Filesystem.read_file name Channel.read_lines)
  with _ ->
    begin
      Printf.printf "Failed %s : Missing file %s\n" litmus name ;
      None
    end

let do_check_output litmus expected  expected_failure expected_warn t =
  let () =
    let _,lines,_ = t in
    if false && lines <> [] then begin
      Printf.eprintf "Expected %s, Out of test:\n" expected ;
      List.iter prerr_endline lines
    end in

  match t with
    | _,[],[] ->
      Printf.printf "Failed %s : Herd finished but returned no output or errors\n" litmus ; false
    | 0,(_::_ as stdout), [] -> (* Herd finished without errors - normal *)
       begin
         match read_some_file litmus expected with
         | None -> false
         | Some expected_output ->
            if log_compare stdout expected_output <> 0 then begin
              Printf.printf "Failed %s : Logs do not match\n%!" litmus ;
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
         "Failed %s : unexpected exit code %i, %s output %s error.\n"
         litmus r (some stdout) (some stderr) ;
       false

let read_output_files litmus =
  let o = read_file (outname litmus)
  and e = read_file (errname litmus) in
  o,e

let output_matches_expected litmus expected =
  try
    let o,e = read_output_files litmus in
    do_check_output litmus expected "" "" (0,o,e)
  with Command.Error e ->
     Printf.printf "Failed %s : %s \n" litmus
       (Command.string_of_error e) ; false

let do_herd_output_matches_expected
      do_run litmus expected expected_failure expected_warn =
  try
    let t = do_run litmus in
    do_check_output
      litmus expected expected_failure expected_warn t
  with
  | Command.Error e ->
     Printf.printf "Failed %s : %s \n" litmus
       (Command.string_of_error e) ; false

let herd_output_matches_expected
      ~bell ~cat ~conf ~variants ~libdir
      herd litmus expected expected_failure expected_warn =
  do_herd_output_matches_expected
    (fun litmus ->
      run_herd
        ~bell:bell ~cat:cat ~conf:conf
        ~variants:variants ~libdir:libdir herd [litmus])
    litmus expected expected_failure expected_warn

let herd_args_output_matches_expected
      herd args  litmus expected expected_failure expected_warn =
  do_herd_output_matches_expected
    (run_herd_args herd args)
    litmus expected expected_failure expected_warn

let is_litmus path = Filename.check_suffix path ".litmus"
let is_expected path = Filename.check_suffix path ".litmus.expected"

let expected_of_litmus litmus = litmus ^ ".expected"
let litmus_of_expected expected = Filename.chop_suffix expected ".expected"

let expected_failure_of_litmus litmus = litmus ^ ".expected-failure"
let litmus_of_expected_failure expected = Filename.chop_suffix expected ".expected-failure"

let expected_warn_of_litmus litmus = litmus ^ ".expected-warn"

let remove_if_exists path =
  if Sys.file_exists path then
    Sys.remove path

let write_file path lines =
  Filesystem.write_file path (fun o -> Channel.write_lines o lines)


let promote litmus t =
  let expected = expected_of_litmus litmus in
  let expected_failure = expected_failure_of_litmus litmus in
  match t with
  | 0, [], [] ->
     Printf.printf "Failed %s : Returned neither stdout nor stderr\n" litmus ;
     false

  | 0, out, [] ->
     remove_if_exists expected_failure ;
     write_file expected out ;
     true

  | r, [], err when r <> 0 ->
     remove_if_exists expected ;
     write_file expected_failure err ;
     true

  | 0, out, err ->
     write_file expected out ;
     let expected_warn = expected_warn_of_litmus litmus in
     write_file expected_warn err ;
     true

  | r, _, _  ->
     Printf.printf "Failed %s : unexpected exit code %i\n" litmus r ;
     false
