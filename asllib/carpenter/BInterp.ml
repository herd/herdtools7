(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

type binterp_interface = {
  binterp_in : out_channel;
  binterp_out : in_channel;
  base_pickled : string;
  pid : int;
}

(* Triggers character level stderr logging - to use with caution. *)
let _dbg = false

let _log_src =
  Logs.Src.create ~doc:"BInterp Ex Runner for carpenter" "carpenter.binterp"

let debug logs = Logs.debug ~src:_log_src logs
let info logs = Logs.info ~src:_log_src logs

let exception_regex =
  Re.compile
    Re.(
      seq
        [
          bol;
          alt
            [
              str "Syntax error"; str "Exception"; seq [ rep any; str "error" ];
            ];
          rep space;
          rep any;
          stop;
        ])

let parse_output output_std =
  match Re.Seq.matches exception_regex output_std () with
  | Seq.Nil ->
      debug (fun m -> m "Output did not match regex.");
      Ok ()
  | Seq.Cons (h, _) ->
      debug (fun m -> m "Output matched regex.");
      Error h

let until_prompt =
  let rec loop iter in_channel =
    match input_char in_channel with
    | '\n' -> on_new_line iter in_channel
    | c ->
        iter c;
        loop iter in_channel
  and on_new_line iter in_channel =
    match input_char in_channel with
    | '>' -> on_chevron iter in_channel
    | c ->
        iter '\n';
        iter c;
        loop iter in_channel
  and on_chevron iter in_channel =
    match input_char in_channel with
    | ' ' -> ()
    | c ->
        iter '\n';
        iter '>';
        iter c;
        loop iter in_channel
  and entry_point iter in_channel =
    match input_char in_channel with
    | '>' -> on_chevron iter in_channel
    | c ->
        iter c;
        loop iter in_channel
  in
  fun iter in_channel ->
    if _dbg then Printf.eprintf "Waiting for prompt ...\n%!";
    try entry_point iter in_channel with End_of_file -> ()

let get_until_prompt state =
  if _dbg then Printf.eprintf "Getting output: \"%!";
  let buf = Buffer.create 16 in
  let iter c = Buffer.add_char buf c in
  let iter =
    if _dbg then (fun c ->
      Printf.eprintf "%c%!" c;
      iter c)
    else iter
  in
  until_prompt iter state.binterp_out;
  if _dbg then Printf.eprintf "\".\n%!";
  Buffer.contents buf

let ignore_until_prompt state =
  if _dbg then Printf.eprintf "Ignoring output: \"%!";
  let iter = if _dbg then fun c -> Printf.eprintf "%c%!" c else fun _c -> () in
  until_prompt iter state.binterp_out;
  if _dbg then Printf.eprintf "\".\n%!"

let run_command state cmd =
  debug (fun m -> m "Running command %S" cmd);
  let oc = state.binterp_in in
  output_string oc cmd;
  output_char oc '\n';
  flush oc;
  ()

let run_command_and_ignore state cmd =
  run_command state cmd;
  ignore_until_prompt state;
  ()

let run_command_and_get_output state cmd =
  run_command state cmd;
  let output = get_until_prompt state in
  debug (fun m -> m "Got output from command: %S." output);
  output

let run_commands_and_ignore state cmds =
  List.iter (run_command_and_ignore state) cmds

let prepare_binterp_state state =
  info (fun m -> m "Start preparing BInterp initial state.");
  ignore_until_prompt state;
  run_commands_and_ignore state
    [ ":set asl=1.0"; ":pickle " ^ state.base_pickled ];
  info (fun m -> m "Finished preparing BInterp initial state.");
  ()

let reset_binterp_state state =
  debug (fun m -> m "Start resetting BInterp state.");
  run_commands_and_ignore state [ ":reset"; ":unpickle " ^ state.base_pickled ];
  debug (fun m -> m "Finished resetting BInterp state.");
  ()

let quit_binterp state () =
  info (fun m -> m "Start quitting BInterp.");
  debug (fun m -> m "Removing %s" state.base_pickled);
  Sys.remove state.base_pickled;
  run_command state ":quit";
  debug (fun m -> m "Waiting for BInterp to quit.");
  let _pid', _status = UnixLabels.waitpid ~mode:[] state.pid in
  debug (fun m -> m "BInterp quitted. Removing pipes.");
  close_out state.binterp_in;
  close_in state.binterp_out;
  info (fun m -> m "BInterp quitted.");
  ()

let create_binterp binterp_path =
  info (fun m -> m "Spawning new BInterp thread from %s." binterp_path);
  let base_pickled = Filename.temp_file "binterp-base-state" ".tmp" in
  debug (fun m -> m "BInterp initial state will be pickled at %s." base_pickled);
  let open UnixLabels in
  let stdin, binterp_in =
    let i, o = pipe () in
    (i, out_channel_of_descr o)
  and stdout, stderr, binterp_out =
    let i, o = pipe () in
    (o, dup o, in_channel_of_descr i)
  and prog = binterp_path
  and args = [||] in
  let pid = create_process ~prog ~args ~stdin ~stdout ~stderr in
  info (fun m -> m "BInterp process spawned.");
  debug (fun m -> m "BInterp thread spawned with PID %d." pid);
  { binterp_out; binterp_in; base_pickled; pid }

let with_binterp binterp_path f =
  let state = create_binterp binterp_path in
  Fun.protect ~finally:(quit_binterp state) @@ fun () ->
  prepare_binterp_state state;
  f state

let run_one state type_only asl_file =
  debug (fun m -> m "BInterp loading file at %s." asl_file);
  let output = run_command_and_get_output state (":load " ^ asl_file) in
  let parsed_output = parse_output output in
  match (type_only, parsed_output) with
  | true, _ | _, Error _ ->
      debug (fun m ->
          if type_only then m "BInterp loaded file."
          else
            m
              "Error while type-checking. BInterp runner will not execute this \
               file.");
      reset_binterp_state state;
      parsed_output
  | false, Ok () ->
      debug (fun m -> m "File type-checked successfully, running it.");
      let output =
        run_command_and_get_output state
          "assert main () == 0; print(\"\\\\n\");"
      in
      debug (fun m -> m "File ran.");
      reset_binterp_state state;
      parse_output output

let run_one_ast state type_only ast =
  let asl_file, out = Filename.open_temp_file "gen-ast" ".asl" in
  debug (fun m -> m "Writing ast at %s" asl_file);
  let fmt = Format.formatter_of_out_channel out in
  Asllib.PP.pp_t fmt ast;
  Format.pp_print_flush fmt ();
  close_out out;
  debug (fun m -> m "Done.");
  run_one state type_only asl_file
