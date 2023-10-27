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

(** Utilities for running commands. *)

module Fun = Base.Fun
module Option = Base.Option

type error = {
  binary : string ;
  args   : string list ;
  status : Unix.process_status ;
}

exception Error of error

let command bin args =
  match args with
  | [] -> (Filename.quote bin)
  | _ -> Printf.sprintf "%s %s" (Filename.quote bin) (String.concat " " (List.map Filename.quote args))


let string_of_process_status = function
  | Unix.WEXITED n -> Printf.sprintf "returned error code %i" n
  | Unix.WSIGNALED n -> Printf.sprintf "killed by signal %i" n
  | Unix.WSTOPPED n -> Printf.sprintf "stopped by signal %i" n

let string_of_error { binary = bin ; args = args ; status = s } =
  Printf.sprintf "Process %s (command: %s)"
    (string_of_process_status s)
    (command bin args)


let nop _ = ()

let in_pipe nonblock =
  let in_fd, out_fd = Unix.pipe ~cloexec:true () in
  if nonblock then Unix.set_nonblock in_fd ;
  out_fd, Unix.in_channel_of_descr in_fd

let out_pipe nonblock =
  let in_fd, out_fd = Unix.pipe ~cloexec:true () in
  if nonblock then Unix.set_nonblock out_fd ;
  in_fd, Unix.out_channel_of_descr out_fd

let do_run must_succeed ?stdin:in_f ?stdout:out_f ?stderr:err_f bin args =
  (* Notes:
   * - By default, the file descriptors are Unix.stdin, Unix.stdout, Unix.stderr,
   *   and the {in,out,err}_f and close_{in,out,err}_pipe functions are all nops.
   * - If the user passed us a function, the corresponding file descriptor is
   *   one side of a pipe, and the close_*_pipe function closes the other side.
   * - Communication pipes need to be marked ~cloexec:true.
   * - We need to close our side of each pipe once the subprocess has started.
   *)

  let in_fd, in_f, close_in_pipe =
    match in_f with
    | None -> Unix.stdin, nop, nop
    | Some f ->
        let fd, o = out_pipe false in
        fd, (fun _ -> Unix.close fd ; f o), (fun _ -> close_out o)
  in
  let out_fd, out_f, close_out_pipe =
    match out_f with
    | None -> Unix.stdout, nop, nop
    | Some f ->
        let fd, i = in_pipe false in
        fd, (fun _ -> Unix.close fd ; f i), (fun _ -> close_in i)
  in
  let err_fd, err_f, close_err_pipe =
    match err_f with
    | None -> Unix.stderr, nop, nop
    | Some f ->
        let fd, i = in_pipe false in
        fd, (fun _ -> Unix.close fd ; f i), (fun _ -> close_in i)
  in

  let pid =
    Fun.protect
      ~finally:(fun _ -> close_in_pipe () ; close_out_pipe () ; close_err_pipe ())
      (fun _ ->
        let pid = Unix.create_process bin (Array.of_list (bin :: args)) in_fd out_fd err_fd in
        in_f () ;
        out_f () ;
        err_f () ;
        pid
      )
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> 0
  | Unix.WEXITED r when not must_succeed -> r
  | status ->
      raise (Error { binary = bin ; args = args ; status = status })

let run ?stdin ?stdout ?stderr bin args =
  ignore (do_run true ?stdin ?stdout ?stderr bin args)
and run_status ?stdin ?stdout ?stderr bin args =
  do_run false ?stdin ?stdout ?stderr bin args


module NonBlock = struct

  let output_line out line =
    try
      let line = line ^ "\n" in
      output_string out line ;
      true
    with Sys_blocked_io -> false

  let process_input fds_i i = match fds_i,i with
    | [],_ -> i
    | [_],Some (i,gen) ->
        let rec do_rec () = match gen () with
          | None -> close_out i ; None
          | Some line ->
              if output_line i line then do_rec ()
              else Some (i,gen) in
        do_rec ()
    | _ -> assert false

  let input_lines f chan =
    try
      while true do
        let () = f (input_line chan) in ()
      done ;
      assert false
    with
    | End_of_file -> close_in chan ; None
    | Sys_blocked_io -> Some (chan,f)

  let process_output fds o = match o with
    | None -> o
    | Some (out,f) ->
        let fd = Unix.descr_of_in_channel out in
        if List.exists (fun x -> x = fd) fds then
          input_lines f out
        else o

  let process_outputs fds o e = match fds with
    | [] -> o,e
    | _::_ ->
        let o = process_output fds o
        and e = process_output fds e in
        o,e

  let rec loop i o e =
    let fds_i =
      match i with
      | None -> []
      | Some (i,_) -> [Unix.descr_of_out_channel i]
    and fds_o =
      (match o with
      | None -> []
      | Some (o,_) -> [Unix.descr_of_in_channel o])
      @(match e with
      | None -> []
      | Some (e,_) -> [Unix.descr_of_in_channel e]) in
      let fds_o,fds_i,_ = Unix.select fds_o fds_i [] (-1.0) in
      let i = process_input fds_i i in
      let o,e = process_outputs fds_o o e in
      match i,o,e with
      | None,None,None -> ()
      | _,_,_ -> loop i o e

  let do_run must_succeed ?stdin:in_f ?stdout:out_f ?stderr:err_f bin args =
  (* Notes:
   * - By default, the file descriptors are Unix.stdin, Unix.stdout, Unix.stderr,
   *   and the {in,out,err}_f and close_{in,out,err}_pipe functions are all nops.
   * - If the user passed us a function, the corresponding file descriptor is
   *   one side of a pipe, and the close_*_pipe function closes the other side.
   * - Communication pipes need to be marked ~cloexec:true.
   * - We need to close our side of each pipe once the subprocess has started.
   *)

    let in_fd, in_f, close_in_pipe, i =
    match in_f with
    | None -> Unix.stdin, nop, nop, None
    | Some f ->
        let fd, o = out_pipe true in
        fd, (fun _ -> Unix.close fd), (fun _ -> close_out o),Some (o,f) in
    let out_fd, out_f, close_out_pipe, o =
      match out_f with
      | None -> Unix.stdout, nop, nop, None
      | Some f ->
          let fd, i = in_pipe true in
          fd, (fun _ -> Unix.close fd), (fun _ -> close_in i),Some (i,f) in

    let err_fd, err_f, close_err_pipe, e =
      match err_f with
      | None -> Unix.stderr, nop, nop, None
      | Some f ->
          let fd, i = in_pipe true in
          fd, (fun _ -> Unix.close fd), (fun _ -> close_in i), Some (i,f) in
    let pid =
      Fun.protect
        ~finally:
        (fun _ -> close_in_pipe () ; close_out_pipe () ; close_err_pipe ())
        (fun _ ->
          let pid =
            Unix.create_process bin (Array.of_list (bin :: args))
              in_fd out_fd err_fd in
          in_f () ;
          out_f () ;
          err_f () ;
          loop i o e ;
          pid) in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> 0
  | Unix.WEXITED r when not must_succeed -> r
  | status ->
      raise (Error { binary = bin ; args = args ; status = status })

let run ?stdin ?stdout ?stderr bin args =
  ignore (do_run true ?stdin ?stdout ?stderr bin args)
and run_status ?stdin ?stdout ?stderr bin args =
  do_run false ?stdin ?stdout ?stderr bin args
end
