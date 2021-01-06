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

(** Utilities for running commands. *)

module Fun = Base.Fun
module Option = Base.Option

exception Error of string

let command bin args =
  match args with
  | [] -> (Filename.quote bin)
  | _ -> Printf.sprintf "%s %s" (Filename.quote bin) (String.concat " " (List.map Filename.quote args))


let run ?stdin:in_f ?stdout:out_f ?stderr:err_f bin args =
  (* Notes:
   * - By default, the file descriptors are Unix.stdin, Unix.stdout, Unix.stderr,
   *   and the {in,out,err}_f and close_{in,out,err}_pipe functions are all nops.
   * - If the user passed us a function, the corresponding file descriptor is
   *   one side of a pipe, and the close_*_pipe function closes the other side.
   * - Communication pipes need to be marked ~cloexec:true.
   * - We need to close our side of each pipe once the subprocess has started.
   *)
  let nop _ = () in

  let in_pipe () =
    let in_fd, out_fd = Unix.pipe ~cloexec:true () in
    out_fd, Unix.in_channel_of_descr in_fd
  in
  let out_pipe () =
    let in_fd, out_fd = Unix.pipe ~cloexec:true () in
    in_fd, Unix.out_channel_of_descr out_fd
  in

  let in_fd, in_f, close_in_pipe =
    match in_f with
    | None -> Unix.stdin, nop, nop
    | Some f ->
        let fd, o = out_pipe () in
        fd, (fun _ -> Unix.close fd ; f o), (fun _ -> close_out o)
  in
  let out_fd, out_f, close_out_pipe =
    match out_f with
    | None -> Unix.stdout, nop, nop
    | Some f ->
        let fd, i = in_pipe () in
        fd, (fun _ -> Unix.close fd ; f i), (fun _ -> close_in i)
  in
  let err_fd, err_f, close_err_pipe =
    match err_f with
    | None -> Unix.stderr, nop, nop
    | Some f ->
        let fd, i = in_pipe () in
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
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> raise (Error (Printf.sprintf "Process returned error code %i" n))
  | Unix.WSIGNALED n -> raise (Error (Printf.sprintf "Process was killed by signal %i" n))
  | Unix.WSTOPPED n -> raise (Error (Printf.sprintf "Process was stopped by signal %i" n))
