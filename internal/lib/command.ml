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

exception Error of string

let command bin args =
  match args with
  | [] -> (Filename.quote bin)
  | _ -> Printf.sprintf "%s %s" (Filename.quote bin) (String.concat " " (List.map Filename.quote args))

let run bin args =
  let cmd = command bin args in
  match Sys.command cmd with
  | 0 -> ()
  | n -> raise (Error (Printf.sprintf "Process returned error code %i" n))

let run_with_stdout bin args f =
  let cmd = command bin args in
  let stdout = Unix.open_process_in cmd in
  let ret = f stdout in
  match Unix.close_process_in stdout with
  | Unix.WEXITED 0 -> ret
  | Unix.WEXITED n -> raise (Error (Printf.sprintf "Process returned error code %i" n))
  | Unix.WSIGNALED n -> raise (Error (Printf.sprintf "Process was killed by signal %i" n))
  | Unix.WSTOPPED n -> raise (Error (Printf.sprintf "Process was stopped by signal %i" n))
