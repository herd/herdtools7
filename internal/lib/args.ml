(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities for using the built-in Arg module. *)

type spec = Arg.key * Arg.spec * Arg.doc

(** Specs. *)

let append_string r =
  Arg.String (fun v -> r := !r @ [v])

let set_string_option r =
  Arg.String (fun v -> r := Some v)

(** Common options *)

let npar j =
  "-j",Arg.Int (fun i -> j := Some (max i 1)),
  "<n> concurrent run with at most <n> instances"

(** Validators. *)

let validate check msg (key, spec, doc) =
  let check_value v =
    if not (check v) then
      raise (Arg.Bad (Printf.sprintf "Invalid %s: %s" key msg))
  in
  let spec =
    match spec with
    | Arg.String f ->
        Arg.String (fun v -> check_value v ; f v)
    | Arg.Set_string r ->
        Arg.String (fun v -> check_value v ; r := v)
    | _ ->
        failwith "Args.validate only accepts Arg.String or Arg.Set_string"
  in
  key, spec, doc

let is_file =
  validate (fun v -> Sys.file_exists v && not (Sys.is_directory v)) "Must be a path to a file"

let is_dir =
  validate Sys.is_directory "Must be a path to a directory"
