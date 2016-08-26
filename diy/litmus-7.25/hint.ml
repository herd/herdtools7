(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(*******************************************)
(* Passing hints from generators to litmus *)
(*******************************************)

type t = (string * string) list


(*****************)
(* Dumping hints *)
(*****************)

open Printf

type out = out_channel option

let none = None

let open_out fname =
  try
    let chan = open_out fname in
    Some chan
  with Sys_error msg ->
    Warn.warn_always "Cannot open info file: %s" msg ;
    None

let close_out = function
  | None -> ()
  | Some chan -> close_out chan

let dump out name h = match out with
| None -> ()
| Some chan ->
    List.iter
      (fun (k,v) -> fprintf chan "%s %s %s\n" name k v)
      h


(*****************)
(* Reading hints *)
(*****************)

module M = Map.Make(String)

let get_infos m name =
  try  M.find name m
  with Not_found -> []

type table = t M.t

let empty = M.empty

let do_read m fname =
  LexHint.read fname
    (fun m name k v ->
      let xs = get_infos m name in
      M.add name ((k,v)::xs) m)
    m

let read fname = do_read empty fname

let get m name =
  let xs = M.find name m in
  xs
