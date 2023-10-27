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

(** Utilities for handling in_channel and out_channel. *)

(* in_channel utilities *)

let iter_lines f chan =
  try
    let rec iter () = f (input_line chan) ; iter () in iter ()
  with End_of_file -> ()

let map_lines f chan =
  let ret = ref [] in
  iter_lines (fun l -> ret := f l :: !ret) chan ;
  List.rev !ret

let map_opt_lines f chan =
  let ret = ref [] in
  iter_lines
    (fun l -> match f l with
     | None -> ()
     | Some r -> ret := r :: !ret) chan ;
  List.rev !ret

let read_lines chan =
  map_lines (fun l -> l) chan


(* out_channel utilities *)

let write_lines chan lines =
  List.iter (fun l -> Printf.fprintf chan "%s\n" l) lines
