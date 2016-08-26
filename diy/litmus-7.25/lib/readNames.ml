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

(* Read name (or kind files) for names *)


let input_line chan =
  try Some (Pervasives.input_line chan)
  with End_of_file -> None

let get_fst s =
  try
    let i = String.index s ' ' in
    String.sub s 0 i
  with
  | Not_found -> s

let from_chan chan add k =
  let rec do_rec k = match input_line chan with
  | Some name ->
      let name = get_fst name in
      do_rec (add name k)
  | None -> k in
  do_rec k

let from_file fname add k =
  Misc.input_protect
    (fun chan -> from_chan chan add k)
    fname

let from_files fnames  add k =
  List.fold_right
    (fun fname k -> from_file fname add k)
    fnames k

