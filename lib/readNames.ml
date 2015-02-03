(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

