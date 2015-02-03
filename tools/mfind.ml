(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*********************************************)
(* Inverse of mnames: find source from names *)
(*********************************************)

open Printf

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mfind"

let args = ref []
let names = ref []
let uniq = ref false
let neg = ref false

let () =
  Arg.parse
    ["-names", Arg.String (fun n -> names := n :: !names),
     "<file> list of names (cumulate when repeated)";
     "-u", Arg.Set uniq, " one source per matching sources";
     "-neg", Arg.Set neg, " find sources whose names are not given";]
    (fun s -> args := s :: !args)
    (sprintf "Usage: %s [test]*" prog)


let tests = !args
let uniq = !uniq
let neg = !neg

let names =
  ReadNames.from_files !names
    (fun name -> StringMap.add name []) StringMap.empty

let from_file name = name,Names.from_fname name
  
(* Positive version: find tests with name in k *)
let do_test_pos src k =
  try
    let src,name = from_file src in
    try
      let old = StringMap.find name k in
      StringMap.add name (src::old) k
    with Not_found -> k
  with
  | Misc.Exit -> k
  | Misc.Fatal msg ->
      Warn.warn_always "%s" msg ; k
(* Negative version *)
let do_test_neg names src k =
  try
    let src,name = from_file src in
    let in_names =
      try ignore (StringMap.find name names) ; true
      with Not_found -> false in
    if in_names then k
    else
      let old =  try StringMap.find name k with Not_found -> [] in
      StringMap.add name (src::old) k
  with
  | Misc.Exit -> k
  | Misc.Fatal msg ->
      Warn.warn_always "%s" msg ; k

let names =
  let do_test =
    if neg then do_test_neg names
    else do_test_pos in
  match tests with
  | [] -> Misc.fold_stdin do_test names
  | _  -> Misc.fold_argv do_test tests names


let () =
  StringMap.iter 
    (fun _name xs -> match xs with
    | []-> ()
    | src::_ ->
        if uniq then
          printf "%s\n" src
        else
          Misc.rev_iter (printf "%s\n") xs)
    names
    
