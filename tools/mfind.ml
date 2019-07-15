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

(*********************************************)
(* Inverse of mnames: find source from names *)
(*********************************************)

open Printf

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mfind"

let args = ref []
let names = ref []
let excl = ref []
let uniq = ref false
let neg = ref false

let () =
  Arg.parse
    [CheckName.parse_names names ;
     CheckName.parse_excl excl ;
     "-u", Arg.Set uniq, " one source per matching sources";
     "-neg", Arg.Set neg, " find sources whose names are not given";]
    (fun s -> args := s :: !args)
    (sprintf "Usage: %s [test]*" prog)


let tests = !args
let uniq = !uniq
let neg = !neg

(* Read names *)
module Check =
  CheckName.Make
    (struct
      let verbose = 0
      let rename = []
      let select = []
      let names = !names
      let excl = !excl
    end)

let from_file name = name,Names.from_fname name

(* Positive version: find tests with name in k *)

let do_test ok src k =
  try
    let src,name = from_file src in
    if ok name then
      let old = StringMap.safe_find [] name k in
      StringMap.add name (src::old) k
    else k
  with
  | Misc.Exit -> k
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.warn_always "%s" msg ; k

let do_test_pos = do_test Check.ok
and do_test_neg = do_test (fun name -> not (Check.ok name))

let names =
  let fold =
    let do_test = if neg then do_test_neg else do_test_pos in
    match tests with
    | [] -> Misc.fold_stdin do_test
    | _  -> Misc.fold_argv do_test tests in
  fold StringMap.empty


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
