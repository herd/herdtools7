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

(******************************)
(* Extract names from test(s) *)
(******************************)

open Printf

let from_file name = name,Names.from_fname name

let with_source = ref true
let arg = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "msort"

let () =
  Arg.parse
    [
     "-src",Arg.Bool (fun b -> with_source := b),
     (sprintf "<bool> include source file names into output, default %b" !with_source)]
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [test]*" prog)

let tests = !arg

let do_test name =
  try
    let fname,name =  from_file name in
    if !with_source then
      printf "%s %s\n" fname name
    else
      printf "%s\n" name
  with
  | Misc.Exit -> ()
  | Misc.Fatal msg ->
      Warn.warn_always "%a %s" Pos.pp_pos0 name msg
  | e ->
      Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
      raise e

let () = match tests with
| [] -> Misc.iter_stdin do_test
| _ ->  Misc.iter_argv do_test tests
