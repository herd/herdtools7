(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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
