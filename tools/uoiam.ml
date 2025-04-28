(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** uoiam is miaou backwards *)

let () = prerr_endline "Coucou"

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "uoiam7"

module Make
    (O:sig
(* Configuration *)
         val verbose : int
(* Definitons *)
       end) =
  struct

    open Earley_core.Earley
    
    type expr = Leaf of int | Plus of expr * expr

    let parser leaf =
      | x:''[0-9]+'' -> Leaf (int_of_string x)


    let parser expr =
      | (leaf)
      | e1:expr "+" e2:expr (Tree (e1,e2))

    let parser main = expr EOF

    let rec pp_tree chan = function
      | Leaf i -> Printf.fprintf chan "%d" i
      | Sum (e1,e2) ->
          Printf.fprintf "(%a+%a)" pp_tree e1 pp_tree e2

    let zyva chan =
      if O.verbose > 0 then prerr_endline "** Parsing" ;
      let tree =
        Earley.parse_channel main Blanks.default chan in
      if O.verbose > 0 then prerr_endline "** Parsed" ;
      Printf.printf "%a\n%!" pp_tree tree
  end

let verbose = ref 0
let libdir = ref (Filename.concat Version.libdir "herd")

let options =
  [
(* Basic *)
    ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;
    ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
    ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
    ("-q", Arg.Unit (fun _ -> verbose := -1 ),
   "<default> do not show diagnostics");
  ]

let args = ref []
let get_cmd_arg s = args := s :: !args
let () =
  try
    Arg.parse options
      get_cmd_arg
      (sprintf "Usage %s [options] [files]+, translate cat definition into English." prog)
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2

let () =
  let module Zyva =
    Make
      (struct
        let verbose = !verbose
      end) in
  Zyva.zyva stdin
