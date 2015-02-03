(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Extract names + kinds from test(s) (ie produce kind file) *)

open Printf
let verbose = true

let from_file fname = Names.from_fname fname

let arg = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "kname"

let kinds = ref []
let defkind = ref LogState.NoKind 

module NoVerb = struct let verbose = 0 end
module L =  LogState.Make(NoVerb)

let parse_kind s = match L.parse_kind s with
| Some k -> k
| None -> raise (Arg.Bad (sprintf "Incorrect kind: %s" s))

let () =
  Arg.parse
    [("-kinds", Arg.String (fun s -> kinds := !kinds @ [s]),
      "<name> specify kinds of tests") ;    
     ("-default", Arg.String (fun s -> defkind := (parse_kind s)),
      sprintf "<kind> default kind (default %s)" (L.pp_kind  !defkind)) ;
   ]
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [test]*" prog)

let tests = !arg
let kinds = !kinds
let defkind = !defkind

module LR =  LexRename.Make(NoVerb)
let kinds = LR.read_from_files kinds L.parse_kind

let do_test fname =
  try
    let name =  from_file fname in
    let k =
      try TblRename.find_value kinds name
      with Not_found -> defkind in
    printf "%s %s\n" name (L.pp_kind k)
  with
  | Misc.Exit -> ()
  | Misc.Fatal msg ->
      Warn.warn_always "%a %s" Pos.pp_pos0 fname msg
  | e ->
      Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
      raise e

let () = match tests with
| [] -> Misc.iter_stdin do_test
| _ ->  Misc.iter_argv do_test tests
