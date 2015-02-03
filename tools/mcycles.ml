(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Extract name: cycle from test(s) *)

open Printf

type cycle = One of string | Norm of string * string

let get_cycle info =
  let orig =
    try Some (List.assoc "Orig" info)
    with Not_found -> None
  and cycle =
    try Some (List.assoc "Cycle" info)
    with Not_found -> None in
  match orig,cycle with
  | Some o,Some c -> Norm (o,c)
  | None,Some c
  | Some c,None -> One c
  | None,None -> raise Not_found

let from_splitted splitted =
  let name = splitted.Splitter.name
  and info = splitted.Splitter.info in
  name.Name.name,get_cycle info

module SP = Splitter.Make(Splitter.Default)

let from_file name =
  let sp = Misc.input_protect (SP.split name) name in
  from_splitted sp


(* Command line *)
let arg = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mcycles"

let () =
  Arg.parse
    []
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [test]*" prog)

let tests = !arg

let seen = ref StringSet.empty

let () =
  Misc.iter_argv
    (fun name ->
      try
        let tname,cycle = from_file name in
        match cycle with
        | One c ->
            printf "%s: %s\n" tname c
        | Norm (o,c) ->
            if not (StringSet.mem c !seen) then begin
              seen := StringSet.add c !seen ;
              printf "%s: %s\n" tname o
            end
      with
      | Not_found -> Warn.warn_always "%s: no cycle" name
      | Misc.Exit -> ()
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg
      | e ->
	  Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
	  raise e)
    tests






