(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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

let get_scope info =
  try Some (List.assoc "Scopes" info)
  with Not_found -> None

let from_splitted splitted =
  let name = splitted.Splitter.name
  and info = splitted.Splitter.info in
  name.Name.name,get_cycle info,get_scope info

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

module S =
  MySet.Make
    (struct
      type t = string * string option

      let compare x y =
        Misc.pair_compare 
          String.compare
          (Misc.opt_compare String.compare)
          x y
    end)

let seen = ref S.empty

let ppo = function
  | None -> ""
  | Some s -> " " ^ s

let () =
  Misc.iter_argv_or_stdin
    (fun name ->
      try
        let tname,cycle,st = from_file name in
        match cycle with
        | One c ->
            printf "%s: %s%s\n" tname c (ppo st)
        | Norm (o,c) ->
            let k = c,st in
            if not (S.mem k !seen) then begin
              seen := S.add k !seen ;
              printf "%s: %s%s\n" tname o (ppo st)
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






