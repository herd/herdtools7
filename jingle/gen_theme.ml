(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Boqun Feng                                                               *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf
open Str

let verbose = ref false
let map = ref None
let call = ref None
let outdir = ref None
let prog =
  if Array.length Sys.argv > 0 
  then Sys.argv.(0)
  else "gen_theme"

exception Error of string

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> verbose := true),
     "- be verbose";
     "-map",Arg.String (fun s -> map := Some s),
     "<name> - give the map file <name>";
     "-call",Arg.String (fun s -> call := Some s),
     "<name> - give the call file <name>";
     "-o",Arg.String (fun s -> outdir := Some s),
     "<name> - directory for output files"]
    (fun _ -> ())
    (sprintf "Usage: %s [option]* -map <file> -call <file>" prog)


let map = !map
let call = !call
let verbose = !verbose
let outdir = !outdir

let parsed = match map with
| None -> raise (Error "No map file provided.")
| Some s ->
      try Misc.input_protect ParseMap.parse s
      with ParseMap.Error msg ->
        eprintf "File \"%s\": %s\n" s msg ;
        exit 1

let parsed_call = match call with
| None -> raise (Error "No call file provided.")
| Some s ->
      try Misc.input_protect CallMap.parse s
      with CallMap.Error msg ->
        eprintf "File \"%s\": %s\n" s msg ;
        exit 1

let rec apply f e map_env func_env = match List.filter (fun (x, _, _) -> String.equal x f) func_env with
| [] -> raise(Error ("no func for " ^ f ^ "."))
| (_, d, c) :: tl -> match List.filter (fun (x, _) -> String.equal x e) map_env with
               | [] -> raise(Error ("no left rule for " ^ e ^ "."))
               | (_, mr) :: _ ->
                    let re = Str.regexp d in
                    if Str.string_match re mr 0
                    then Str.replace_first (Str.regexp d) c mr
                    else apply f e map_env tl

let expand call_list map_env func_env =
        let try_expand (l, f, a) = 
                let r = apply f a map_env func_env
                in (l, r)
        in List.map try_expand call_list

let () =
  if verbose then begin
   eprintf "Reading map file :\n";
    List.iter (fun (s,t) ->
      eprintf "\"%s\" -> \"%s\"\n" s t)
      parsed.ParseMap.conversions;
    List.iter (fun (n, f, a) ->
      eprintf "\"%s\" -> \"@%s %s\"\n" n f a)
      parsed.ParseMap.funcs;
   eprintf "Reading call file :\n";
    List.iter (fun (n, l, r) ->
      eprintf "\"%s\" : \"%s\" -> \" %s\"\n" n l r)
      parsed_call.CallMap.conversions;
   eprintf "End Reading.\n"
  end;

  let maps = parsed.ParseMap.conversions in

  let calls = parsed_call.CallMap.conversions in

  let funcs = parsed.ParseMap.funcs in

  List.iter (fun (l, r) -> Printf.printf "\"%s\" -> \"%s\"\n" l r) maps;
  List.iter (fun (l, r) -> Printf.printf "\"%s\" -> \"%s\"\n" l r) (expand calls maps funcs)


