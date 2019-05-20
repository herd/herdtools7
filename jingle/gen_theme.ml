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
let includes = ref []
let map = ref None
let call = ref None
let outdir = ref None
let loops = ref 5
let prog =
  if Array.length Sys.argv > 0 
  then Sys.argv.(0)
  else "gen_theme"

exception Error of string

let () =
  Arg.parse
    ["-version",
     Arg.Unit
       (fun () ->
         printf "%s, Rev: %s\n" Version_jingle.version Version_jingle.rev ;
         exit 0),
     " - show version number and exit" ;
     "-libdir",
     Arg.Unit (fun () -> print_endline Version_jingle.libdir; exit 0),
     " - show installation directory and exit";
     "-v",Arg.Unit (fun () -> verbose := true),
     "- be verbose";
     "-I", Arg.String (fun s -> includes := !includes @ [s]),
     "<dir> - add <dir> to search path";
     "-map",Arg.String (fun s -> map := Some s),
     "<name> - give the map file <name>";
     "-call",Arg.String (fun s -> call := Some s),
     "<name> - give the call file <name>";
     "-o",Arg.String (fun s -> outdir := Some s),
     "<name> - directory for output files";
     "-n",Arg.Int (fun i -> loops := i),
     "<loops> - times re-apply the functions in call file with the update env"]
    (fun _ -> ())
    (sprintf "Usage: %s [option]* -map <file> -call <file> -n <loops>" prog)


let includes = !includes
let map = !map
let call = !call
let verbose = !verbose
let outdir = !outdir
let loops = !loops

let libfind =
  let module ML =
    MyLib.Make
      (struct
        let includes = includes
        let env = None
        let libdir = Version_jingle.libdir
      end) in
  ML.find

let parsed = match map with
| None -> raise (Error "No map file provided.")
| Some s ->
    let s = libfind s in
    try Misc.input_protect ParseMap.parse s
    with ParseMap.Error msg ->
      eprintf "File \"%s\": %s\n" s msg ;
      exit 1

let parsed_call = match call with
| None -> raise (Error "No call file provided.")
| Some s ->
    let s = libfind s in
    try Misc.input_protect CallMap.parse s
    with CallMap.Error msg ->
      eprintf "File \"%s\": %s\n" s msg ;
      exit 1


let pick_func f func_env = List.find_opt (fun (x, _) -> Misc.string_eq x f) func_env

let remove_func f func_env = List.remove_assoc f func_env

let pick_mapping e map_env = List.find_opt (fun (x, _) -> Misc.string_eq x e) map_env

let rec apply f arg func_env = match pick_func f func_env with
| None -> raise(Error ("no func for " ^ f ^ " for arg: " ^ arg ^ "."))
| Some(_, ParseMap.Appliable(d, c)) -> begin
        let re = Str.regexp d in
        try let _ = Str.search_forward re arg 0 in
                Str.replace_first re c arg
        with Not_found ->  apply f arg (remove_func f func_env)
        end
| Some(_, ParseMap.Sequence(fs)) -> begin
        (* If all functions in the sequence exist in 'func_env' *)
        if fs |> List.for_all (fun g -> Misc.is_some (pick_func g func_env)) then
                List.fold_left (fun a g -> apply g a func_env) arg fs
        else apply f arg (remove_func f func_env)
        end

let rec expand call_list map_env func_env times =
        let callable = List.filter (fun (_, _, e) -> List.mem_assoc e map_env) call_list in
        let uncallable = List.filter (fun (_, _, e) -> not (List.mem_assoc e map_env)) call_list in
        let try_expand (l, f, a) = 
                (pick_mapping a map_env) |> Misc.map_opt (fun (_, arg) -> let r = apply f arg func_env in (l, r)) in
        let res = callable |> List.map try_expand
                           |> List.filter (fun x -> Misc.is_some x)
                           |> List.map Misc.as_some in
        if times <= 1 then res
        else match uncallable with
                | [] -> res
                | (_, _, _) :: _ -> (expand uncallable (res @ map_env) func_env (times - 1)) @ res

let () =
  if verbose then begin
   eprintf "Reading map file :\n";
    List.iter (fun (s,t) ->
      eprintf "\"%s\" -> \"%s\"\n" s t)
      parsed.ParseMap.conversions;
    List.iter (fun (n, func) -> match func with
      | ParseMap.Appliable(f, a) -> eprintf "\"%s\" : \"@%s %s\"\n" n f a
      | ParseMap.Sequence(fs) -> eprintf "\"%s\" : %s\n" n (String.concat " | " fs))
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
  Printf.printf "%s to %s\n\n" (Archs.pp parsed.ParseMap.source) (Archs.pp parsed.ParseMap.target);
  List.iter (fun (l, r) -> Printf.printf "\"%s\" -> \"%s\"\n" l r) maps;
  List.iter (fun (l, r) -> Printf.printf "\"%s\" -> \"%s\"\n" l r) (expand calls maps funcs loops)


