(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

type bd = string * string
type bds = bd list
type cnf = bds list

let pp_bd (loc,v) = sprintf "%s=%s" loc v

let pp_simple bdss =
  let pp =
    List.map
      (fun bds ->
        String.concat " /\\ "
          (List.map pp_bd bds))
      bdss in
  let pp = List.map (sprintf "(%s)") pp in
  let pp = String.concat " \\/ "  pp in
  pp

let compare_bd (loc1,v1) (loc2,v2) =
  match String.compare loc1 loc2 with
  | 0 -> String.compare v1 v2
  | r -> r

module Env =
  MyMap.Make
    (struct
      type t = string * string
      let compare = compare_bd
    end)


type prop =
  | Or of prop * prop
  | And of prop * prop
  | Atom of bd
  | True
  | False

let mk_or p1 p2 = match p1,p2 with
| (True,_)|(_,True) -> True
| (False,p)|(p,False) -> p
| _,_ -> Or (p1,p2)

let mk_and p1 p2 = match p1,p2 with
| (True,p)|(p,True) -> p
| (False,_)|(_,False) -> False
| _,_ -> And (p1,p2)

 
let pp_prop =
  let rec pp_or_arg = function
    | True|False -> assert false
    | Atom bd -> pp_bd bd
    | Or (p1,p2) ->
        sprintf "%s \\/ %s" (pp_or_arg p1) (pp_or_arg p2)
    | And (p1,p2) ->
        sprintf "%s /\\ %s" (pp_and_arg p1) (pp_and_arg p2)
  and pp_and_arg = function
    | True|False -> assert false
    | Atom bd -> pp_bd bd
    | Or (p1,p2) ->
        sprintf "(%s \\/ %s)" (pp_or_arg p1) (pp_or_arg p2)
    | And (p1,p2) ->
        sprintf "%s /\\ %s" (pp_and_arg p1) (pp_and_arg p2) in
  pp_or_arg
  
let do_opt =
  let build_env =
    List.fold_left
      (List.fold_left
         (fun env bd ->
           let old =
             try Env.find bd env
             with Not_found -> 0 in
           Env.add bd (old+1) env))
      Env.empty in
  let find_max env =
    Env.fold
      (fun bd n (_,n_max as max) ->
        if n > n_max then (bd,n) else max)
      env (("",""),0) in

  let split bd bdss =
    let rec remove = function
      | [] -> raise Not_found
      | bd0::rem ->
          if compare_bd bd bd0 = 0 then rem
          else bd0::remove rem in
    List.fold_left
      (fun (ok,no) bds ->
        try
          let bds = remove bds in
          (bds::ok,no)
        with
          Not_found -> (ok,bds::no))
      ([],[]) bdss in
  fun bdss ->
    let rec do_rec bdss = match bdss with
    | [] -> False
    | [bds] ->
        let rec do_rec = function
          | [] -> True
          | bd::bds -> mk_and (Atom bd) (do_rec bds) in
        do_rec bds
    | []::_bdss -> True
    | [bd]::bdss ->
        mk_or (Atom bd) (do_rec bdss)
    | _ ->
        let (bd_max,_) = find_max (build_env bdss) in
        let ok,no = split bd_max bdss in
        let pp_ok = do_rec ok in
        let pp_no = do_rec no in
        mk_or (mk_and (Atom bd_max) pp_ok) pp_no in
    do_rec bdss

let pp_opt bdss = pp_prop (do_opt bdss)
