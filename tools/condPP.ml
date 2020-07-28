(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type bd = string * string
type fault = string
type bds = bd list * fault list
type cnf = bds list

let pp_bd (loc,v) = sprintf "%s=%s" loc v
and pp_fault f = f

let pp_simple bdss =
  let pp =
    List.map
      (fun (bds,fs) ->
        String.concat " /\\ "
          (List.map pp_bd bds@fs))
      bdss in
  let pp = List.map (sprintf "(%s)") pp in
  let pp = String.concat " \\/ "  pp in
  pp

let compare_bd (loc1,v1) (loc2,v2) =
  match String.compare loc1 loc2 with
  | 0 -> String.compare v1 v2
  | r -> r

type key = Bd of bd | Fa of fault

module type SigEnv = sig
  type t
  val empty : t
  val see_bd : bd -> t -> t
  val see_fault : fault -> t -> t
  val find_max : t -> key
end

module Env : SigEnv = struct
  module BdEnv =
    MyMap.Make
      (struct
        type t = string * string
        let compare = compare_bd
      end)

  type t = int BdEnv.t * int StringMap.t

  let empty = BdEnv.empty,StringMap.empty

  let see_bd bd (t1,t2) =
    let old = BdEnv.safe_find 0 bd t1 in
    BdEnv.add bd (old+1) t1,t2

  and see_fault (f:fault) (t1,t2) =
    let old = StringMap.safe_find 0 f t2 in
    t1,StringMap.add f (old+1) t2

  let find_max (t1,t2) =
    let bd,max1 =
      BdEnv.fold
        (fun bd n (_,n_max as max) ->
          if n > n_max then (bd,n) else max)
        t1 (("",""),0)
    and f,max2 =
      StringMap.fold
        (fun f n (_,n_max as max) ->
          if n > n_max then (f,n) else max)
        t2 ("",0) in
    if max1 > max2 then Bd bd else Fa f
end


type prop =
  | Or of prop * prop
  | And of prop * prop
  | Atom of bd
  | Fault of fault
  | True
  | False

let mk_atom = function
  | Bd bd -> Atom bd
  | Fa f -> Fault f

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
    | Fault f -> pp_fault f
    | Or (p1,p2) ->
        sprintf "%s \\/ %s" (pp_or_arg p1) (pp_or_arg p2)
    | And (p1,p2) ->
        sprintf "%s /\\ %s" (pp_and_arg p1) (pp_and_arg p2)
  and pp_and_arg = function
    | True|False -> assert false
    | Atom bd -> pp_bd bd
    | Fault f -> f
    | Or (p1,p2) ->
        sprintf "(%s \\/ %s)" (pp_or_arg p1) (pp_or_arg p2)
    | And (p1,p2) ->
        sprintf "%s /\\ %s" (pp_and_arg p1) (pp_and_arg p2) in
  pp_or_arg

let rec remove pred = function
  | [] -> raise Not_found
  | x::xs ->
      if pred x then xs else x::remove pred xs

let do_opt =

  let build_env =
    List.fold_left
      (fun env (bds,fs) ->
        let env =
          List.fold_left
            (fun env bd -> Env.see_bd bd env)
            env bds in
        List.fold_left
          (fun env f -> Env.see_fault f env)
          env fs)
      Env.empty in

  let split k bdss =

    List.fold_left
      (fun (ok,no) (bds,fs) -> match k with
      | Bd bd ->
          begin try
            let bds = remove (fun x -> compare_bd bd x = 0) bds in
            ((bds,fs)::ok,no)
          with
            Not_found -> (ok,(bds,fs)::no)
          end
      | Fa f ->
          begin try
            let fs = remove (Misc.string_eq f) fs in
            ((bds,fs)::ok,no)
          with
            Not_found -> (ok,(bds,fs)::no)
          end)
      ([],[]) bdss in

  fun bdss ->
    let rec do_rec bdss = match bdss with
    | [] -> False
    | [bds,fs] ->
        List.fold_right
          (fun bd k -> mk_and (Atom bd) k)
          bds
          (List.fold_right
             (fun f k -> mk_and (Fault f) k)
             fs True)
    | ([],[])::_bdss -> True
    | ([bd],[])::bdss ->
        mk_or (Atom bd) (do_rec bdss)
    | ([],[f])::bdss ->
        mk_or (Fault f) (do_rec bdss)
    | _ ->
        let r_max = Env.find_max (build_env bdss) in
        let ok,no = split r_max bdss in
        let pp_ok = do_rec ok in
        let pp_no = do_rec no in
        mk_or (mk_and (mk_atom r_max) pp_ok) pp_no in
    do_rec bdss

let pp_opt bdss = pp_prop (do_opt bdss)
