(*********************************************************************)
(*                     DIY                                           *)
(*                                                                   *)
(*     John Wickerson, Imperial College, London UK                   *)
(* Copyright 2014 Institut National de Recherche en Informatique et  *)
(* en Automatique and the authors. All rights reserved.              *)
(* This file is distributed under the terms of the Lesser GNU        *)
(* General Public License.                                           *)
(*********************************************************************)

open Printf

(** GPU scope tree *)

type thread      = int
type warp        = thread list
type cta         = warp list
type kernel      = cta list
type device      = kernel list
type scope_tree  = device list


(* let scope_tree = ref No_scope_tree *)
    
let rec create_list total acc l = 
  if acc = total then l
  else
    create_list total (acc+1) l@[acc]
      
(*The default scope tree for CPUs (used in -ext and -int operator in herd)*)
let cpu_scope_tree n = 
  let w = (create_list n 0 []) in 
  let cta = List.map (fun x -> [x]) w in
  let ker = List.map (fun x -> [x]) cta in
  let dev = List.map (fun x -> [x]) ker in
  let top = List.map (fun x -> [x]) dev in
  top

let pp_warp w =
  let mapped_list = List.map (fun (i) -> sprintf "P%d" i) w in
  "(warp " ^(String.concat " " mapped_list) ^ ")"

let pp_cta c =
  let mapped_list = List.map (fun (w) -> pp_warp w) c in
  "(cta " ^(String.concat " " mapped_list) ^ ")"

let pp_kernel k =
  let mapped_list = List.map (fun (c) -> pp_cta c) k in
  "(kernel " ^(String.concat " " mapped_list) ^ ")"

let pp_device k =
  let mapped_list = List.map (fun (c) -> pp_kernel c) k in
  "(device " ^(String.concat " " mapped_list) ^ ")"

let pp_scope_tree s = 
  let mapped_list = List.map (fun (k) -> pp_device k) s in
  String.concat " " mapped_list
  
