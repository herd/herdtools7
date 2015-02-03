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

(** GPU memory space map *)


type gpu_memory_space = 
| GlobalMem
| LocalMem

let pp_gpu_memory_space = function
  | GlobalMem -> "CLK_GLOBAL_MEM_FENCE"
  | LocalMem -> "CLK_LOCAL_MEM_FENCE"

let pp_gpu_memory_spaces spaces = 
  String.concat "|" (List.map pp_gpu_memory_space spaces)

let pp_gpu_memory_space_short = function
  | GlobalMem -> "G"
  | LocalMem -> "L"

let pp_gpu_memory_spaces_short spaces = 
  String.concat "" (List.map pp_gpu_memory_space_short spaces)

type mem_space_map = (string * gpu_memory_space) list

(* let mem_space_map = ref No_mem_space_map *)

let pp_memory_space_map m = 
  let str_list = 
    List.map 
      (fun (x,y) -> sprintf "%s:%s" x (pp_gpu_memory_space y)) m 
  in
  String.concat "; " str_list

let is_global msm x = 
  let ms =
    try
      List.assoc x msm
    with 
      Not_found -> 
      Warn.fatal "Location %s not in memory space map" x
  in match ms with
  | GlobalMem -> true
  | LocalMem -> false

let is_local msm x = 
  let ms =
    try
      List.assoc x msm
    with 
      Not_found -> 
      Warn.fatal "Location %s not in memory space map" x
  in match ms with
  | GlobalMem -> false
  | LocalMem -> true
  
        
