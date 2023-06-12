(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Branch target for instruction *)

type t = Lbl of Label.t | Offset of int

open Printf

let pp_offset i =
  if i >= 0 then sprintf "+%d" i
  else sprintf "%d" i
  
let pp = function
  | Lbl lbl -> Label.pp lbl
  | Offset i -> pp_offset i


let tgt2next = function
  | Lbl lbl -> Label.To lbl
  | Offset _ -> Label.Any

and tgt_cons t1 = function
  | Lbl lbl -> [t1; Label.To lbl;]
  | Offset _ -> [Label.Any]
  
                                      
let as_string_fun f s =
  match f (Lbl s) with
  | Lbl s -> s
  | Offset _ -> s (* Offset not supported, forget about it *)
