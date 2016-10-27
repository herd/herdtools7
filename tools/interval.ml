(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Natural integer interval *)

type k = Open | Closed
type b = Nat of int * k | Infinity
type t = b * b

let all = Nat (0,Closed),Infinity
open Printf

let pp_b low = function
  | Nat (i,o) ->
      if low then
        sprintf "%c%i" (match o with Open -> ']' | Closed -> '[') i
      else
        sprintf "%i%c" i (match o with Open -> '[' | Closed -> ']')
  | Infinity ->
      if low then "]" else "["
      
let pp (b1,b2) = match b1,b2 with
| Nat (i1,Open),Nat(i2,Open) when i1=i2 -> sprintf "%i" i1
| _,_ ->
    sprintf "%s..%s" (pp_b true b1) (pp_b false b2)

let get_pred = function
  | Open -> (<)
  | Closed ->  (<=)

let inside  (b1,b2) c =
  (match b1 with
  | Nat (i,o) -> (get_pred o) i c
  | Infinity -> true) &&
  (match b2 with
  | Nat (i,o) -> (get_pred o) c i
  | Infinity ->  true)


