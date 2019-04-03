(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t = string

let lab_count = ref 0 

let reset () = lab_count := 0

let next_label s =
  let x = !lab_count in
  incr lab_count ;
  sprintf "%s%02i" s x
 
let fail p = sprintf "Fail%i" p
and exit p = sprintf "Exit%i" p

type next = Any | Next | To of t

module Set = StringSet
module Map = StringMap
