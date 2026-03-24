(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = int

let make n = n
let to_string i = Format.sprintf "E%d" i
let re = Re.(seq [ char 'E'; group digit ])

let from_string =
  let regexp = Re.(compile (seq [ re; eos ])) in
  fun s ->
    Re.exec_opt regexp s
    |> Option.map (fun re_match -> int_of_string (Re.Group.get re_match 1))

let regexp = Re.compile re

let detect str =
  Re.all regexp str
  |> List.map (fun re_match -> Re.Group.get re_match 1 |> int_of_string)

let equal = Int.equal
let compare = Int.compare
let pp fmt e = Format.fprintf fmt "%s" (to_string e)
