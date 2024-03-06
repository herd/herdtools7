(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t =
  { intid : string;
    field : (string * string) }

let empty = { intid = ""; field = ("", "") }

let add_intid v p = { p with intid = v }

let add_field f v p =
  if p.field != empty.field then
    let (f, _) = p.field in
    Warn.user_error "Cannot update more than one field. Field %s already defined" f
  else
    { p with field = (f, v) }

let eq p1 p2 =
  let (f1, v1) = p1.field in
  let (f2, v2) = p2.field in
  String.equal p1.intid p2.intid &&
  String.equal f1 f2 &&
  String.equal v1 v2

let compare =
  (fun p1 p2 -> String.compare p1.intid p2.intid)
    |> Misc.lex_compare (fun p1 p2 -> compare p1.field p2.field)

let pp p =
  match p.field with
  | "valid", "0" -> "(valid:0)"
  | f,v -> sprintf "(intid:%s, %s:%s)" p.intid f v
