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
  { intid : string option;
    field : (string * string) option }

let empty = { intid= None; field= None }

let add_intid v p = { p with intid = Some v }

let add_field f v p =
  match p.field with
  | Some (f, _) ->
    Warn.user_error "Cannot update more than one field. Field %s already defined" f
  | None ->
    { p with field = Some (f, v) }

let eq p1 p2 =
  let eq_intid =
    match p1.intid, p2.intid with
    | Some i1, Some i2 -> String.equal i1 i2
    | Some _, None| None, Some _
    | None, None -> true
  in
  match p1.field, p2.field with
  | Some (f1, v1), Some (f2, v2) ->
    eq_intid && String.equal f1 f2 && String.equal v1 v2
  | Some _, None
  | None, Some _
  | None, None -> eq_intid

let compare =
  (fun p1 p2 -> Option.compare String.compare p1.intid p2.intid)
    |> Misc.lex_compare (fun p1 p2 -> compare p1.field p2.field)

let pp p =
  match p.field, p.intid with
  | Some ("valid", "0"), _ -> "(valid:0)"
  | None, Some i1 -> sprintf "(intid:%s)" i1
  | Some (f, v), Some i1 -> sprintf "(intid:%s, %s:%s)" i1 f v
  | Some (f, v), None -> sprintf "(%s:%s)" f v
  | None, None -> assert false
