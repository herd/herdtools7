(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = ((Proc.t * string option) * string option * string option)

let has_diprefix s =
  String.(starts_with ~prefix:"D-" s || starts_with ~prefix:"I-" s)

let strip_diprefix s =
  if has_diprefix s then Some String.(sub s 2 (length s-2))
  else None

(* Notice: ft1 is a fault specification or pattern *)
let match_fault_type ft1 ft2 = match ft1, ft2 with
  | Some ft1, Some ft2 ->
      begin
        match  strip_diprefix ft1 with
        | Some ft1 ->
            let ft2 =
              match strip_diprefix ft2 with
              | Some ft2 -> ft2
              | None -> ft2 in
            String.equal ft1 ft2
        | None -> String.equal ft1 ft2
      end
  | None,(None|Some _) -> true
  | Some _,None -> false

let pp ((p,lab),v,ft) =
  Printf.sprintf "fault(%s%s%s%s)"
    (Proc.pp p) (match lab with None -> "" | Some lab -> ":"^lab)
    (match v with None -> "" | Some v -> ","^v)
    (match ft with None -> "" | Some ft -> ","^ft)
