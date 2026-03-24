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

let placeholder_re =
  Re.(compile (seq [ char '{'; group (rep1 digit); char '}' ]))

let subst str args =
  let str, () =
    Util.replace_all placeholder_re
      (fun () re_match ->
        let n = int_of_string (Re.Group.get re_match 1) in
        let new_str = args.(n) in
        (new_str, ()))
      () str
  in
  str

let rec uniq rs = function
  | [] -> []
  | c :: cs ->
      if List.exists (String.equal c) rs then uniq rs cs
      else c :: uniq (c :: rs) cs

let is_reverse str =
  let uniq_placehs =
    Re.all placeholder_re str
    |> List.map (fun re_match -> Re.Group.get re_match 1)
    |> uniq []
  in
  match uniq_placehs with
  | [ "1"; "0" ] -> true
  | [ "0"; "1" ] -> false
  | _ -> raise (Invalid_argument "not a binary relation")
