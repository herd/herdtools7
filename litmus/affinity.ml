(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t =
  | No
  | Incr of int
  | Random
  | Custom
  | Scan


let incr_pref = "incr"
let pref_len = String.length incr_pref
let tags = ["none"; "random"; "custom" ; sprintf "%s<n>" incr_pref; "scan";]


let parse tag = match Misc.lowercase tag with
| "none" -> Some No
| "random"|"rand" -> Some Random
| "custom" -> Some Custom
| "scan" -> Some Scan
| _ ->
      try
        let len = String.length tag in
        if len > pref_len then
          let pref =  String.sub tag 0 pref_len in
          if pref <> incr_pref then raise (Failure incr_pref) ;
          let suff = String.sub tag pref_len (len-pref_len) in
          Some (Incr (int_of_string suff))
        else
          None
      with Failure _ -> None

  let pp = function
    | No -> "none"
    | Incr i -> sprintf "incr%i" i
    | Random -> "random"
    | Custom -> "custom"
    | Scan -> "scan"
