(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris France.                                        *)
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

(* DIC and IDC are set per proc for legacy reasons:
 * they are used to compute hashes of litmus tests
 *)
{
type t =
  {
     dic : Proc.t -> bool;
     idc : Proc.t -> bool;
  }

type nat = DIC | IDC

exception Error

}

let blank = [' ''\t''\r']
rule all k = parse
 (['a'-'z''A'-'Z']+ as key)
 {
  let ct = match Misc.lowercase key with
  | "dic" -> DIC
  | "idc" -> IDC
  | _ -> Warn.user_error "'%s' is not a cache-type key, keys are DIC, IDC" key in
  all (ct::k) lexbuf }
| blank+ { all k lexbuf }
| eof { k }
| "" { raise Error }

{

let default =
  let f _ = false in
  { dic=f; idc=f; }

let get info =
 match
   MiscParser.get_info_on_info
     MiscParser.cache_type_key info
 with
 | None -> None
 | Some s ->
    try
      let xs = all [] (Lexing.from_string s) in
      let dic =
        let dic_exists = List.exists (function DIC -> true | _ -> false) xs in
          (fun _ -> dic_exists) in
      (* Note that DIC implies IDC *)
      let idc =
        let idc_or_dic_exists = List.exists (function (IDC|DIC) -> true) xs in
          (fun _ -> idc_or_dic_exists) in
      Some { dic; idc; }
    with Error ->
      Warn.user_error "Incorrect cache-type feature specification '%s'" s
}
