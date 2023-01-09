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

{
type t =
  {
     dic : Proc.t -> bool;
     idc : Proc.t -> bool;
  }

type nat = DIC | IDC

exception Error

}

let num = ['0'-'9']+
let blank = [' ''\t''\r']
rule all k = parse
|('P'? (num as x) ':')?
 (['a'-'z''A'-'Z']+ as key)
 {
  let proc = Misc.app_opt int_of_string x
  and f = match Misc.lowercase key with
  | "dic" -> DIC
  | "idc" -> IDC
  | _ -> Warn.user_error "'%s' is not a cache-type key, keys are DIC, IDC" key in
  all ((proc,f)::k) lexbuf }
| blank+ { all k lexbuf }
| eof { k }
| "" { raise Error }

{

let default =
  let f _ = false in
  { dic=f; idc=f; }

let filter_opt f =
  List.fold_left
    (fun k p -> match f p with
    | None -> k
    | Some x -> x::k)
    []

let get info =
 match
   MiscParser.get_info_on_info
     MiscParser.cache_type_key info
 with
 | None -> None
 | Some s ->
    try
      let xs = all [] (Lexing.from_string s) in
      let idcs = xs
      and dics = List.filter (function (_,DIC) -> true | _ -> false) xs in
      let idc =
        if List.exists (function (None,(IDC|DIC)) -> true | _ -> false) idcs then
          (fun _ -> true)
        else
          let xs =
            filter_opt
              (function (Some _ as p,(IDC|DIC)) -> p | _ -> None)
              idcs in
          fun proc -> List.exists (Misc.int_eq proc) xs
      and dic =
        if List.exists (function (None,DIC) -> true | _ -> false) dics then
          (fun _ -> true)
        else
          let xs =
            filter_opt
              (function (Some _ as p,DIC) -> p | _ -> None)
              dics in
          fun proc -> List.exists (Misc.int_eq proc) xs in
      Some { dic; idc; }
    with Error ->
      Warn.user_error "Incorrect cache-type feature specification '%s'" s
}
