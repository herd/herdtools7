(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
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
  { p_oa : OutputAddress.t option;
    p_kv : string StringMap.t;
    p_attrs : StringSet.t; }

let empty = { p_oa=None; p_kv=StringMap.empty; p_attrs=StringSet.empty; }

let add_oa oa p =
  if Misc.is_some p.p_oa then
    Warn.user_error "multiple defintion or property oa" ;
  { p with p_oa = Some oa; }

let add_oa_if_none oa p =
  if Misc.is_some p.p_oa then p
  else { p with p_oa = Some oa; }

let add_kv k v p =
  let {p_kv;_} = p in
  if StringMap.mem k p_kv then
    Warn.user_error "multiple defintion or property %s" k ;
  { p with p_kv=StringMap.add k v p_kv; }

let add_attr a p =
    { p with p_attrs = StringSet.add a p.p_attrs; }
let add_attrs a p =
  { p with p_attrs = StringSet.union (StringSet.of_list a) p.p_attrs; }


let apply_not_empty f = function
  | "" -> ""
  | s -> f s

let pp_comma = apply_not_empty (sprintf ", %s")

let mk_pp pp_oa { p_oa; p_kv; p_attrs; } =
  sprintf
    "(%s%s%s)"
    (match p_oa with
     | None -> ""
     | Some oa -> sprintf "oa:%s" (pp_oa oa))
    (pp_comma
       (StringMap.pp_str_delim ", "
          (fun k v -> sprintf "%s:%s" k v)
          p_kv))
    (pp_comma (StringSet.pp_str ", " Misc.identity p_attrs))

let pp_old = mk_pp OutputAddress.pp_old
and pp = mk_pp  OutputAddress.pp
let pp_norm norm p =
  let p = { p with p_kv=norm p.p_kv; } in
  pp p

let compare p1 p2 =
  match Misc.opt_compare OutputAddress.compare p1.p_oa p2.p_oa with
  | 0 ->
      begin
        match StringMap.compare String.compare p1.p_kv p2.p_kv with
        | 0 ->
            StringSet.compare p1.p_attrs p2.p_attrs
        | r -> r
      end
  | r -> r

let eq p1 p2 =
  Misc.opt_eq OutputAddress.eq p1.p_oa p2.p_oa
  && StringMap.equal String.equal p1.p_kv p2.p_kv
  && StringSet.equal p1.p_attrs p2.p_attrs
