(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* PAR_EL1 first then general
We've gone for a generic system register that definitely contains an output address*)

open Printf

type t =
  { p_oa : OutputAddress.t option;
    p_kv : string StringMap.t; }

let empty = { p_oa=None; p_kv=StringMap.empty;}

let add_oa oa p =
  if Misc.is_some p.p_oa then
    Warn.user_error "multiple defintion or property oa" ;
  { p with p_oa = Some oa; }

let add_kv k v p =
  let {p_kv;_} = p in
  if StringMap.mem k p_kv then
    Warn.user_error "multiple defintion or property %s" k ;
  { p with p_kv=StringMap.add k v p_kv; }

  let apply_not_empty f = function
  | "" -> ""
  | s -> f s

let pp_comma = apply_not_empty (sprintf ", %s")

let mk_pp pp_oa { p_oa; p_kv;} =
  sprintf
    "(%s%s)"
    (match p_oa with
     | None -> ""
     | Some pa -> sprintf "oa:%s" (pp_oa pa))
    (pp_comma
       (StringMap.pp_str_delim ", "
          (fun k v -> sprintf "%s:%s" k v)
          p_kv))

let pp_old = mk_pp OutputAddress.pp_old
and pp = mk_pp  OutputAddress.pp
let pp_norm norm p =
  let p = { p with p_kv=norm p.p_kv; } in
  pp p

let compare p1 p2 =
  match Misc.opt_compare OutputAddress.compare p1.p_oa p2.p_oa with
  | 0 -> StringMap.compare String.compare p1.p_kv p2.p_kv
  | r -> r

let eq p1 p2 =
  Misc.opt_eq OutputAddress.eq p1.p_oa p2.p_oa
  && StringMap.equal String.equal p1.p_kv p2.p_kv