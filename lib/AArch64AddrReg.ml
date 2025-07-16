(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* PAR_EL1 *)

open Printf

type t = {
  oa : OutputAddress.t;
  f : int;
  }

let default =
  { oa=OutputAddress.PHY "";
  f=0; }

  let do_pp _hexa old_oa p =
    let field =
      if p.f = 0 then
        sprintf "parel1_t:(oa:%s)"
          ((if old_oa then OutputAddress.pp_old
            else OutputAddress.pp) p.oa)
      else "parel1_t:(f:1)" in
    sprintf "(%s)" field

(* By default pp does not list fields whose value is default *)
let pp hexa = do_pp hexa false
let pp_v = pp false

let my_int_of_string s v =
  let v = try int_of_string v with
    _ -> Warn.user_error "PAR_EL1 field %s should be an integer" s
  in v

let add_field k v p =
  match k with
  | "f" -> { p with f = my_int_of_string k v }
  | _ ->
      Warn.user_error "Illegal PAR_EL1 property %s" k

let tr p =
  let open ParsedAddrReg in
  let r = default in
  let r =
    match p.p_oa with
    | None -> r
    | Some oa -> { r with oa; } in
  let r = StringMap.fold add_field p.p_kv r in
    if r.f = 1 && Option.is_some p.p_oa then
      Warn.user_error "Illegal combination: f = 1 and OA is present";
  r

let pp_norm p =
  let n = tr p in
  pp_v n

(* used in symbConstant.ml *)

let compare =
  let cmp = (fun p1 p2 -> Misc.int_compare p1.f p2.f) in
  cmp

let eq p1 p2 = OutputAddress.eq p1.oa p2.oa && Misc.int_eq p1.f p2.f

(*For Litmus*)
let dump_pack pp_oa a =
  if a.f = 1 then
    "pack_synthetic_par_el1(0, 1)"
  else
    Printf.sprintf "pack_synthetic_par_el1(%s, 0)"
      (pp_oa (OutputAddress.pp_old a.oa))

let fields = ["f";]
and default_fields = ["0"]
