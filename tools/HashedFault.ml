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

module S = struct
  type t = int * HashedStringOpt.t * HashedStringOpt.t * HashedStringOpt.t

  let equal (p1,a1,b1,c1) (p2,a2,b2,c2) =  p1 == p2 && a1 == a2 && b1 == b2 && c1 == c2

  let hash (p,a,b,c) =
    let ah =  HashedStringOpt.as_hash a
    and bh = HashedStringOpt.as_hash b
    and ch = HashedStringOpt.as_hash c in
    abs (Misc.mix (Misc.mix (0x4F1BBCDC+ah) (0x4F1BBCDC+bh) (0x4F1BBCDC+p)) (0x4F1BBCDC+ch) 0)
end

include (Hashcons.Make(S))

let table = create 101

let as_tt h = h.Hashcons.node

let as_hashed ((p,lab),x,ft) =
  hashcons table
    (p,HashedStringOpt.as_hashed lab,
     HashedStringOpt.as_hashed x,
     HashedStringOpt.as_hashed ft)

let as_t h =
  let p,hlab,hx,hft = h.Hashcons.node in
  ((p,HashedStringOpt.as_t hlab),HashedStringOpt.as_t hx,HashedStringOpt.as_t hft)

let as_hash h = h.Hashcons.hkey

(******************)
(* Compare faults *)
(******************)


(*
 * It is important to notice that faults occurrences
 * as present in logs have changed over time.
 * We have "old" faults (no fault type), "new" faults (with fault-types)
 * and "very new" faults, where MMU faults bear a "D-" or "I-" prefix.
 * depending on if they originate from data memory or from instruction
 * memory.
 * The function "equivalent" abstract on those differences.
 *)

let has_diprefix s =
  String.(starts_with ~prefix:"D-" s || starts_with ~prefix:"I-" s)

let strip_diprefix s =
  if has_diprefix s then Some String.(sub s 2 (length s-2))
  else None

let warn_once = ref true

let equivalent_ftype_names s1 s2 =
  match strip_diprefix s1,strip_diprefix s2 with
  | None,Some s2 -> String.equal s1 s2
  | Some s1,None -> String.equal s1 s2
  | _,_ -> String.equal s1 s2

let equivalent_ftypes ft1 ft2 =
  match ft1,ft2 with
  | None,None -> true
  | Some s1,Some s2 ->
      equivalent_ftype_names s1 s2
  | (None,Some _)
  | (Some _,None) ->
      if !warn_once then begin
        Warn.warn_always "Comparing faults with and without fault type, \
                          assuming same type";
        warn_once := false;
      end ;
      true

let equivalent f1 f2 =
  let p1,lab1,x1,ft1 = as_tt f1
  and p2,lab2,x2,ft2 = as_tt f2 in
  Int.equal p1 p2
  && HashedStringOpt.equal_node lab1 lab2
  && HashedStringOpt.equal_node x1 x2
  && equivalent_ftypes (HashedStringOpt.as_t ft1) (HashedStringOpt.as_t ft2)


(* Standard "compare" function  on faults.
   Can be used for building sets, sorting, etc. *)

let compare_ftypes ft1 ft2 =
  Option.compare
    String.compare
    (HashedStringOpt.as_t ft1)
    (HashedStringOpt.as_t ft2)

let compare h1 h2 =
  Misc.tuple4_compare
    Misc.int_compare  HashedStringOpt.compare
    HashedStringOpt.compare compare_ftypes
    (as_tt h1) (as_tt h2)

type ft_kind =
  | No (* No name *)
  | DIPrefix (* Prefixed with "D-" or "I-" *)
  | Other    (* All other names *)

let compare_kinds = Misc.polymorphic_compare

let get_fault_type h =
  let _,_,_,ft = as_tt h in
  let ft = HashedStringOpt.as_t ft in
  match ft with
  | Some ft -> if has_diprefix ft then DIPrefix else Other
  | None -> No
