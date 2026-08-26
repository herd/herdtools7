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

(********************)
(* "Compare" faults *)
(********************)

(*
 * It is important to notice that compare on fault does not
 *  yield a transitive equality function. For instance
 *   + compare Fault(L0,x,"MMU:Permission") Fault(L0,x) == 0
 *   + compare Fault(L0,x) Fault(L0,x,"MMU:Translation") == 0
 *   + compare  Fault(L0,x,"MMU:Permission")  Fault(L0,x,"MMU:Translation") !=0
 * A similar example is possible with "prefixed" MMU faults:
 *   + compare Fault(L0,x,"D-MMU:Permission") Fault(L0,x,"MMU:Permission") == 0
 *   + compare Fault(L0,x,"MMU:Permission") Fault(L0,x,"I-MMU:Permission") == 0
 *   + compare  Fault(L0,x,"D-MMU:Permission")  Fault(L0,x,"I-MMU:Permission") !=0
 * Hence we have "old" faults (no fault type), "new" faults (with fault-types)
 * and "very new" faults. As long as the three sets do not mix, teh compare
 * function has the transivity properties that are expected from compare
 * functions. Such properties are required for sort to operate 
 * As the compare function is used to sort final states, it is important
 * for these sets never to mix. Notice that this invariant holds naturally
 * for initial logs. Moreover, when summing logs, the "newer" states are
 * priviledged over "older" ones.
 *)

let warn_once = ref true

let compare_ftype_names s1 s2 =
  match
    FaultType.strip_diprefix s1,
    FaultType.strip_diprefix s2
  with
  | None,Some s2 -> String.compare s1 s2
  | Some s1,None -> String.compare s1 s2
  | _,_ -> String.compare s1 s2

let compare_ftypes ft1 ft2 =
  match HashedStringOpt.as_t ft1, HashedStringOpt.as_t ft2 with
  | Some ft1, Some ft2 -> compare_ftype_names ft1 ft2
  | None, None -> 0
  | None, Some _ | Some _, None ->
      if !warn_once then begin
        Warn.warn_always "Comparing faults with and without fault type, \
                          assuming same type";
        warn_once := false;
      end;
      0

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
  | Some ft -> if FaultType.has_diprefix ft then DIPrefix else Other
  | None -> No
