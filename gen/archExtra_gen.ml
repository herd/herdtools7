(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module type I = sig
 type arch_reg

  val is_symbolic : arch_reg -> bool
  val pp_reg : arch_reg -> string
  val free_registers : arch_reg list

  type special
  val specials : special list
end

module type S = sig
  type arch_reg

(* Locations *)
  type location =
    | Reg of Code.proc * arch_reg
    | Loc of string

  val of_loc : Code.loc -> location
  val of_reg : Code.proc -> arch_reg -> location

  val pp_location : location -> string
  val location_compare : location -> location -> int

  module LocSet : MySet.S with type elt = location
  module LocMap : MyMap.S with type key = location

(* Initial states *)
  type initval = S of string | P of PTEVal.t
  val pp_initval : initval -> string
  val initval_eq : initval -> initval -> bool


  type init = (location * initval option) list

(* complete init with necessary information *)
  val complete_init : init -> init


(***********************)
(* Register allocation *)
(***********************)

  type st
  val st0 : st

  val alloc_reg : st -> arch_reg * st
  val alloc_trashed_reg : string -> st -> arch_reg * st
  val alloc_loop_idx : string -> st -> arch_reg * st

  val current_label : st -> int
  val next_label : st -> int

  val next_label_st : st -> st

  type special
  val alloc_special : st -> special * st

  val ok_reg : st -> arch_reg * st
  val next_ok : st -> st
  val get_noks : st -> int
end

module Make(I:I) : S
with type arch_reg = I.arch_reg and type special = I.special
= struct
  type arch_reg = I.arch_reg

  type location =
    | Reg of int * arch_reg
    | Loc of string

  let pp_location = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> loc

  let location_compare loc1 loc2 = match loc1,loc2 with
  | Reg _,Loc _ -> -1
  | Loc _,Reg _ -> 1
  | Reg (p1,r1),Reg (p2,r2) ->
      begin match Misc.int_compare p1 p2 with
      | 0 -> compare r1 r2
      | r -> r
      end
  | Loc loc1,Loc loc2 -> compare loc1 loc2

  module LocOrd = struct
    type t = location
    let compare = location_compare
  end

  module LocSet = MySet.Make(LocOrd)
  module LocMap = MyMap.Make(LocOrd)

  let of_loc loc = Loc (Code.as_data loc)
  let of_reg p r = Reg (p,r)

  type initval = S of string | P of PTEVal.t
  let pp_initval = function
    | S v ->  v
    | P p -> PTEVal.pp p

  let initval_eq v1 v2 = match v1,v2 with
  | S s1,S s2 -> Misc.string_eq s1 s2
  | P p1,P p2 -> PTEVal.compare p1 p2 = 0
  | (S _,P _)|(P _,S _) -> false

  type init = (location * initval option) list

  let as_virtual s = match Misc.tr_pte s with
  | Some _ -> None
  | None ->
      if LexScan.is_num s then None else Some s

  let refers_virtual s = match Misc.tr_pte s with
  | Some _ as r -> r
  | None -> match Misc.tr_physical s with
    | Some _ as r -> r
    | None -> None

  let add_some x xs = match x with
  | None -> xs
  | Some x -> StringSet.add x xs

  let complete_init i =
    let already_here =
      List.fold_left
        (fun k (loc,v) ->
          let k = match loc with
          | Loc s -> add_some (as_virtual s) k
          | Reg _  -> k in
          let k = match v with
          | Some (S s) -> add_some (as_virtual s) k
          | _ -> k in
          k)
        StringSet.empty i in
    let refer =
      List.fold_left
        (fun k (loc,v) ->
          let k = match loc with
          | Loc s -> add_some (refers_virtual s) k
          | Reg _ -> k in
          let k = match v with
          | Some (S s) -> add_some (refers_virtual s) k
          | Some (P p) -> add_some (refers_virtual p.PTEVal.oa) k
          | None -> k in
          k)
        StringSet.empty i in
    StringSet.fold
      (fun x i -> (Loc x,None)::i)
      (StringSet.diff refer already_here)
      i

  type st =
      { regs : arch_reg list ;
        map  : arch_reg StringMap.t ;
        label : int ;
        specials : I.special list ;
        noks : int ; }


  let st0 =
    { regs = I.free_registers;
      map = StringMap.empty;
      label = 0;
      specials = I.specials;
      noks = 0; }

  let alloc_reg st = match st.regs with
    | [] -> Warn.fatal "No more registers"
    | r::rs -> r,{ st with regs = rs; }

  let alloc_last_reg st = match st.regs with
    | [] -> Warn.fatal "No more registers"
    | r::rs ->
        let r,rs = Misc.pop_last r rs in
        r,{ st with regs = rs; }


  let do_alloc_trashed_reg alloc k st =
    try
      let r = StringMap.find k st.map in
      r,st
    with Not_found ->
      let r,st = alloc st in
      r,{ st with map = StringMap.add k r st.map; }

  let alloc_trashed_reg k st = do_alloc_trashed_reg alloc_reg k st
  and alloc_loop_idx k st = do_alloc_trashed_reg alloc_last_reg k st

  let current_label st = st.label
  let next_label st = st.label+1
  let next_label_st st = { st with label = st.label+1; }

  type special = I.special
  let alloc_special st = match st.specials with
  | [] -> Warn.fatal "No more special registers"
  | r::rs -> r,{ st with specials = rs; }

  let ok_reg st = alloc_trashed_reg "ok" st
  let next_ok st = { st with noks = st.noks+1; }
  let get_noks st = st.noks
end
