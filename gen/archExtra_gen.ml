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
  val pp_location_brk : location -> string
  val location_compare : location -> location -> int

  module LocSet : MySet.S with type elt = location
  module LocMap : MyMap.S with type key = location

(* Initial states *)
  type initval = S of string | P of AArch64PteVal.t
  val pp_initval : initval -> string
  val initval_eq : initval -> initval -> bool


  type init = (location * initval option) list

(* complete init with necessary information *)
  val complete_init : init -> init


(***********************)
(* Register allocation *)
(***********************)

  type st
  val debug_env : st -> string

  val st0 : st

  val alloc_reg : st -> arch_reg * st
  val alloc_trashed_reg : string -> st -> arch_reg * st
  val alloc_loop_idx : string -> st -> arch_reg * st

  type special
  val alloc_special : st -> special * st

  val set_friends : arch_reg -> arch_reg list -> st -> st
  val get_friends : st -> arch_reg -> arch_reg list

  val ok_reg : st -> arch_reg * st
  val next_ok : st -> st
  val get_noks : st -> int

  val add_type : location -> TypBase.t -> st -> st
  val get_env : st -> TypBase.t LocMap.t

  val next_addr : st -> string * st
end

module Make(I:I) : S
with type arch_reg = I.arch_reg and type special = I.special
= struct
  type arch_reg = I.arch_reg

  type location =
    | Reg of int * arch_reg
    | Loc of string

  let pp_symbol loc =
    match Misc.tr_pte loc with
    | Some s -> Misc.pp_pte s
    | None -> loc

  let pp_location = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> pp_symbol loc

  let pp_location_brk = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> sprintf "[%s]" (pp_symbol loc)

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

  type initval = S of string | P of AArch64PteVal.t
  let pp_initval = function
    | S v ->  pp_symbol v
    | P p -> AArch64PteVal.pp_v p

  let initval_eq v1 v2 = match v1,v2 with
  | S s1,S s2 -> Misc.string_eq s1 s2
  | P p1,P p2 -> AArch64PteVal.compare p1 p2 = 0
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
          | Some (P p) ->
             add_some
               (OutputAddress.refers_virtual p.AArch64PteVal.oa) k
          | None -> k in
          k)
        StringSet.empty i in
    StringSet.fold
      (fun x i -> (Loc x,None)::i)
      (StringSet.diff refer already_here)
      i

  module RegMap =
    MyMap.Make
      (struct
        type t = I.arch_reg
        let compare = compare
      end)

  type st =
      { regs : arch_reg list ;
        map  : arch_reg StringMap.t ;
        specials : I.special list ;
        noks : int ;
        env : TypBase.t LocMap.t ; (* Record types *)
        (* Group special registers together *)
        friends : arch_reg list RegMap.t;
        (* For fresh addresses *)
        next_addr : int; }

  let debug_env st =
    LocMap.pp_str
      (fun loc t -> sprintf "%s->%s" (pp_location loc) (TypBase.pp t))
      st.env

  let st0 =
    { regs = I.free_registers;
      map = StringMap.empty;
      specials = I.specials;
      noks = 0;
      env = LocMap.empty;
      friends = RegMap.empty;
      next_addr = 0; }

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

  type special = I.special
  let alloc_special st = match st.specials with
  | [] -> Warn.fatal "No more special registers"
  | r::rs -> r,{ st with specials = rs; }

  let set_friends r rs st =
    let friends = RegMap.add r rs st.friends in
    { st with friends; }

  let get_friends st r = RegMap.safe_find [] r st.friends

  let ok_reg st = alloc_trashed_reg "ok" st
  let next_ok st = { st with noks = st.noks+1; }
  let get_noks st = st.noks

  let add_type loc t st = { st with env = LocMap.add loc t st.env; }
  let get_env st = st.env

  let next_addr st =
    let n = st.next_addr in
    let r =
      if n = 0 then "_z" else sprintf "_z%d" n in
    r,{ st with next_addr = n+1; }
end
