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
  type arch_atom

  val is_symbolic : arch_reg -> bool
  val pp_reg : arch_reg -> string
  val free_registers : arch_reg list

  type special
  type special2
  type special3

  val specials : special list
  val specials2 : special2 list
  val specials3 : special3 list
  val pp_i : int -> string
  module Value:Value_gen.S with type atom = arch_atom
end

module type S = sig

  type arch_atom
  type arch_reg

  module Value : Value_gen.S with type atom = arch_atom

(* Locations *)
  type location =
    | Reg of Code.proc * arch_reg
    | Loc of string

  val of_loc : Code.loc -> location
  val of_reg : Code.proc -> arch_reg -> location

  val pp_i : int -> string
  val pp_location : location -> string
  val pp_location_brk : location -> string
  val location_compare : location -> location -> int

  module LocSet : MySet.S with type elt = location
  module LocMap : MyMap.S with type key = location

(* Initial states *)
  type initval = S of string | P of Value.pte
  val pp_initval : initval -> string
  val initval_eq : initval -> initval -> bool


  type init = (location * initval option) list

(* complete init with necessary information *)
  val complete_init : bool (* hexa *) -> Value.env -> init -> init
  val _pp_env: init -> string


(***********************)
(* Register allocation *)
(***********************)

  type st
  val debug_env : st -> string

  val st0 : st

  (* Given the init state `init`, return the registers,
     `arch_reg list` used in the initial state. *)
  val used_register : init -> arch_reg list
  (* Remove the registers of `arch_reg list`,
     from the registers allocation pool in (the first) `st`
     and return a new (the final) `st`. *)
  val remove_reg_allocator : st -> arch_reg list -> st

  val alloc_reg : st -> arch_reg * st
  val alloc_trashed_reg : string -> st -> arch_reg * st
  val alloc_loop_idx : string -> st -> arch_reg * st

  type special
  type special2
  type special3

  val alloc_special : st -> special * st
  val alloc_special2 : st -> special2 * st
  val alloc_special3 : st -> special3 * st

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
with type arch_reg = I.arch_reg
and type special = I.special
and type special2 = I.special2
and type special3 = I.special3
and type arch_atom = I.arch_atom
and module Value := I.Value
= struct
  type arch_reg = I.arch_reg
  type arch_atom = I.arch_atom

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
    | Loc loc when Misc.tr_pte loc <> None
      -> sprintf "[%s]" (pp_symbol loc)
    | Loc loc -> pp_symbol loc

  let pp_location_brk = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> sprintf "[%s]" (pp_symbol loc)

  let pp_i = I.pp_i

  let location_compare loc1 loc2 = match loc1,loc2 with
  | Reg _,Loc _ -> 1
  | Loc _,Reg _ -> -1
  | Reg (p1,r1),Reg (p2,r2) ->
    begin match Misc.int_compare p1 p2 with
    | 0 -> compare (I.pp_reg r1) (I.pp_reg r2)
    | r -> r
    end
  | Loc loc1, Loc loc2 ->
    (* order `x` before `pte(x)` before `y` *)
    match Misc.tr_pte loc1, Misc.tr_pte loc2 with
    | None, None -> compare loc1 loc2
    | Some pte1, Some pte2 -> compare pte1 pte2
    | Some pte1, None ->
      let result = compare pte1 loc2 in
      if result = 0 then 1 else result
    | None, Some pte2 ->
      let result = compare loc1 pte2 in
      if result = 0 then -1 else result

  module Value = I.Value

  module LocOrd = struct
    type t = location
    let compare = location_compare
  end

  module LocSet = MySet.Make(LocOrd)
  module LocMap = MyMap.Make(LocOrd)

  let of_loc loc = Loc (Code.as_data loc)
  let of_reg p r = Reg (p,r)

  (* - S of a plain value, a pte_* address or a phy_* address
     - P of a PteVal *)
  type initval = S of string | P of Value.pte
  let pp_initval = function
    | S v ->  pp_symbol v
    | P p -> Value.pp_pte p

  let initval_eq v1 v2 = match v1,v2 with
  | S s1,S s2 -> Misc.string_eq s1 s2
  | P p1,P p2 -> Value.pte_compare p1 p2 = 0
  | (S _,P _)|(P _,S _) -> false

  type init = (location * initval option) list

  (* convert to `Some`, if input `s`
     is not a pteval nor a number *)
  let as_virtual s = match Misc.tr_pte s with
  | Some _ -> None
  | None ->
      if LexScan.is_num s then None else Some s

  (* convert to `Some`, if input `s`
     is neither a pteval or a physical location *)
  let refers_virtual s = match Misc.tr_pte s with
  | Some _ as r -> r
  | None -> match Misc.tr_physical s with
    | Some _ as r -> r
    | None -> None

  let add_some x xs = match x with
  | None -> xs
  | Some x -> StringSet.add x xs

  let ppo = function
    | None -> "-"
    | Some v -> pp_initval v

  let _pp_env env =
    String.concat ", "
       (List.map (fun (loc,v) -> pp_location loc ^ "->" ^ ppo v) env)

  let complete_init hexa iv i =
    let i =
      (* Add the locs `loc` and values `v` inside `iv` to `i` *)
      List.fold_left
        (fun env (loc,v) ->
          if Misc.is_pte loc then (Loc loc,Some (P (Value.to_pte v)))::env
          (* Do not include if the value is default zero *)
          else if Value.to_int v = 0 then env
          else (Loc loc,Some (S (Value.pp_v ~hexa:hexa v)))::env
        ) i iv in
    let already_here =
      List.fold_left
        (fun k (loc,v) ->
          let k = match loc with
          (* Add Loc `s` into `k` if `s` is not a pte address nor a number *)
          | Loc s -> add_some (as_virtual s) k
          (* No process on register *)
          | Reg _  -> k in
          let k = match v with
          (* Add value `s` into `k` if `s` is not a pte nor a number *)
          | Some (S s) -> add_some (as_virtual s) k
          | _ -> k in
          k)
        StringSet.empty i in
    let refer =
      List.fold_left
        (fun k (loc,v) ->
          let k = match loc with
          (* Add Loc `s` into `k` if `s` is a pte or physical address *)
          | Loc s -> add_some (refers_virtual s) k
          (* No process on register *)
          | Reg _ -> k in
          let k = match v with
          (* Add value `s` into `k` if `s` is a pte or physical address *)
          | Some (S s) -> add_some (refers_virtual s) k
          (* Add the associated physical address in a pteval `p` into `k` *)
          | Some (P p) -> add_some (Value.refers_virtual p) k
          | None -> k in
          k)
        StringSet.empty i in
    (* If a `refer` exist but there is no entry,
       then add it into init state `i` *)
    let i =
      StringSet.fold
        (fun x i -> (Loc x,None)::i)
        (StringSet.diff refer already_here)
        i in
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
        specials2 : I.special2 list ;
        specials3 : I.special3 list ;
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
      specials2 = I.specials2;
      specials3 = I.specials3;
      noks = 0;
      env = LocMap.empty;
      friends = RegMap.empty;
      next_addr = 0; }

  let used_register init =
    List.filter_map ( function
      | (Reg (_,r),Some _) -> Some r
      | _ -> None ) init

  let remove_reg_allocator st remove =
    (* Keep those `reg` NOT equals to any in `remove` *)
    let regs = List.filter (fun reg -> not ( List.mem reg remove )) st.regs in
    { st with regs }

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
  type special2 = I.special2
  type special3 = I.special3

  let alloc_special st = match st.specials with
  | [] -> Warn.fatal "No more special registers"
  | r::rs -> r,{ st with specials = rs; }

  let alloc_special2 st = match st.specials2 with
  | [] -> Warn.fatal "No more special registers"
  | r::rs -> r,{ st with specials2 = rs; }

  let alloc_special3 st = match st.specials3 with
  | [] -> Warn.fatal "No more special registers"
  | r::rs -> r,{ st with specials3 = rs; }

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
