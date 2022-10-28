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

(* The basic types of architectures and semantics, just parsed *)

open Printf

type func = Main | FaultHandler
type proc = Proc.t * string list option * func

let pp_proc (p,ao,f) =
  sprintf
    "P%i%s%s" p
    (match f with
     | Main -> ""
     | FaultHandler -> ".F")
    (match ao with
    | None -> ""
    | Some a -> sprintf ":%s" (String.concat "," a))

let count_procs prog =
  List.fold_left
    (fun n (((_,_,f):proc),_) ->
      match f with
      | Main -> n+1
      | FaultHandler -> n)
    0 prog

type maybev = ParsedConstant.v

type reg = string (* Registers not yet parsed *)

type location =
  | Location_reg of int * reg
  | Location_sreg of string
  | Location_global of maybev

let location_compare loc1 loc2 = match loc1,loc2 with
| Location_reg (i1,r1), Location_reg (i2,r2) ->
    begin match Misc.int_compare i1 i2 with
    | 0 -> String.compare r1 r2
    | c -> c
    end
| Location_sreg r1,Location_sreg r2 ->
    String.compare r1 r2
| Location_global v1,Location_global v2 ->
    ParsedConstant.compare v1 v2
| Location_reg _,(Location_sreg _|Location_global _) -> -1
| (Location_sreg _|Location_global _),Location_reg _ -> 1
| Location_sreg _, Location_global  _ -> -1
| Location_global _, Location_sreg _ -> 1

let dump_value = ParsedConstant.pp_v

let dump_location = function
  | Location_reg (i,r) -> sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> ParsedConstant.pp_v v

let dump_location_brk = function
  | Location_reg (i,r) -> sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> sprintf "[%s]" (ParsedConstant.pp_v v)

let is_global = function
  | Location_global _ -> true
  | Location_reg _
  | Location_sreg _ -> false

let as_local_proc i syms = function
  | Location_reg (j,reg) -> if i=j then Some reg else None
  | Location_global _ -> None
  | Location_sreg reg ->
      if StringSet.mem reg syms then
        Some (Misc.dump_symbolic reg)
      else None

let env_for_pp env =
  Misc.group_by_int
    (fun loc ->
      match loc with
      | Location_reg (proc,_) -> Some proc
      | Location_global _|Location_sreg _ -> None)
    env

module LocSet =
  MySet.Make
    (struct type t = location let compare = location_compare end)

module LocMap =
  MyMap.Make
    (struct type t = location let compare = location_compare end)

type rlocation = location ConstrGen.rloc
module RLocSet =
  MySet.Make
    (struct
      type t = rlocation
      let compare = ConstrGen.compare_rloc location_compare
    end)

type fault_type = string
let dump_fault_type ft = ft

type locations = (location,maybev,fault_type) LocationsItem.t list

type prop = (location, maybev, fault_type) ConstrGen.prop
type constr = prop ConstrGen.constr
type quantifier = ConstrGen.kind

type state_atom = location * (TestType.t * maybev)
type state = state_atom list

(* Check that initialisations are unique *)

let check_env_for_dups env =
  let bad,_ =
    List.fold_left
      (fun (bad,seen) (loc,_) ->
        if LocSet.mem loc seen then LocSet.add loc bad,seen
        else bad,LocSet.add loc seen)
      (LocSet.empty,LocSet.empty)
      env in
  if not (LocSet.is_empty bad) then begin
    match LocSet.as_singleton bad with
    | Some loc ->
        Warn.user_error
          "Location %s is initialized more than once"
          (dump_location loc)
    | None ->
        Warn.user_error
          "Locations {%s} are initialized more than once"
          (LocSet.pp_str "," dump_location bad)
  end

let dump_state_atom is_global dump_loc dump_val (loc,(t,v)) =
  let open TestType in
  match t with
  | TyDef ->
     if is_global loc then
       sprintf "[%s]=%s" (dump_loc loc) (dump_val v)
     else
      sprintf "%s=%s" (dump_loc loc) (dump_val v)
  | TyDefPointer ->
      sprintf "*%s=%s" (dump_loc loc) (dump_val v)
  | Ty "pteval_t" when is_global loc ->
      sprintf "[%s]=%s" (dump_loc loc) (dump_val v)
  | Ty t ->
      sprintf "%s %s=%s" t (dump_loc loc) (dump_val v)
  | Atomic t ->
      sprintf "_Atomic %s %s=%s" t (dump_loc loc) (dump_val v)
  | Pointer t ->
      sprintf "%s *%s=%s" t (dump_loc loc) (dump_val v)
  | TyArray (t,sz) ->
      sprintf "%s %s[%i]=%s" t (dump_loc loc) sz (dump_val v)

(* Packed result *)
type info = (string * string) list

(* Some source files contain addditionnal information *)

type extra_data =
  | NoExtra
  | CExtra of CAst.param list list
  | BellExtra of BellInfo.test

let empty_extra = NoExtra

type ('i, 'p, 'prop, 'loc, 'v, 'ftype) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      filter : 'prop option ;
      condition : 'prop ConstrGen.constr ;
      locations : ('loc,'v,'ftype) LocationsItem.t list ;
      extra_data : extra_data ;
}

(* Easier to handle *)
type ('loc,'v,'ins,'ftype) r3 =
      (('loc * (TestType.t * 'v)) list,
       (proc * 'ins list) list,
       ('loc, 'v, 'ftype) ConstrGen.prop,
       'loc, 'v, 'ftype) result

type ('loc,'v,'code,'ftype) r4 =
      (('loc * (TestType.t * 'v)) list,
       'code list,
       ('loc, 'v, 'ftype) ConstrGen.prop,
       'loc, 'v, 'ftype) result

(* Result of generic parsing *)
type 'pseudo t = (state, (proc * 'pseudo list) list, prop, location, maybev, fault_type) result

(* Add empty GPU/Bell info to machine parsers *)

let mach2generic parser lexer buff =
    let procs,code = parser lexer buff in
    procs,code,NoExtra

(* Info keys *)

let hash_key =  "Hash"
and stable_key = "Stable"
and align_key = "Align"
and tthm_key = "TTHM"
and variant_key = "Variant"
and user_key = "user"
and el0_key = "el0"
and memory_type_key = "MemoryType"
and mt_key = "MT"

let key_match k1 k2 =
  let len1 = String.length k1 in
  let len2 = String.length k2 in
  Misc.int_eq len1 len2 &&
    begin
      let len = len1 in
      let rec do_rec k =
        if k >= len then true
        else
          Misc.char_uppercase k1.[k] = Misc.char_uppercase k2.[k]
          && do_rec (k+1) in
      do_rec 0
    end

let digested_keys = [memory_type_key; mt_key;]

let digest_mem k = List.exists (key_match k) digested_keys

let get_info_on_info key =
  let rec find = function
    | [] -> None
    | (k,v)::rem ->
        if key_match k key then Some v
        else find rem in
  find

(* get hash from info fields *)
let get_hash p = get_info_on_info hash_key p.info

let rec set_hash_rec h = function
  | [] -> [hash_key,h]
  | (k,_)::rem when key_match hash_key k -> (k,h)::rem
  | p::rem -> p::set_hash_rec h rem

let set_hash p h = { p with info = set_hash_rec  h p.info; }

let get_info p key = get_info_on_info key p.info

let add_oa_if_none loc p =
  let open Constant in
  try
    let oa =
      match loc with
      | Location_global (Symbolic (Virtual s)) ->
         begin match Misc.tr_pte s.name with
           | Some s -> OutputAddress.PHY s
           | None -> raise Exit
         end
      | _ -> raise Exit in
    let p = ParsedPteVal.add_oa_if_none oa p in
    Constant.PteVal p
  with Exit -> PteVal p

let mk_instr_val v =
  match v with
  | "NOP" -> Constant.Instruction(InstrLit.LIT_NOP)
  | _ -> Warn.user_error "unexpected {%s} value while parsing an instruction" v
