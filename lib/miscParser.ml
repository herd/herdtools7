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

type proc = int * string list option

let pp_proc (p,ao) =
  Printf.sprintf
    "P%i%s" p
    (match ao with
    | None -> ""
    | Some a -> Printf.sprintf ":%s" (String.concat "," a))

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

let dump_location = function
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> ParsedConstant.pp_v v

let dump_rval loc = match loc with
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> Printf.sprintf "*%s" (ParsedConstant.pp_v v)

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

type locations = (location,maybev) LocationsItem.t list

type prop = (location, maybev) ConstrGen.prop
type constr = prop ConstrGen.constr
type quantifier = ConstrGen.kind

type atom = location * maybev
type outcome = atom list

open Printf

let pp_atom (loc,v) =
  sprintf "%s=%s" (dump_location loc) (ParsedConstant.pp_v v)

let pp_outcome o =
  String.concat " "
    (List.map (fun a -> sprintf "%s;" (pp_atom a)) o)

type state = (location * (TestType.t * maybev)) list

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

let mk_pte_val pte l =
  let open Constant in
  let s = match pte with
  | Location_global (Symbolic (System (PTE,s))) -> s
  | Location_global (Symbolic (Virtual {tag;offset;_})) ->
    assert (offset = 0) ;
    assert (Misc.is_none tag) ;
    ""
  | Location_reg _ -> ""
  | _ -> Warn.user_error "Expected a PTE or a register" in
  let v = PTEVal.of_list s l in
  PteVal v

let dump_state_atom dump_loc dump_val (loc,(t,v)) =
  let open TestType in
  match t with
  | TyDef ->
      sprintf "%s=%s" (dump_loc loc) (dump_val v)
  | TyDefPointer ->
      sprintf "*%s=%s" (dump_loc loc) (dump_val v)
  | Ty t ->
      sprintf "%s %s=%s" t (dump_loc loc) (dump_val v)
  | Atomic t ->
      sprintf "_Atomic %s %s=%s" t (dump_loc loc) (dump_val v)
  | Pointer t ->
      sprintf "%s *%s=%s" t (dump_loc loc) (dump_val v)
  | TyArray (t,sz) ->
      sprintf "%s %s[%i]" t (dump_loc loc) sz

(* Packed result *)
type info = (string * string) list

(* Some source files contain addditionnal information *)

type extra_data =
  | NoExtra
  | CExtra of CAst.param list list
  | BellExtra of BellInfo.test

let empty_extra = NoExtra

type ('i, 'p, 'prop, 'loc, 'v) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      filter : 'prop option ;
      condition : 'prop ConstrGen.constr ;
      locations : ('loc,'v) LocationsItem.t list ;
      extra_data : extra_data ;
}

(* Easier to handle *)
type ('loc,'v,'ins) r3 =
      (('loc * (TestType.t * 'v)) list,
       (proc * 'ins list) list,
       ('loc, 'v) ConstrGen.prop,
       'loc, 'v) result

type ('loc,'v,'code) r4 =
      (('loc * (TestType.t * 'v)) list,
       'code list,
       ('loc, 'v) ConstrGen.prop,
       'loc, 'v) result

(* Result of generic parsing *)
type 'pseudo t = (state, (proc * 'pseudo list) list, prop, location, maybev) result

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
