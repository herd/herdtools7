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
  | Location_deref of maybev * int

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
| Location_deref (v1,i1),Location_deref (v2,i2) ->
    begin match ParsedConstant.compare v1 v2 with
    | 0 -> Misc.int_compare i1 i2
    | r -> r
    end
| Location_reg _,(Location_sreg _|Location_global _|Location_deref _) -> -1
| (Location_sreg _|Location_global _|Location_deref _),Location_reg _ -> 1
| Location_sreg _, (Location_global _|Location_deref _) -> -1
| (Location_global _|Location_deref _), Location_sreg _ -> 1
| Location_global  _,Location_deref _ -> -1
| Location_deref  _,Location_global _ -> 1

let dump_location = function
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> ParsedConstant.pp_v v
  | Location_deref (v,i) ->
      Printf.sprintf "%s[%i]" (ParsedConstant.pp_v v) i

let dump_rval loc = match loc with
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> Misc.dump_symbolic s
  | Location_global v -> Printf.sprintf "*%s" (ParsedConstant.pp_v v)
  | Location_deref _ -> assert false

let is_global = function
  | Location_global _
  | Location_deref _ -> true
  | Location_reg _
  | Location_sreg _ -> false

let as_local_proc i syms = function
  | Location_reg (j,reg) -> if i=j then Some reg else None
  | Location_global _|Location_deref _ -> None
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

type run_type =
  | TyDef | TyDefPointer
  | Ty of string | Pointer of string
  | TyArray of string * int
  | Atomic of string

let pp_run_type = function
  | TyDef -> "TyDef"
  | TyDefPointer -> "TyDefPointer"
  | Ty s -> sprintf "Ty<%s>" s
  | Atomic s -> sprintf "Atomic<%s>" s
  | Pointer s -> sprintf "Pointer<%s>" s
  | TyArray (s,sz) -> sprintf "TyArray<%s,%i>" s sz

type state = (location * (run_type * maybev)) list

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
  | Location_global (Symbolic (Virtual ((_,t),i))) ->
    assert (i = 0) ;
    assert (Misc.is_none t) ;
    ""
  | Location_reg _ -> ""
  | _ -> Warn.user_error "Expected a PTE or a register" in
  let v = PTEVal.of_list s l in
  PteVal v

let dump_state_atom dump_loc dump_val (loc,(t,v)) = match t with
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

type ('i, 'p, 'prop, 'loc) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      filter : 'prop option ;
      condition : 'prop ConstrGen.constr ;
      locations : ('loc * run_type) list ;
      extra_data : extra_data ;
}

(* Easier to handle *)
type ('loc,'v,'ins) r3 =
      (('loc * (run_type * 'v)) list,
       (proc * 'ins list) list,
       ('loc, 'v) ConstrGen.prop,
       'loc) result

type ('loc,'v,'code) r4 =
      (('loc * (run_type * 'v)) list,
       'code list,
       ('loc, 'v) ConstrGen.prop,
       'loc) result

(* Result of generic parsing *)
type 'pseudo t = (state, (proc * 'pseudo list) list, prop, location) result

(* Add empty GPU/Bell info to machine parsers *)

let mach2generic parser lexer buff =
    let procs,code = parser lexer buff in
    procs,code,NoExtra

(* Info keys *)
let hash_key =  "Hash"
and stable_key = "Stable"
and align_key = "Align"
and tthm_key = "TTHM"

let low_hash = "hash"

let get_info_on_info key =
  let key = Misc.lowercase key in
  let rec find = function
    | [] -> None
    | (k,v)::rem ->
        if Misc.string_eq (Misc.lowercase k) key then Some v
        else find rem in
  find

(* get hash from info fields *)
let get_hash p = get_info_on_info hash_key p.info

let rec set_hash_rec h = function
  | [] -> [hash_key,h]
  | (k,_)::rem when Misc.string_eq (Misc.lowercase k) low_hash -> (k,h)::rem
  | p::rem -> p::set_hash_rec h rem

let set_hash p h = { p with info = set_hash_rec  h p.info; }


let get_info p key = get_info_on_info key p.info
