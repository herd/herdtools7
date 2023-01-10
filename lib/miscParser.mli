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

(** The basic types of architectures and semantics, just parsed *)

(* Processor name with optional annotations *)
type func = Main | FaultHandler
type proc = Proc.t * string list option * func

val proc_num : proc -> Proc.t
val pp_proc : proc -> string
val count_procs : (proc * 'c) list -> int

(* Values just parsed *)
type maybev = ParsedConstant.v

(* Registers not yet parsed *)
type reg = string

type location =
  | Location_reg of int * reg
  | Location_sreg of string (** symbolic register *)
  | Location_global of maybev

val location_compare : location -> location -> int
val dump_value : maybev -> string
val dump_location : location -> string
val dump_location_brk : location -> string
val is_global : location -> bool
val as_local_proc : int -> StringSet.t -> location -> reg option

val env_for_pp : (location * 'a) list -> (location * 'a) list list

module LocSet : MySet.S with type elt = location
module LocMap : MyMap.S with type key = location

type rlocation = location ConstrGen.rloc
module RLocSet : MySet.S with type elt = rlocation

type fault_type = string
val dump_fault_type : fault_type -> string

type locations = (location,maybev,fault_type) LocationsItem.t list

type prop = (location, maybev, fault_type) ConstrGen.prop
type constr = prop ConstrGen.constr
type quantifier = ConstrGen.kind

type state_atom = location * (TestType.t * maybev)
type state = state_atom list

val check_env_for_dups : state -> unit


val dump_state_atom :
  ('loc -> bool) ->
  ('loc -> string) -> ('v -> string) -> ('loc * (TestType.t * 'v)) -> string

(* Packed result *)
type info = (string * string) list

(* Some source files contain additional information *)

type extra_data =
  | NoExtra
  | CExtra of CAst.param list list
  | BellExtra of BellInfo.test

val empty_extra : extra_data

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
       'loc,'v,'ftype) result

type ('loc,'v,'code,'ftype) r4 =
      (('loc * (TestType.t * 'v)) list,
       'code list,
       ('loc, 'v,'ftype) ConstrGen.prop,
       'loc,'v,'ftype) result

(* Result of generic parsing *)
type 'pseudo t = (state, (proc * 'pseudo list) list, prop, location,maybev, fault_type) result

(* Add empty extra info to machine parsers *)
val mach2generic :
  (('lexbuf -> 'token) -> 'lexbuf -> 'a * 'b) ->
    ('lexbuf -> 'token) -> 'lexbuf -> 'a * 'b * extra_data

(* Info keys *)
val hash_key : string
val stable_key : string
val align_key : string
val tthm_key : string
val cache_type_key : string
val variant_key : string
val user_key : string
val el0_key : string
val memory_type_key : string
val mt_key : string

val key_match : string -> string -> bool

(* Meta-data included in digest ? *)
val digest_mem : string -> bool

(* Extract hash *)
val get_hash : ('i, 'p, 'c, 'loc, 'v, 'ftype) result -> string option
val set_hash :
    ('i, 'p, 'c, 'loc, 'v, 'ftype) result -> string ->
      ('i, 'p, 'c, 'loc, 'v, 'ftype) result


(* Extract meta information from key *)

val get_info_on_info : string -> (string * string) list -> string option

val get_info :  ('i, 'p, 'c, 'loc, 'v, 'ftype) result -> string -> string option

val add_oa_if_none : location -> ParsedPteVal.t -> maybev

val mk_instr_val : string option -> ('scalar,'pte,InstrLit.t) Constant.t
