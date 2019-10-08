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

type maybev = ParsedConstant.v

type reg = string (* Registers not yet parsed *)

type location =
  | Location_reg of int * reg
  | Location_sreg of string (** symbolic register *)
  | Location_global of maybev
  | Location_deref of maybev * int

val location_compare : location -> location -> int
val dump_location : location -> string
val dump_rval : location -> string
val is_global : location -> bool
val as_local_proc : int -> StringSet.t -> location -> reg option

module LocSet : MySet.S with type elt = location
module LocMap : MyMap.S with type key = location


type prop = (location, maybev) ConstrGen.prop
type constr = prop ConstrGen.constr
type quantifier = ConstrGen.kind

type atom = location * maybev
type outcome = atom list

val pp_atom : atom -> string
val pp_outcome : outcome -> string

type run_type =
  | TyDef | TyDefPointer
  | Ty of string | Pointer of string
  | TyArray of string * int
  | Atomic of string

val pp_run_type : run_type -> string

type state = (location * (run_type * maybev)) list

val dump_state_atom :
  ('loc -> string) -> ('v -> string) -> ('loc * (run_type * 'v)) -> string

(* Packed result *)
type info = (string * string) list

(* Some source files contain additional information *)

type extra_data =
  | NoExtra
  | CExtra of CAst.param list list
  | BellExtra of BellInfo.test

val empty_extra : extra_data

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
       (int * 'ins list) list,
       ('loc, 'v) ConstrGen.prop,
       'loc) result

type ('loc,'v,'code) r4 =
      (('loc * (run_type * 'v)) list,
       'code list,
       ('loc, 'v) ConstrGen.prop,
       'loc) result

(* Result of generic parsing *)
type 'pseudo t = (state, (int * 'pseudo list) list, prop, location) result

(* Add empty extra info to machine parsers *)
val mach2generic :
  (('lexbuf -> 'token) -> 'lexbuf -> 'a * 'b) ->
    ('lexbuf -> 'token) -> 'lexbuf -> 'a * 'b * extra_data

(* Info keys *)
val hash_key : string
val stable_key : string

(* Extract hash *)
val get_hash : ('i, 'p, 'c, 'loc) result -> string option
val set_hash :
    ('i, 'p, 'c, 'loc) result -> string ->
      ('i, 'p, 'c, 'loc) result

(* Extract meta information from key *)
val get_info :  ('i, 'p, 'c, 'loc) result -> string -> string option
