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

(** Generic part of constraint (ie the one to be checked
   at the end of test run) *)

(* Type of locations on the right of LV atoms *)

type 'loc rloc =
  | Loc of 'loc
  | Deref of 'loc * int

val dump_rloc : ('loc -> string) -> 'loc rloc -> string

val compare_rloc : ('loc -> 'loc -> int) -> 'loc rloc -> 'loc rloc -> int

val rloc_of_loc : 'loc -> 'loc rloc
val loc_of_rloc : 'loc rloc -> 'loc

val map_rloc : ('a -> 'b) -> 'a rloc -> 'b rloc

val fold_rloc : ('loc -> 'b -> 'c) -> 'loc rloc -> 'b -> 'c

val match_rloc :
  ('loc -> 'r) -> ('loc -> int -> 'r) -> 'loc rloc -> 'r

(* Type of propositions and constraint *)
type ('loc,'v,'ftype) atom =
  | LV of 'loc rloc * 'v
  | LL of 'loc * 'loc
  | FF of ('v,'ftype) Fault.atom

val dump_atom :
  ('loc -> string) ->  ('loc -> string) -> (('c,'d) Constant.t -> string) ->
  ('ftype -> string) -> ('loc,('c,'d) Constant.t,'ftype) atom -> string

type ('loc,'v, 'ftype) prop =
  | Atom of ('loc, 'v, 'ftype) atom
  | Not of ('loc,'v,'ftype) prop
  | And of ('loc,'v,'ftype) prop list
  | Or of ('loc,'v,'ftype) prop list
  | Implies of ('loc,'v,'ftype) prop * ('loc,'v,'ftype) prop

type 'prop constr =
    ForallStates of 'prop
  | ExistsState of 'prop
  | NotExistsState of 'prop

type  ('loc,'v,'ftype) cond = ('loc,'v,'ftype) prop constr

val constr_true :  ('loc,'v,'ftype) cond
val is_true :  ('loc,'v,'ftype) cond -> bool
val is_existential : 'prop constr -> bool
val prop_of : 'prop constr -> 'prop

(* val dnf : ('loc,'v) prop -> ('loc,'v) prop list list *)

(* Style of constraints in papers *)
type kind =
  | Allow
  | Forbid
  | Require
  | Unconstrained

val kind_of : 'a constr -> kind
val set_kind : kind -> 'a constr -> 'a constr
val pp_kind : kind -> string
val parse_kind : string -> kind option
val compare_kind : kind -> kind -> int

(* Polymorphic constraint combinators *)

 val fold_prop :
     (('loc, 'v, 'ftype) atom -> 'a -> 'a) -> ('loc,'v,'ftype) prop -> 'a -> 'a

 val fold_constr :
     (('loc, 'v, 'ftype) atom -> 'a -> 'a) -> ('loc,'v,'ftype) prop constr -> 'a -> 'a

val  map_prop :
    (('loc1, 'v1, 'ftype1) atom -> ('loc2, 'v2, 'ftype2) atom) ->
      ('loc1,'v1, 'ftype1) prop ->
          ('loc2,'v2,'ftype2) prop

val  map_constr :
    (('loc1, 'v1, 'ftype1) atom -> ('loc2, 'v2, 'ftype2) atom) ->
      ('loc1,'v1,'ftype1) prop constr ->
          ('loc2,'v2,'ftype2) prop constr

(* Pretty print *)

type 'atom pp_arg =
    { pp_true : string;
      pp_false : string;
      pp_not : string;
      pp_or : string;
      pp_and : string;
      pp_implies : string;
      pp_mbox : string -> string;
      pp_atom : 'atom -> string; }

val pp_prop : ('loc,'v,'ftype) atom pp_arg -> ('loc,'v,'ftype) prop  -> string

val dump_prop :
    (('loc, 'v, 'ftype) atom -> string) -> out_channel -> ('loc,'v,'ftype) prop -> unit

val prop_to_string :
    (('loc, 'v, 'ftype) atom -> string) -> ('loc,'v,'ftype) prop -> string

val dump_constraints :
    out_channel -> (('loc,'v,'ftype) atom -> string) -> ('loc,'v,'ftype) prop constr -> unit

val constraints_to_string :
  (('loc, 'v, 'ftype) atom -> string) -> ('loc,'v, 'ftype) prop constr -> string
