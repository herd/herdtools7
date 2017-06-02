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


(* Type of propositions and constraint *)
type ('loc,'v) atom =
  | LV of 'loc * 'v
  | LL of 'loc * 'loc

type ('loc,'v) prop =
  | Atom of ('loc, 'v) atom
  | Not of ('loc,'v) prop
  | And of ('loc,'v) prop list
  | Or of ('loc,'v) prop list
  | Implies of ('loc,'v) prop * ('loc,'v) prop

type 'prop constr =
    ForallStates of 'prop
  | ExistsState of 'prop
  | NotExistsState of 'prop

type  ('loc,'v) cond = ('loc,'v) prop constr

val constr_true :  ('loc,'v) cond
val is_true :  ('loc,'v) cond -> bool
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

(* Polymorphic constraint combinators *)

 val fold_prop :
     (('loc, 'v) atom -> 'a -> 'a) -> ('loc,'v) prop -> 'a -> 'a

 val fold_constr :
     (('loc, 'v) atom -> 'a -> 'a) -> ('loc,'v) prop constr -> 'a -> 'a

val  map_prop :
    (('loc1, 'v1) atom -> ('loc2, 'v2) atom) ->
      ('loc1,'v1) prop ->
          ('loc2,'v2) prop

val  map_constr :
    (('loc1, 'v1) atom -> ('loc2, 'v2) atom) ->
      ('loc1,'v1) prop constr ->
          ('loc2,'v2) prop constr

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

val pp_prop : ('loc,'v) atom pp_arg -> ('loc,'v) prop  -> string

val dump_prop :
    (('loc, 'v) atom -> string) -> out_channel -> ('loc,'v) prop -> unit

val prop_to_string :
    (('loc, 'v) atom -> string) -> ('loc,'v) prop -> string

val dump_constraints :
    out_channel -> (('loc, 'v) atom -> string) -> ('loc,'v) prop constr -> unit

val constraints_to_string :
  (('loc, 'v) atom -> string) -> ('loc,'v) prop constr -> string
