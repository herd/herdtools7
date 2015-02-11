(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** The basic types of architectures and semantics, just parsed *)

type maybev = SymbConstant.v

type reg = string (* Registers not yet parsed *)

type location =
  | Location_reg of int * reg
  | Location_sreg of string (** symbolic register *)
  | Location_global of maybev

val location_compare : location -> location -> int
val dump_location : location -> string
val dump_rval : location -> string
val is_global : location -> bool
val as_local_proc : int -> location -> reg option

module LocSet : MySet.S with type elt = location


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

val pp_run_type : run_type -> string

type state = (location * (run_type * maybev)) list

val dump_state_atom :
  ('loc -> string) -> ('v -> string) -> ('loc * (run_type * 'v)) -> string

(* Packed result *)
type info = (string * string) list
type gpu_data = {
      scope_tree : ScopeTree.scope_tree option ;
      mem_space_map : MemSpaceMap.mem_space_map ;
      param_map : CAst.param list list ;
  }

val empty_gpu : gpu_data

type ('i, 'p, 'c, 'loc) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      condition : 'c ;
      locations : ('loc * run_type) list ;
      gpu_data : gpu_data option ;
}

(* Easier to handle *)
type ('loc,'v,'ins) r3 =
       (('loc * (run_type * 'v)) list,
       (int * 'ins list) list,
       ('loc, 'v) ConstrGen.prop ConstrGen.constr,
       'loc) result

type ('loc,'v,'code) r4 =
      (('loc * (run_type * 'v)) list,
       'code list,
       ('loc, 'v) ConstrGen.prop ConstrGen.constr,
       'loc) result

(* Result of generic parsing *)
type 'pseudo t =
    (state, (int * 'pseudo list) list, constr, location) result


(* Extract hash *)
val get_hash :  ('i, 'p, 'c, 'loc) result -> string option
