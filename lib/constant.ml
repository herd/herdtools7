(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Constants in code *)

type 'scalar t =
  | Concrete of 'scalar
(* Memory cell, with optional tag, capability<128:95> and offet *)
  | Symbolic  of (string * string option * int) * int
  | ConcreteVector of int * 'scalar t list
  | Label of Proc.t * string     (* In code *)
  | Tag of string

let mk_sym s = Symbolic ((s,None,0),0)

and get_sym = function
  | Symbolic ((s,_,_),_) -> s
  | Concrete _|Label _| Tag _ | ConcreteVector _ -> assert false

let mk_vec sz v =
  assert (sz == (List.length v));
  ConcreteVector (sz, v)

let is_symbol = function
  | Symbolic _ -> true
  | Concrete _|ConcreteVector _| Label _| Tag _ -> false

let is_non_mixed_symbol = function
  | Symbolic (_,idx) -> true(*idx=0 problem: REVIEWER PLEASE READ we emit vector elements*)
  (* as v+0, v+8, v+16 etc... which is false with idx=0 forall i > 0*)
  (* this is valid in non-mixed mode: we should return false only*)
  (* when the offset is not byte aligned to the size of the prim *)
  (* type in the array e.g idx=6 => False, when prim size = uint64 = 8 *)
  (* however idx=16/0 is fine*)
  (* this is because herd/mem uses this function after we elaborate*)
  (* the ConcreteVector below into location_globals in build_state*)
  (* this is correct as Loc_global has the vector semantics we need!*)
  (* see test A130 for the syntax semantics problem*)
  (* however we lose the metadata we need to check alignment *)
  (* for now I have removed the symbolic check here, but this is wrong*)
  (* and I'm tired *)
  (* if this was a compiler we would/should split out the elaboration*)
  (* phase from herd/archExtra and pass metadata about alignment forward*)
  | Concrete _|Label _| Tag _| ConcreteVector _ -> true

let default_tag = Tag "green"

let check_sym v =  match v with
| Concrete _|ConcreteVector (_,_) ->  assert false
| Symbolic _|Label _|Tag _ as sym -> sym

module type S =  sig

  module Scalar : Scalar.S

  type v = Scalar.t t
  val intToV  : int -> v
  val nameToV  : string -> v
  val zero : v
  val one : v
  val bit_at : int -> Scalar.t -> Scalar.t
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
  val vToName : v -> string
end
