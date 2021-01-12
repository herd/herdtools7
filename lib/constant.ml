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

(*Metadata for when  a constant is a vector element *)
(*vector metadata - prim size (arg1) and total array size (arg2) *)
(*needed for is-non-mixed-symbol and global vectors *)

type tag = string option
type cap = int
type offset = int

(* Symbolic location metadata*)
(* Memory cell, with optional tag, capability<128:95>,optional vector metadata, and offset *)
type symbolic_data =
  {
   name : string ;
   tag : tag ;
   cap : cap ;
   offset : offset ;
  }

type 'scalar t =
  | Concrete of 'scalar
  | Symbolic  of symbolic_data
  | ConcreteVector of int * 'scalar t list
  | Label of Proc.t * string     (* In code *)
  | Tag of string

let default_symbolic_data =
  {
   name = "" ;
   tag = None ;
   cap = 0 ;
   offset = 0 ;
  }

let mk_sym s = Symbolic {default_symbolic_data with name=s }

and get_sym = function
  | Symbolic s -> s.name
  | Concrete _|Label _| Tag _ | ConcreteVector _ -> assert false

let mk_vec sz v =
  assert (sz == (List.length v));
  ConcreteVector (sz, v)

let is_symbol = function
  | Symbolic _ -> true
  | Concrete _|ConcreteVector _| Label _| Tag _ -> false

(* idx into array, check idx aligned to prim-size ps*)
(* and idx is less than total array size ts*)
(* this is not the same as herd mixed mode which may not align exactly to the size of the prim-type -> this is used for non-mixed mode too*)
let is_aligned_to_vec (ps,ts) idx =
  if idx > 0 then
    (ps >= idx && (ps mod idx = 0) && idx < (ts*ps))
    || ((idx mod ps = 0) && idx < (ts*ps))
  else idx=0

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
