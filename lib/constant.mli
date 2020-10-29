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

(** Constants in code *)

type 'scalar t =
  | Concrete of 'scalar
(* Memory cell, with optional tag, optional capability<128:95> and offet *)
  | Symbolic  of (string * string option * int) * int
  | ConcreteVector of int * 'scalar t list
  | Label of Proc.t * string     (* In code *)
  | Tag of string


val mk_sym : string -> 'scalar t
val get_sym : 'scalar t -> string
val mk_vec : int -> 'scalar t list -> 'scalar t
val is_symbol : 'scalar t -> bool
val is_non_mixed_symbol : 'scalar t -> bool
val default_tag : 'scalar t

(* Check  non-concrete constant (and change type!) *)
val check_sym : 'a t -> 'b t

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
