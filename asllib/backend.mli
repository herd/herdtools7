(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

(** This module is the signature of any backend of the ASL interpreter. *)
module type S = sig
  (* Value constructors *)
  (*--------------------*)

  type value
  (** The runtime values that the interpreter should use. *)

  val debug_value : value -> string
  (** A printer for value, should only be used for debugging. *)

  val is_undetermined : value -> bool
  (** [is_undetermined v] returns true when [c] is a non-constant value  *)

  val v_of_parsed_v : AST.value -> value
  (** [v_of_parsed_v] constructs a value from a parsed value.
      Note that the prefered method to create records or any complex values
      is [create_vector], and should be used for constructing complex values. *)

  val v_unknown_of_type : AST.ty -> value
  (** [v_unknwon_of_type t] constructs a value from a type. *)

  val v_of_int : int -> value
  (** [v_of_int] is used to convert raw integers arising from the interpretation,
      and not parsed values. *)

  val v_to_int : value -> int option
  (** [v_to_int v] returns, if possible, an integer corresponding to the value.
      Should be called only on values of type integer. *)

  (* Monadic operators *)
  (*-------------------*)

  type 'a m
  (** Main monad type to chain operations done by the interpreter. *)

  val return : 'a -> 'a m
  (** Monadic constructor. *)

  val warnT : string -> 'a -> 'a m
  (** Add warning message *)

  val bind_data : 'a m -> ('a -> 'b m) -> 'b m
  (** Monadic bind operation, used when data from the first operation is needed
      to compute the second operation. *)

  val bind_seq : 'a m -> ('a -> 'b m) -> 'b m
  (** Monadic bind operation. but that only pass internal interpreter data.
      This should not create any data-dependency. *)

  val bind_ctrl : 'a m -> ('a -> 'b m) -> 'b m
  (** Monadic bind operation, but that creates a control dependency between the
      first argument and the result of the second one. *)

  val prod : 'a m -> 'b m -> ('a * 'b) m
  (** Monadic product operation, two monads are combined "in parrallel".*)

  val choice : value m -> 'b m -> 'b m -> 'b m
  (** choice is a boolean if operator. *)

  val delay : 'a m -> ('a -> 'a m -> 'b m) -> 'b m
  (** delay operator spits monad into result ['a] and
     hidden structure. This permits deciding on
     the monad value, while using hidden structure later *)

  (** Special operations with vectors *)
  (*  --------------------------------*)

  val create_vector : AST.ty -> value list -> value m
  (** Creates a vector, with possible names for the fields *)

  val get_i : int -> value -> value m
  (** [get_i i vec] returns value at index [i] inside [vec].*)

  val set_i : int -> value -> value -> value m
  (** [set_i i v vec] returns [vec] with index [i] replaced by [v].*)

  (** Other operations *)
  (*  -----------------*)

  val binop : AST.binop -> value -> value -> value m
  (** Evaluates the binary operation on those two values. *)

  val unop : AST.unop -> value -> value m
  (** Evaluate this unary operation on this value. *)

  val ternary : value -> (unit -> value m) -> (unit -> value m) -> value m
  (** [ternary v w1 w2] is w1 if v is true and w2 if v is false *)

  val on_read_identifier : AST.identifier -> AST.scope -> value -> unit m
  (** [on_read_identifier] is called when a value is read from the local
      environment.*)

  val on_write_identifier : AST.identifier -> AST.scope -> value -> unit m
  (** [on_write_identifier] is called when a value is read from the local
      environment.*)

  val read_from_bitvector : (value * value) list -> value -> value m
  (** Read a slice (represented by a list of positions) from a bitvector. *)

  val write_to_bitvector : (value * value) list -> value -> value -> value m
  (** [write_to_bitvector positions w v] writes the bits of [w] into [v] at the specified positions. *)

  val concat_bitvectors : value list -> value m
  (** Similar to Bitvector.concat, but monadic style obviously. *)
end
