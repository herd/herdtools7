(******************************************************************************)
(*                                ASLRef                                      *)
(*                                                                            *)
(* Copyright (c) 2022-present, Arm Limited or its affiliates.                 *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* SPDX-License-Identifier: Apache-2.0                                        *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                         *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(** Signature module of the backend of {!Interpreter}. *)

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

  val v_of_literal : AST.literal -> value
  (** [v_of_parsed_v] constructs a value from a parsed value.
      Note that the prefered method to create records or any complex values
      is [create_vector], and should be used for constructing complex values. *)

  val v_unknown_of_type : AST.ty -> value
  (** [v_unknown_of_type t] constructs a value from a type. *)

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
  (** Monadic bind operation, but that only passes internal interpreter data.
      This should not create any data-dependency. *)

  val bind_ctrl : 'a m -> ('a -> 'b m) -> 'b m
  (** Monadic bind operation, but that creates a control dependency between the
      first argument and the result of the second one. *)

  val prod_par : 'a m -> 'b m -> ('a * 'b) m
  (** Monadic product operation, two monads are combined "in parallel".*)

  val appl_data : 'a m -> ('a -> 'b) -> 'b m
  (** Applicative map. 
  
      Creates a data dependency between the output events and
      the input events of the argument in the resulting monad. *)

  val choice : value m -> 'b m -> 'b m -> 'b m
  (** choice is a boolean if operator. *)

  val delay : 'a m -> ('a -> 'a m -> 'b m) -> 'b m
  (** delay operator spits monad into result ['a] and
     hidden structure. This permits deciding on
     the monad value, while using hidden structure later *)

  (** Special operations with vectors *)
  (*  --------------------------------*)

  val create_vector : value list -> value m
  (** Creates a vector with this values. *)

  val create_record : (AST.identifier * value) list -> value m
  (** Creates a record, with the indicated names. *)

  val create_exception : (AST.identifier * value) list -> value m
  (** Creates an exception, with the indicated names. *)

  val get_index : int -> value -> value m
  (** [get_i i vec] returns value at index [i] inside [vec].*)

  val set_index : int -> value -> value -> value m
  (** [set_i i v vec] returns [vec] with index [i] replaced by [v].*)

  val get_field : string -> value -> value m
  (** [get_field "foo" v] is the value mapped by "foo" in the record [v]. *)

  val set_field : string -> value -> value -> value m
  (** [set_field "foo" v record] is [record] with "foo" mapping to [v]. *)

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

  val bitvector_length : value -> value m
  (** Get the length of a bitvector. *)

  type primitive = value m list -> value m list m
  (** primitive types that go with this AST. *)

  type ast = primitive AST.t
  (** The considered AST type. *)
end
