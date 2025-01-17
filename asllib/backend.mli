(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
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

module type SCOPE = sig
  type t
  (** Data making a subprogram call unique, if needed. *)

  val global : init:bool -> t
  (** The global scope during runtime or init, depending on [init] flag. *)

  val new_local : AST.identifier -> t
  (** [new_local_scope subprogram_name] returns a new identifier for the call
      to the function named [subprogram_name]. *)
end

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

  val v_of_int : int -> value
  (** [v_of_int] is used to convert raw integers arising from the interpretation,
      and not parsed values. *)

  val v_to_z : value -> Z.t option
  (** [v_to_z v] returns, if possible, an integer corresponding to the value.
      Should be called only on values of type integer. *)

  val v_to_label : value -> string
  (** [v_to_label v] returns the identifier of the label nested in the literal inside [v].
      Should be called only on values of type [NV_Literal] where the literal is of type [L_Label]. *)

  (* Monadic operators *)
  (*-------------------*)

  type 'a m
  (** Main monad type to chain operations done by the interpreter. *)

  val return : 'a -> 'a m
  (** Monadic constructor. *)

  val cutoffT : string -> 'a -> 'a m
  (** Flag loop unrolling pruning *)

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

  val debugT : string -> 'a m -> 'a m
  (** Print representation of monad on stderr *)

  val commit : string option -> unit m
  (** Branching event *)

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

  val v_unknown_of_type :
    eval_expr_sef:(AST.expr -> value m) -> AST.ty -> value m
  (** [v_unknown_of_type ~eval_expr_sef t] constructs an arbitrary value from a
      type. *)

  val binop : AST.binop -> value -> value -> value m
  (** Evaluates the binary operation on those two values. *)

  val unop : AST.unop -> value -> value m
  (** Evaluate this unary operation on this value. *)

  val ternary : value -> (unit -> value m) -> (unit -> value m) -> value m
  (** [ternary v w1 w2] is w1 if v is true and w2 if v is false *)

  module Scope : SCOPE

  val on_read_identifier : AST.identifier -> Scope.t -> value -> unit m
  (** [on_read_identifier] is called when a value is read from the local
      environment.*)

  val on_write_identifier : AST.identifier -> Scope.t -> value -> unit m
  (** [on_write_identifier] is called when a value is read from the local
      environment.*)

  type value_range = value * value
  (** Represents a range by its first accessed index and its length. *)

  val read_from_bitvector : value_range list -> value -> value m
  (** Read a slice (represented by a list of value ranges) from a bitvector. *)

  val write_to_bitvector : value_range list -> value -> value -> value m
  (** [write_to_bitvector value_ranges w v] writes the bits of [w] into [v] at
      the positions specified by [value_range]. *)

  val concat_bitvectors : value list -> value m
  (** Similar to Bitvector.concat, but monadic style obviously. *)

  val bitvector_length : value -> value m
  (** Get the length of a bitvector. *)

  type primitive = value m list -> value m list -> value m list m
  (** primitive types that go with this AST.
      First argument is list of parameters,
      second argument is list of arguments. *)

  val primitives : (AST.func * primitive) list
  (** The list of primitives that a backend provides. *)
end
