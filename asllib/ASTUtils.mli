(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
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

(** This module provides some tools to work on ASL ASTs. *)

open AST

(** {1 Identifiers utils} *)
(*------------------------*)

(** An extended [identifier] set. *)
module ISet : sig
  include Set.S with type elt = identifier

  val unions : t list -> t
  (** Iterated union. *)

  val of_option : identifier option -> t
  val pp_print : Format.formatter -> t -> unit
end

(** An extended [identifier] map. *)
module IMap : sig
  include Map.S with type key = identifier

  val of_list : (key * 'a) list -> 'a t

  val pp_print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(** {1 Position utils} *)
(*---------------------*)

val dummy_pos : Lexing.position
(** A dummy position. *)

val default_version : version
(** The default version, [V1]. *)

val annotated : 'a -> position -> position -> version -> 'a annotated
(** [annotated v start end version] is [v] with location specified as from
    [start] to [end] and version specified by [version]. *)

val desc : 'a annotated -> 'a
(** [desc v] is [v.desc] *)

val add_dummy_annotation : ?version:version -> 'a -> 'a annotated
(** Add a dummy annotation to a value. The default version is [default_version].
*)

val dummy_annotated : unit annotated
(** A dummy annotation *)

val is_dummy_annotated : 'a annotated -> bool
(** Returns true if its argument is annotated with [dummy_pos]. *)

val to_pos : 'a annotated -> unit annotated
(** Removes the value from an annotated record. *)

val add_pos_from_pos_of : (string * int * int * int) * 'a -> 'a annotated
(** [add_pos_from_pos_of (__POS_OF__ e)] is [annotated s s' e] where [s] and
    [s'] correspond to [e]'s position in the ocaml file. *)

val add_pos_from : 'a annotated -> 'b -> 'b annotated
(** [add_pos_from loc v] is [v] with the location data from [loc]. *)

val add_pos_range_from : 'a annotated -> 'a annotated -> 'b -> 'b annotated
(** [add_pos_range_from loc_from loc_to v] is [v] with the location data given
    by the range of locations starting at [loc_from] and ending to [loc_to]. *)

val add_pos_from_st : 'a annotated -> 'a -> 'a annotated
(** [add_pos_from_st a' a] is [a] with the location from [a'].

    If both arguments are physically equal, then the result is also physically
    equal. *)

val map2_desc :
  ('a annotated -> 'b annotated -> 'c) ->
  'a annotated ->
  'b annotated ->
  'c annotated
(** Folder on two annotated types. *)

(** {1 Type utils} *)
(*-----------------*)

val integer : ty
(** The ASL unconstrained integer type. *)

val integer' : type_desc
(** [integer], without the position annotation. *)

val integer_exact : ?loc:'a annotated -> expr -> ty
(** [integer_exact e] is the integer type constrained to be equal to [e]. *)

val integer_exact' : expr -> type_desc
(** [integer_exact' e] is [integer_exact e] without the position annotation. *)

val integer_range : ?loc:'a annotated -> expr -> expr -> ty
(* [integer_range e1 e2] is the integer type constrained to be in the range
   [e1..e2]. *)

val integer_range' : expr -> expr -> type_desc
(** [integer_range' e1 e2] is [integer_range e1 e2] without the position
    annotation. *)

val well_constrained :
  ?loc:'a annotated ->
  ?precision:precision_loss_flag ->
  int_constraint list ->
  ty
(** [make_well_constrained cs] builds a constrained integer type with
    constraints [cs]. *)

val well_constrained' :
  ?precision:precision_loss_flag -> int_constraint list -> type_desc
(** [well_constrained' cs] is the [well_constrained cs], without the position
    annotation. *)

val boolean : ty
(** The ASL boolean type. *)

val string : ty
(** The ASL string type. *)

val real : ty
(** The ASL real type. *)

val default_t_bits : type_desc
val default_array_ty : type_desc

(** {1 Constructor helpers} *)
(*--------------------------*)

val s_pass : stmt
(** An ASL pass statement. *)

val s_then : stmt -> stmt -> stmt
(** [s_then s1 s2] is [s1; s2] in ASL. *)

val stmt_from_list : stmt list -> stmt
(** [stmt_from_list [s1; ... sn]] is [s1; ... sn] in ASL. *)

val expr_of_int : int -> expr
(** [expr_of_int i] is the literal expression containing [i]. *)

val literal : literal -> expr
(** [literal v] is the expression evaluated to [v]. *)

val var_ : identifier -> expr
(** [var_ x] is the expression [x]. *)

val binop : binop -> expr -> expr -> expr
(** Builds a binary operation from to subexpressions. *)

val unop : unop -> expr -> expr
(** Builds a unary operation from its subexpression. *)

val expr_of_z : Z.t -> expr
(** [expr_of_z z] is the integer literal for [z]. *)

val zero_expr : expr
(** The integer literal for [0]. *)

val one_expr : expr
(** The integer literal for [1]. *)

val minus_one_expr : expr
(** The integer literal for [-1]. *)

val expr_of_rational : Q.t -> expr
(** [expr_of_rational q] is the rational literal for [q]. *)

val expr_of_bool : bool -> expr
(** [expr_of_bool b] is the boolean literal for [b]. *)

val mul_expr : expr -> expr -> expr
(** [mul_expr e1 e2] is an expression representing [e1 * e2]. *)

val pow_expr : expr -> int -> expr
(** [pow_expr e i] is an expression representing [e ^ i]. *)

val div_expr : expr -> Z.t -> expr
(** [div_expr e z] is an expression representing [e DIV z]. *)

val add_expr : expr -> int * expr -> expr
(** [add_expr e1 (s, e2)] is an expression representing [e1 + sign(s) * e2].
    [e2] is expected to be non-negative. *)

val conj_expr : expr -> expr -> expr
(** [conj_expr e1 e2] is an expression representing [e1 && e2]. *)

val cond_expr : expr -> expr -> expr -> expr
(** [cond_expr e e1 e2] is an expression representing [if e then e1 else e2]. *)

val precision_join :
  precision_loss_flag -> precision_loss_flag -> precision_loss_flag
(** [precision_join p1 p2] is the precision lost by [p1] or [p2]. *)

val register_precision_loss :
  precision_loss_flag -> delayed_warning -> precision_loss_flag
(** [register_lost_precision p warn] register that a precision [p] has lost
    precision with the delayed warning [warn]. *)

val fresh_var : string -> identifier
(** [fresh_var "doc"] is a fresh variable whose name begins with "doc". *)

val global_ignored : unit -> identifier
(** Creates a fresh dummy variable for a global ignored variable. *)

val local_ignored : unit -> identifier
(** Creates a fresh dummy variable for a local ignored variable. *)

val string_starts_with : prefix:string -> string -> bool
(** A copy of String.starts_with out of stdlib 4.12 *)

val is_global_ignored : identifier -> bool
(** [is_global_ignored s] is true iff [s] has been created with
    [global_ignored ()]. *)

val is_local_ignored : identifier -> bool
(** [is_local_ignored s] is true iff [s] has been created with
    [local_ignored ()]. *)

val is_noreturn : func -> bool
(** [is_noreturn f] is true iff [f] was declared with the [noreturn] qualifier.
*)

(** {1 Fields, masks and slices handling} *)

val mask_from_set_bits_positions : int -> int list -> string
(** Builds a mask from specified positions. *)

val inv_mask : string -> string
(** Flip all the 0/1 in the mask. Doesn't change the 'x'. *)

val slices_to_positions : ('a -> int) -> ('a * 'a) list -> int list
(** [slices_to_positions as_int slices] evaluates [slices] and returns a list of
    all queried positions in the correct order. *)

val canonical_fields : (string * 'a) list -> (string * 'a) list
(** Sorts the fields of a record to allow an element wise comparison. *)

val bitfield_get_name : bitfield -> string
(** Returns the name of the bitfield in question. *)

val bitfield_get_slices : bitfield -> slice list
(** Returns the slices corresponding to this bitfield. *)

val bitfield_get_nested : bitfield -> bitfield list
(** Returns the list of bitfields listed in the given bitfield and an empty list
    if it is not a nested bitfield. *)

val find_bitfield_opt : string -> bitfield list -> bitfield option
(** [bitfield_find_opt name bfs] is [Some (bf)] if there exists [bf] in [bfs]
    with [name], [None] otherwise. *)

val find_bitfields_slices_opt : string -> bitfield list -> slice list option
(** [bitfields_find_slices_opt name bfs] is [Some (slices)] if there exists a
    bitfield with name [name] and slices [slices]. *)

module Infix : sig
  (** Infix utils. *)

  val ( ~$ ) : int -> literal
  (** [~$i] is an integer literal that contains [i]. *)

  val ( !$ ) : int -> expr
  (** An alias for [expr_of_int]. *)
end

(** {1 Equality helpers}

    Most of those take a [cmp_expr] argument that is the static analyser
    expression comparison. *)

val expr_equal : (expr -> expr -> bool) -> expr -> expr -> bool
val literal_equal : literal -> literal -> bool
val slice_equal : (expr -> expr -> bool) -> slice -> slice -> bool
val slices_equal : (expr -> expr -> bool) -> slice list -> slice list -> bool

val constraint_equal :
  (expr -> expr -> bool) -> int_constraint -> int_constraint -> bool

val constraints_equal :
  (expr -> expr -> bool) -> int_constraint list -> int_constraint list -> bool

val type_equal : (expr -> expr -> bool) -> ty -> ty -> bool

val array_length_equal :
  (expr -> expr -> bool) -> array_index -> array_index -> bool

val bitfield_equal : (expr -> expr -> bool) -> bitfield -> bitfield -> bool
val bitwidth_equal : (expr -> expr -> bool) -> expr -> expr -> bool
val qualifier_equal : func_qualifier option -> func_qualifier option -> bool

(** {1 Transformers} *)

val ldi_of_lexpr : lexpr -> local_decl_item option
val expr_of_lexpr : lexpr -> expr
val slice_is_single : slice -> bool
val slice_as_single : slice -> expr

val patch : src:AST.t -> patches:AST.t -> AST.t
(** [patch ~src ~patches] replaces in [src] the global identifiers defined by
    [patches]. *)

val subst_expr : (identifier * expr) list -> expr -> expr
(** [subst_expr substs e] replaces the variables used inside [e] by their
    associated expression in [substs], if any.

    Warning: constants and statically-evaluated parts are not changed, for
    example: [E_Slice (E_Var "y", [Slice_Single (E_Var "y")])] will become after
    [subst_expr [("y", E_Var "x")]]:
    [E_Slice (E_Var "x", [Slice_Single (E_Var "y")])] *)

val rename_locals : (identifier -> identifier) -> AST.t -> AST.t
(** [rename_locals f ast] is [ast] where all instances of variables [x] are
    replaced with [f x]. *)

val is_simple_expr : expr -> bool
(** [is_simple_expr e] is true if [e] does not contain any call to any other
    subprogram. It has false negative. *)

val identifier_of_decl : decl -> identifier
(** [identifier_of_decl d] is the name of the global element defined by [d]. *)

(** {1 Standard functions} *)

val pair : 'a -> 'b -> 'a * 'b
(** [pair a b] is [(a, b)]. *)

val pair' : 'b -> 'a -> 'a * 'b
(** [pair' b a] is [(b, a)]. *)

val list_equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
(** [list_equal elt_equal li1 li2] is true iff [li1] and [li2] are element-wise
    equal. *)

val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** An element wise comparaison for lists. *)

val list_cross : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [list_cross f [a1; ... an] [b1; ... bm]] is the list of all [f ai bj] in a
    non-specified order. *)

val list_take : int -> 'a list -> 'a list
(** [list_take n li] is the list of the first [n] elements of [li].

    If [li] has less than [n] elements, [list_take n li] is [li]. *)

val list_flat_cross : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list
(** [list_flat_cross f [a1; ... an] [b1; ... bm]] is the concatenation of all
    [f ai bj] in a non-specified order. *)

val list_concat_map : ('a -> 'b list) -> 'a list -> 'b list
(** [list_concat_map f l] gives the same result as [List.concat (List.map f l)].
    Tail-recursive. Taken from stdlib 4.10. *)

val list_fold_lefti : (int -> 'acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
(** Same as [List.fold_left] but takes a index. *)

val list_fold_left_map :
  ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_left_map] is a combination of [fold_left] and [map] that threads an
    accumulator through calls to [f]. Taken from stdlib 4.11. *)

val list_coalesce_right : ('a -> 'a -> 'a option) -> 'a list -> 'a list
(** [list_coalesce_right f l] applies the coalescing function [f] to adjacent
    elements of [l], using it to folding [l] in a right-to-left order.

    @param [f]
      is a function that, given two elements, either coalesces them into a
      single element or returns [None], signalling that the elements cannot be
      coalesced. *)

val uniq : 'a list -> 'a list
(** [uniq l] returns the unique elements of [l], in the order they appear *)

val get_first_duplicate : identifier list -> identifier option
(** [get_first_duplicate ids] returns [None] if all identifiers in [ids] are
    unique, otherwise it returns [Some id] where [id] is the first duplicate. *)

val list_is_empty : 'a list -> bool
(** [list_is_empty li] is [true] iff [li] is empty, [false] otherwise. *)

val list_split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(** Generalisation of [List.split] for 3-uples. *)

val list_map_split : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
(** Composition of [List.map] and [List.split]. *)

val list_iterated_op : empty:'a -> ('a -> 'a -> 'a) -> 'a list -> 'a
(** [list_iterated_op ~empty op li] computes the iterated binary operation [op]
    on the elements of [li], with the assumption that [op] is associative.

    If the list [li], this function returns [empty].

    This considers that applying [op] to 2 elements is longer than iterating on
    the list, in particular that the complexity of [op a b] depends on the size
    of [a] and [b], and the size of [op a b] increases with the size of [a] and
    the size of [b]. *)

val transitive_closure : ISet.t IMap.t -> ISet.t IMap.t
(** Returns the transitive closure of the graph. *)

val get_cycle : ISet.t IMap.t -> identifier list option
(** [get_cycle m] is [None] if the graph whose transition function is given by
    [m] is acyclic, [Some li] if [li] is a cycle in [m]. *)
