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

(** Pretty-printers for ASL ASTs. *)

open AST

(** {1 Utils} *)

type 'a printer = Format.formatter -> 'a -> unit
(** A general pretty-printer type. *)

(* Available from 4.12.0 *)
val pp_print_seq : ?pp_sep:unit printer -> 'a printer -> 'a Seq.t printer
(** Re-exported from stdlib 4.12, print q sequence from its elements. *)

val pp_pos : 'a annotated printer

val pp_pos_str : 'a annotated -> string
(** Print a position. *)

(** {1 AST pretty-printers} *)

val pp_literal : literal printer
(** Print a literal from its components.*)

val pp_expr : expr printer
(** Pretty-print an expression. *)

val pp_ty : ty printer
(** Pretty-print a type. *)

val pp_typed_identifier : typed_identifier printer
(** Pretty-print a variable and its type. *)

val pp_bitfields : bitfield list printer
(** Pretty-print a list of bitfields. *)

val pp_lexpr : lexpr printer
(** Pretty-print the left-hand side of an assignment. *)

val pp_for_direction : for_direction -> string
(** Pretty-print keyword of for loop direction *)

val pp_stmt : stmt printer
(** Pretty-print a statement. *)

val pp_slice : slice printer
(** Pretty-print a slice. *)

val pp_slice_list : slice list printer
(** Pretty-print a list of slices. *)

val pp_int_constraint : int_constraint printer
(** Pretty-print an int constraint. *)

val pp_int_constraints : int_constraint list printer
(** Pretty-print a list of int constraints. *)

val pp_local_decl_item : local_decl_item printer
(** Pretty-print a local declaration item. *)

val pp_pattern : pattern printer
(** Pretty-print a pattern. *)

val pp_t : t printer
(** Print an AST from printer for a literal *)

val pp_version : [ `ASLv0 | `ASLv1 | `Any ] printer
(** Print the ASL version. *)

val pp_scope : scope printer
(** Print a scope. *)

(** {1 Pretty-print to strings} *)

val literal_to_string : literal -> string
(** Converts a literal into a string. *)

val binop_to_string : binop -> string
(** Writes a binop as an ASL operator. *)

val unop_to_string : unop -> string
(** Writes a unop as an ASL operator. *)

val ty_to_string : ty -> string
(** Converts a type into a string. *)

val t_to_string : t -> string
(** [t_to_string v_to_string ast] is a string representing [ast] with literals
    printed with [v_to_string].*)
