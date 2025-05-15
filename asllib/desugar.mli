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

open AST

(* -------------------------------------------------------------------------
    Elided parameters
   ------------------------------------------------------------------------- *)

val desugar_elided_parameter :
  local_decl_keyword -> local_decl_item -> ty -> call annotated -> stmt_desc
(**
  Desugar an elided parameter, in particular:
  {[
  let x : bits(e) = MyFunc{}(args)     --> ... = MyFunc{e}(args)
  let x : bits(e) = MyFunc{,e1}(args)  --> ... = MyFunc{e,e1}(args)
  ]}
  Similarly for [var] and [constant].
*)

(* -------------------------------------------------------------------------
    Left-hand sides
   ------------------------------------------------------------------------- *)

(* Types to represent valid left-hand sides produced by parsing. *)

type lhs_field = identifier annotated

type field_or_array_access =
  | FieldAccess of lhs_field
  | ArrayAccess of expr  (** An access of a single field or array index. *)

type lhs_access = {
  access : field_or_array_access list;  (** empty means no accesses *)
  slices : slice list annotated;  (** empty means no slices *)
}
(** An access is an optional series of nested field or array accesses,
    optionally followed by slices.
*)

val desugar_lhs_access : identifier annotated * lhs_access -> lexpr
(** Desugar an [lhs_access] on a base [identifier] to an [lexpr]. *)

val desugar_lhs_tuple :
  (identifier annotated * lhs_access) option list annotated -> lexpr
(** Desugar a list of optional pairs to an [LE_Destructuring].
    The [None] entries turn in to [LE_Discard].
    The [Some] entried contain a pair of base [identifier] and [lhs_access], and
    are desugared using [desguar_lhs_access]. *)

val desugar_lhs_fields_tuple :
  identifier annotated -> lhs_field option list -> lexpr_desc
(** [desugar_lhs_fields_tuple x flds] desugars a left-hand side of the form
    [x.(fld1, ..., fldk)] to [(x.fld1, ..., x.fldk)], ensuring that the [flds]
    are unique. *)

(* -------------------------------------------------------------------------
    Setters
   ------------------------------------------------------------------------- *)

val desugar_setter : call annotated -> lhs_access -> expr -> stmt_desc
(**
  Desugar a setter call, in particular:
  {[
  Setter(args) = rhs;                  -->  Setter(rhs, args);
  Setter(args).accesses[slices] = rhs; -->  var temp = Getter(args);
                                            temp.accesses[slices] = rhs;
                                            Setter(temp, args);
  ]}
*)

val desugar_setter_setfields :
  call annotated -> identifier list -> expr -> stmt_desc
(**
  Desugar a setter call that sets concatenated fields, in particular:
  {[
  Setter(args).[fld1,fld2] = rhs;   -->     var temp = Getter(args);
                                            temp.[fld1,fld2] = rhs;
                                            Setter(temp, args);
  ]}
*)

(* -------------------------------------------------------------------------
    Case statements
   ------------------------------------------------------------------------- *)

val desugar_case_stmt :
  expr_desc annotated -> case_alt_desc annotated list -> stmt -> stmt_desc
(** [desugar_case_stmt e0 cases otherwise] desugars a case statement for the
    discriminant expression [e0], case alternatives [cases], and otherwise
    statement [otherwise].
    The result is a conditional statement, possibly preceded by an assignment
    of the condition [e0] to a fresh variable). *)

(* -------------------------------------------------------------------------
    Accessor pairs
   ------------------------------------------------------------------------- *)

type accessor_pair = {
  getter : stmt;  (** getter body *)
  setter : stmt;  (** setter body *)
}
(** A getter/setter pair *)

val desugar_accessor_pair :
  override_info option ->
  identifier ->
  (identifier * ty option) list ->
  typed_identifier list ->
  identifier ->
  ty ->
  accessor_pair ->
  decl list
(** [desugar_accessor_pair override name params args setter_arg ty accessor_pair]
    desugars the accessor pair into two function declarations, with shared
    [override], [name], [params], [args], and input/return type [ty].
    The name of the setter argument is given by [setter_arg]. *)
