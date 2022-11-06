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

(** Operations on base value of arity one. *)
type unop =
  | BNOT  (** Boolean inversion *)
  | NEG  (** Integer or real negation *)
  | NOT  (** Bitvector bitwise inversion *)

(** Operations on base value of arity two. *)
type binop =
  | AND  (** Bitvector bitwise and *)
  | BAND  (** Boolean and *)
  | BEQ  (** Boolean equivalence *)
  | BOR  (** Boolean or *)
  | DIV  (** Integer division *)
  | EOR  (** Bitvector bitwise exclusive or *)
  | EQ_OP  (** Equality on two base values of same type *)
  | GT  (** Greater than for int or reals *)
  | GEQ  (** Greater or equal for int or reals *)
  | IMPL  (** Boolean implication *)
  | LT  (** Less than for int or reals *)
  | LEQ  (** Less or equal for int or reals *)
  | MOD  (** Remainder of integer division *)
  | MINUS  (** Substraction for int or reals or bitvectors *)
  | MUL  (** Multiplication for int or reals or bitvectors *)
  | NEQ  (** Non equality on two base values of same type *)
  | OR  (** Bitvector bitwise or *)
  | PLUS  (** Addition for int or reals or bitvectors *)
  | RDIV  (** Division for reals *)
  | SHL  (** Shift left for ints *)
  | SHR  (** Shift right for ints *)

type identifier = string
(** Type of local identifiers in the AST. *)

module ISet : Set.S with type elt = string
module IMap : Map.S with type key = string

(** Main value type, parametric on its base values *)
type ('i, 'b, 'r, 'bv) value =
  | VInt of 'i
  | VBool of 'b
  | VReal of 'r
  | VBitVector of 'bv

type parsed_value = (int, bool, float, string) value
(** Type of parsed values by the module Parser.mly *)

val value_of_vint : 'i -> ('i, 'b, 'r, 'bv) value
(** Value constructor for VInt. *)

val value_of_vbool : 'b -> ('i, 'b, 'r, 'bv) value
(** Value constructor for VBool. *)

val value_of_vreal : 'r -> ('i, 'b, 'r, 'bv) value
(** Value constructor for VReal. *)

val value_of_vbitvector : 'bv -> ('i, 'b, 'r, 'bv) value
(** Value constructor for VBitVector. *)

(** Expressions. Parametric on the type of literals. *)
type 'v expr =
  | ELiteral of 'v
  | EVar of identifier
  | EBinop of binop * 'v expr * 'v expr
  | EUnop of unop * 'v expr
  | ECall of identifier * 'v expr list
  | EGet of identifier * 'v expr list
  | ECond of 'v expr * 'v expr * 'v expr

(** Type of left-hand side of assignments. *)
type 'v lexpr = LEVar of identifier | LESet of identifier * 'v expr list

(** Statements. Parametric on the type of literals in expressions. *)
type 'v stmt =
  | SPass
  | SThen of 'v stmt * 'v stmt
  | SAssign of 'v lexpr * 'v expr
  | SCall of identifier * 'v expr list
  | SReturn of 'v expr list
  | SCond of 'v expr * 'v stmt * 'v stmt

type 'v func = identifier * identifier list * 'v stmt
(** Declared functions. Parametric on the type of literals in the body. *)

(** Declarations, ie. top level statement in a asl file. *)
type 'v decl =
  | Func of 'v func
  | Enum of string list
  | GlobalConst of identifier * 'v expr

type 'v t = 'v decl list
(** Main AST type. *)

type parsed_t = parsed_value t
(** Type of parsed ast by the module Parser.mly *)

(** Machine readable stable and fast serializing functions *)
module Serialize : sig
  val pp_value :
    ('i -> string) ->
    ('b -> string) ->
    ('r -> string) ->
    ('bv -> string) ->
    Buffer.t ->
    ('i, 'b, 'r, 'bv) value ->
    unit
  (** Print a value from its components.*)

  val pp_parsed_value : Buffer.t -> parsed_value -> unit
  (** Print a parsed value *)

  val pp_t : (Buffer.t -> 'v -> unit) -> Buffer.t -> 'v t -> unit
  (** Serialize an AST from printer for a value *)

  val pp_parsed_t : Buffer.t -> parsed_t -> unit
  (** Serialize a parsed AST.*)

  val t_to_string : ('v -> string) -> 'v t -> string
  (** [t_to_string v_to_string ast] is a string representing [ast] with values printed with [v_to_string].*)

  val parsed_t_to_string : parsed_t -> string
  (* Serialize a parsed AST.*)
end

(** Pretty-printing functions for AST with format.*)
module PP : sig
  val pp_value :
    (Format.formatter -> 'i -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'r -> unit) ->
    (Format.formatter -> 'bv -> unit) ->
    Format.formatter ->
    ('i, 'b, 'r, 'bv) value ->
    unit
  (** Print a value from its components.*)

  val pp_parsed_value : Format.formatter -> parsed_value -> unit
  (** Print a parsed value *)

  val pp_t :
    (Format.formatter -> 'v -> unit) -> Format.formatter -> 'v t -> unit
  (** Print an AST from printer for a value *)

  val pp_parsed_t : Format.formatter -> parsed_t -> unit
  (** Format a parsed AST.*)

  val t_to_string : ('v -> string) -> 'v t -> string
  (** [t_to_string v_to_string ast] is a string representing [ast] with values printed with [v_to_string].*)

  val parsed_t_to_string : parsed_t -> string
  (* Print a parsed AST.*)
end

val stmt_from_list : 'v stmt list -> 'v stmt
(** Builder for a serie of SThen. *)

val use_expr : bool -> 'v expr -> ISet.t
(** use_expr false e is the set of variables used in e. If the first argument is set, function names are added, non-recursively. *)

val tr_values :
  ('i1 -> ('i2, 'b2, 'r2, 'bv2) value) ->
  ('b1 -> ('i2, 'b2, 'r2, 'bv2) value) ->
  ('r1 -> ('i2, 'b2, 'r2, 'bv2) value) ->
  ('bv1 -> ('i2, 'b2, 'r2, 'bv2) value) ->
  ('i1, 'b1, 'r1, 'bv1) value t ->
  ('i2, 'b2, 'r2, 'bv2) value t
(** A translation function that changes the type of values in an AST *)
