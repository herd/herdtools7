(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Syntax tree of model definitions *)

type loc =  { pos:int; len:int;}
type pos =  Pos of loc | Txt of string

type set_or_rln = SET | RLN

type op2 =
  | Union (** applies to sets or relations *)
  | Inter (** applies to sets or relations *)
  | Diff  (** applies to sets or relations *)
  | Seq   (** sequential composition of relations *)
  | Cartesian (** build a relation from two sets *)
  | Add   (** add element to a set *)
  | Tuple (** Build a tuple *)

type op1 =
  | Plus | Star | Opt
  | Comp (** Set or relation complement *)
  | Inv  (** Relation inverse *)
  | ToId (** Lift set to id relation (ie toido(S) = (S * S) & id *)

type konst = Empty of set_or_rln | Universe of set_or_rln
type var = string
type tag = string
type varset = StringSet.t

type scope = Device | Kernel | Work_Group | Sub_Group | Work_Item

type exp =
  | Konst of  TxtLoc.t * konst
  | Tag of TxtLoc.t * tag
  | Var of TxtLoc.t * var
  | Op1 of  TxtLoc.t * op1 * exp
  | Op of  TxtLoc.t * op2 * exp list
  | App of  TxtLoc.t * exp * exp
  | Bind  of  TxtLoc.t * binding list * exp
  | BindRec  of  TxtLoc.t * binding list * exp
  | Fun of  TxtLoc.t * pat * exp *
        var (* name *) * varset (* free vars *)
  | ExplicitSet of TxtLoc.t * exp list
  | Match of TxtLoc.t * exp * clause list * exp option
  | MatchSet of TxtLoc.t * exp * exp * set_clause
  | Try of TxtLoc.t * exp * exp
  | If of TxtLoc.t * cond * exp * exp

and set_clause =
  | EltRem of pat0 * pat0 * exp
  | PreEltPost of pat0 * pat0 * pat0 * exp


and pat = Pvar of pat0 | Ptuple of pat0 list

and pat0 = var option

and variant_cond =
  | Variant of string
  | OpNot of variant_cond
  | OpAnd of variant_cond * variant_cond
  | OpOr of variant_cond * variant_cond

and cond =
| Eq of exp * exp | Subset of exp * exp | In of exp * exp
| VariantCond of variant_cond

and clause = string * exp

and binding = TxtLoc.t * pat * exp

type do_test = Acyclic | Irreflexive | TestEmpty
type test = Yes of do_test | No of do_test
type test_type = Flagged | UndefinedUnless | Check | Assert
type app_test = TxtLoc.t * pos * test * exp * string option
type is_rec = IsRec | IsNotRec

type ins =
  | Let of TxtLoc.t * binding list
  | Rec of  TxtLoc.t * binding list * app_test option
  | InsMatch of TxtLoc.t * exp * insclause list * ins list option
  | Test of  app_test * test_type
  | UnShow of  TxtLoc.t * string list
  | Show of  TxtLoc.t * string list
  | ShowAs of  TxtLoc.t * exp * string
  | Include of  TxtLoc.t * string (* file name, interpreter will read/parse file... *)
  | Procedure of  TxtLoc.t * var * pat * ins list * is_rec
  | Call of  TxtLoc.t * var * exp * string option (* optional name, for skip *)
  | Enum of TxtLoc.t * var * tag list
  | Forall of  TxtLoc.t * var * exp * ins list
  | Debug of TxtLoc.t * exp
  | WithFrom of TxtLoc.t * var * exp (* set of relations *)
(*For bell files*)
  | Events of TxtLoc.t * var * exp list * bool (* define default *)
(*Conditional, on variant as a string, avoiding dependency on herd/variant.ml *)
  | IfVariant of TxtLoc.t * variant_cond * ins list * ins list

and insclause = string * ins list



(** Name X model definition *)
type t = ModelOption.t * string * ins list
