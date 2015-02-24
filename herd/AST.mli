(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Syntax tree of model definitions *)

type pos = { pos:int; len:int;}

type set_or_rln = SET | RLN

type direction = 
  | Write | Read | WriteRead | Atomic | Plain
  | Unv_Set (** universal set *)
  | Bar_Set (** set of barriers *)
type op2 = 
  | Union (** applies to sets or relations *)
  | Inter (** applies to sets or relations *) 
  | Diff  (** applies to sets or relations *)
  | Seq   (** sequential composition of relations *) 
  | Cartesian (** build a relation from two sets *)
  | Add   (** add element to a set *)
type op1 =
  | Plus | Star | Opt 
  | Comp (** Set or relation complement *)
  | Inv  (** Relation inverse *)

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
  | App of  TxtLoc.t * exp * exp list
  | Bind  of  TxtLoc.t * binding list * exp
  | BindRec  of  TxtLoc.t * binding list * exp
  | Fun of  TxtLoc.t * var list * exp *
        var (* name *) * varset (* free vars *)
  | ExplicitSet of TxtLoc.t * exp list
  | Match of TxtLoc.t * exp * clause list * exp option
  | MatchSet of TxtLoc.t * exp * exp * (string * string * exp)
  | Try of TxtLoc.t * exp * exp

and clause = string * exp

and binding = var * exp

type test = Acyclic | Irreflexive | TestEmpty

type test_type = Requires | Provides

type ins =
  | Let of TxtLoc.t * binding list
  | Rec of  TxtLoc.t * binding list
  | Test of  TxtLoc.t * pos * test * exp * string option * test_type
  | UnShow of  TxtLoc.t * string list
  | Show of  TxtLoc.t * string list
  | ShowAs of  TxtLoc.t * exp * string
  | Latex of  TxtLoc.t * string
  | Include of  TxtLoc.t * string (* file name, interpreter will read/parse file... *)
  | Procedure of  TxtLoc.t * var * var list * ins list
  | ProcedureTest of TxtLoc.t * var * exp list * string option
  | Call of  TxtLoc.t * var * exp list
  | Enum of TxtLoc.t * var * tag list
  | Forall of  TxtLoc.t * var * exp * ins list
  | Debug of TxtLoc.t * exp
  | WithFrom of TxtLoc.t * var * exp (* set of relations *)

(*For bell cat files*)
  | EnumSet of TxtLoc.t * var * tag list
  | EnumRel of TxtLoc.t * var * tag list

(*For bell files*)
  | Event_dec of TxtLoc.t * var * exp list
  | Relation_dec of TxtLoc.t * var * exp
  | Order_dec of TxtLoc.t * var * (exp * exp) list


 
(** Name X model definition *)
type t = ModelOption.t * string * ins list
type pp_t = string * t
