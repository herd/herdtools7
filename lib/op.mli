(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Syntax of unary, binary and ternary operations *)

(**********)
(* Binary *)
(**********)

type op =
  | Add | Sub | Mul | Div
  | And | Or | Xor | Nor

(* Logical shift left *)
  | ShiftLeft
(* Return C-style boolean (zero is false, not zero is true) *)
  | Lt | Gt | Eq | Ne
  | Le | Ge

val pp_op : op -> string

val pp_ptx_cmp_op : op -> string

(*********)
(* Unary *)
(*********)

type op1 =
  | Not
(* Low order bit index is zero *)
  | SetBit of int | UnSetBit of int
  | ReadBit of int

val pp_op1 : op1 -> string

(***********)
(* Ternary *)
(***********)

type op3 = If

val pp_op3 : op3 -> string -> string -> string ->  string
