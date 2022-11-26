(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Syntax of unary, binary and ternary operations *)

(**********)
(* Binary *)
(**********)

type op =
  | Add | Sub | Mul | Div
  | And | Or | Xor | Nor
  | AndNot2
(* Arithmetic shift right *)
  | ASR
  | CapaAdd | Alignd | Alignu | Build | ClrPerm | CpyType | CSeal | Cthi | Seal | SetValue | CapaSub | CapaSubs | CapaSetTag | Unseal
(* Logical shift left *)
  | ShiftLeft
  | ShiftRight
  | Lsr
(* Return C-style boolean (zero is false, not zero is true) *)
  | Lt | Gt | Eq | Ne
  | Le | Ge
(* on integers *)
  | Max | Min
(* Build tagged location from location and tag *)
  | SetTag
  | SquashMutable
  | CheckPerms of string

val pp_op : op -> string

val is_infix : op -> bool

val pp_ptx_cmp_op : op -> string

(*********)
(* Unary *)
(*********)

type 'aop op1 =
  | Not
(* Low order bit index is zero *)
  | SetBit of int | UnSetBit of int
  | ReadBit of int
  | LeftShift of int
  | LogicalRightShift of int
  | AddK of int
  | AndK of string
  | Mask of MachSize.sz
  | Sxt of MachSize.sz (* Sign extension *)
  | Inv          (* Logical not or inverse *)
  | TagLoc       (* Get tag memory location from location *)
  | CapaTagLoc
  | TagExtract   (* Extract tag from tagged location *)
  | LocExtract   (* Extract actual location from location *)
  | UnSetXBits of int * int
  | CapaGetTag
  | CheckSealed
  | CapaStrip
  | IsVirtual (* Predicate for virtual adresses *)
  | TLBLoc (* get TLB entry from location *)
  | PTELoc (* get PTE entry from location *)
  | Offset (* get offset from base (symbolic) location *)
  | IsInstr (* Check nature of constant *)
  | ArchOp1 of 'aop

val pp_op1 : bool -> (bool -> 'aop -> string) -> 'aop op1 -> string

(***********)
(* Ternary *)
(***********)

type op3 = If

val pp_op3 : op3 -> string -> string -> string ->  string
