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

open Printf

(*********************)

type op =
  | Add | Sub | Mul | Div
  | And | Or | Xor | Nor | AndNot2
  | ShiftLeft
  | Lt | Gt | Eq | Ne
  | Le | Ge
  | Max | Min
  | SetTag

let pp_op o =
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^" (* in C ?? *)
  | Nor -> "(nor)"
  | AndNot2 -> "(andnot2)"
  | ShiftLeft -> "<<<" (* In Java ?? *)
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Ne -> "!="
  | Max -> "max"
  | Min -> "min"
  | SetTag -> "settag"

let pp_ptx_cmp_op = function
  | Eq -> ".eq"
  | Lt -> ".lt"
  | Gt -> ".gt"
  | Ne -> ".ne"
  | Le -> ".le"
  | Ge -> ".ge"
  | _ -> Warn.user_error "Invalid PTX comparison operator"

(********************)

type op1 =
  | Not
  | SetBit of int | UnSetBit of int
  | ReadBit of int
  | LeftShift of int
  | LogicalRightShift of int
  | AddK of int
  | AndK of string
  | Mask of MachSize.sz
  | TagLoc       (* Get tag memory location from location *)
  | TagExtract   (* Extract tag from tagged location *)
  | LocExtract   (* Extract actual location from location *)

let pp_op1 hexa o = match o with
| Not -> "!"
| SetBit i -> sprintf "setbit%i" i
| UnSetBit i -> sprintf "unsetbit%i" i
| ReadBit i -> sprintf "readbit%i" i
| LeftShift i -> sprintf "<<[%i]" i
| LogicalRightShift i -> sprintf ">>>[%i]" i
| AddK i  -> (if hexa then sprintf "+[0x%x]" else sprintf "+[%i]") i
| AndK i  -> sprintf "&[%s]" i
| Mask sz  -> sprintf "mask%02i" (MachSize.nbits sz)
| TagLoc ->  "tagloc"
| TagExtract -> "tagextract"
| LocExtract -> "locextract"
(***********)

type op3 = If

let pp_op3 o s1 s2 s3 = match o with
| If -> sprintf "%s ? %s : %s" s1 s2 s3
