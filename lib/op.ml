(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(*********************)

type op =
  | Add | Sub | Mul | Div
  | And | Or | Xor | Nor
  | ShiftLeft
  | Lt | Gt | Eq | Ne
  | Le | Ge

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
  | ShiftLeft -> "<<<" (* In Java ?? *)
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Ne -> "!="

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

let pp_op1 o = match o with
| Not -> "!"
| SetBit i -> sprintf "setbit%i" i
| UnSetBit i -> sprintf "unsetbit%i" i
| ReadBit i -> sprintf "readbit%i" i

(***********)

type op3 = If

let pp_op3 o s1 s2 s3 = match o with
| If -> sprintf "%s ? %s : %s" s1 s2 s3
