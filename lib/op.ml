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
  | ASR
  | CapaAdd | Alignd | Alignu | Build | ClrPerm | CpyType | CSeal | Cthi | Seal
  | SetValue | CapaSub | CapaSubs | CapaSetTag | Unseal
  | ShiftLeft
  | ShiftRight
  | Lsr
  | Lt | Gt | Eq | Ne
  | Le | Ge
  | Max | Min
  | SetTag
  | SquashMutable
  | CheckPerms of string

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
  | CapaAdd -> "(capaadd)"
  | Alignd -> "(alignd)"
  | Alignu -> "(alignu)"
  | Build -> "(build)"
  | ClrPerm -> "(clrperm)"
  | CpyType -> "(cpytype)"
  | CSeal -> "(cseal)"
  | Cthi -> "(cthi)"
  | Seal -> "(seal)"
  | SetValue -> "(setvalue)"
  | CapaSub -> "(capasub)"
  | CapaSubs -> "(capasubs)"
  | Unseal -> "(unseal)"
  | ShiftLeft -> "<<<" (* In Java ?? *)
  | ShiftRight -> ">>>"
  | Lsr -> ">>>"
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Ne -> "!="
  | Max -> "max"
  | Min -> "min"
  | ASR -> "ASR"
  | SetTag -> "settag"
  | CapaSetTag -> "capasettag"
  | SquashMutable -> "squashmutable"
  | CheckPerms perms -> sprintf "(checkcapa:%s)" perms

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
  | Sxt of MachSize.sz
  | Inv
  | TagLoc       (* Get tag memory location from location *)
  | CapaTagLoc
  | TagExtract   (* Extract tag from tagged location *)
  | LocExtract   (* Extract actual location from location *)
  | UnSetXBits of int * int (* Unset x bits to the left from y*)
  | CapaGetTag
  | CheckSealed
  | CapaStrip
  | TLBLoc (* get TLB entry from location *)
  | PTELoc (* get PTE entry from location *)
  | AF (* get AF from PTE entry *)
  | SetAF (* set AF to 1 in PTE entry *)
  | DB (* get DB from PTE entry *)
  | SetDB (* set DB to 1 in PTE entry *)
  | DBM (* get DBM from PTE entry *)
  | Valid (* get Valid bit from PTE entry *)
  | EL0 (* get EL0 bit from PTE entry *)
  | OA (* get OA from PTE entry *)
  | IsVirtual (* Detect virtual addresses *)


let pp_op1 hexa o = match o with
| Not -> "!"
| SetBit i -> sprintf "setbit%i" i
| UnSetBit i -> sprintf "unsetbit%i" i
| ReadBit i -> sprintf "readbit%i" i
| LeftShift i -> sprintf "<<[%i]" i
| LogicalRightShift i -> sprintf ">>>[%i]" i
| AddK i  -> (if hexa then sprintf "+[0x%x]" else sprintf "+[%i]") i
| AndK i  -> sprintf "&[%s]" i
| Inv -> "~"
| Mask sz  -> sprintf "mask%02i" (MachSize.nbits sz)
| Sxt sz -> sprintf "sxt%02i" (MachSize.nbits sz)
| TagLoc ->  "tagloc"
| CapaTagLoc -> "capatagloc"
| TagExtract -> "tagextract"
| LocExtract -> "locextract"
| UnSetXBits (nbBits, from) -> sprintf "unset %i bits to the left from %ith bit" nbBits from
| CapaGetTag -> "capagettag"
| CheckSealed -> "checksealed"
| CapaStrip -> "capastrip"
| TLBLoc -> "TLBloc"
| PTELoc -> "PTEloc"
| IsVirtual -> "IsVirtual"
| AF -> "AF"
| SetAF -> "SetAF"
| DB -> "DB"
| SetDB -> "SetDB"
| DBM -> "DBM"
| Valid -> "Valid"
| EL0 -> "EL0"
| OA -> "OA"

(***********)

type op3 = If

let pp_op3 o s1 s2 s3 = match o with
| If -> sprintf "%s ? %s : %s" s1 s2 s3

(***********************************)
(* Specific "Illegal Op" exception *)
(***********************************)

type any_op = Op1 of op1 | Op of op | Op3 of op3

exception Illegal of any_op * string

let illegal op fmt =
  ksprintf (fun msg -> raise (Illegal (op,msg)))  fmt

