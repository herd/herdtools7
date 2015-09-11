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

(*********)
(* Archs *)
(*********)

module System = struct
  type t =
    [ `X86
    | `PPC
    | `ARM
    | `MIPS
    | `AArch64
    ]

  let tags = ["X86";"PPC";"ARM";"PPCGen";"MIPS";"AArch64"]

  let parse s = match s with
  | "X86" -> Some `X86
  | "PPC" -> Some `PPC
  | "ARM" -> Some `ARM
  | "MIPS" -> Some `MIPS
  | "AArch64" -> Some `AArch64
  | _ -> None

  let lex s = match parse s with
  | Some a -> a
  | None -> assert false


  let pp a = match a with
  | `X86 -> "X86"
  | `PPC -> "PPC"
  | `ARM -> "ARM"
  | `MIPS -> "MIPS"
  | `AArch64 -> "AArch64"
end

type t = [ System.t | `C ]

let tags = "C"::System.tags

let parse s = match System.parse s with
| Some _ as r -> r
| None -> match s with
  | "C" -> Some `C
  | _ -> None


let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp = function
| `C -> "C"
| #System.t as a -> System.pp a


let arm = `ARM
let ppc = `PPC
let x86 = `X86
let mips = `MIPS
let c = `C
let aarch64 = `AArch64

