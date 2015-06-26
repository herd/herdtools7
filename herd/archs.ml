(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*********)
(* Archs *)
(*********)

type t =
    [`X86
    | `PPC
    | `ARM
    | `AArch64
    | `MIPS
    | `LISA
    | `C ]

(*jade: des tonnes a virer!*)
let tags = ["X86";"PPC";"ARM";"AArch64";"MIPS";"C"; "LISA"]

let parse s = match s with
| "X86" -> Some `X86
| "PPC" -> Some `PPC
| "ARM" -> Some `ARM
| "AArch64" -> Some `AArch64
| "MIPS" -> Some `MIPS
| "C" -> Some `C
| "Bell" ->
    Warn.warn_always "Bell is deprecated as an arch name, use LISA" ;
    Some `LISA
| "LISA" -> Some `LISA
| _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
| `X86 -> "X86"
| `PPC -> "PPC"
| `ARM -> "ARM"
| `AArch64 -> "AArch64"
| `MIPS -> "MIPS"
| `LISA -> "LISA"
| `C -> "C"

let arm = `ARM
let aarch64 = `AArch64
let ppc = `PPC
let x86 = `X86
let mips = `MIPS
let c = `C
let lisa = `LISA
