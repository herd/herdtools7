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
  | X86
  | PPC
  | ARM
  | MIPS
  | AArch64
  | C
let tags = ["X86";"PPC";"ARM";"MIPS";"AArch64";"C"]

let parse s = match s with
| "X86" -> Some X86
| "PPC" -> Some PPC
| "ARM" -> Some ARM
| "MIPS" -> Some MIPS
| "AArch64" -> Some AArch64
| "C" -> Some C
| _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
| X86 -> "X86"
| PPC -> "PPC"
| ARM -> "ARM"
| MIPS -> "MIPS"
| AArch64 -> "AArch64"
| C -> "C"

let arm = ARM
let ppc = PPC
let x86 = X86
let mips = MIPS
let aarch64 = AArch64

