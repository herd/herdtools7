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

  type arch = [
    | `AArch64
    | `ARM
    | `MIPS
    | `PPC
    | `X86
    | `RISCV
    | `X86_64
    ]

  type t = [ arch | `Unknown ]

  let tags = [
    "AArch64";
    "ARM";
    "MIPS";
    "PPC";
    "X86";
    "RISCV";
    "X86_64";
  ]

  let parse = function
    | "AArch64" -> Some `AArch64
    | "ARM" -> Some `ARM
    | "MIPS" -> Some `MIPS
    | "PPC" -> Some `PPC
    | "X86" -> Some `X86
    | "RISCV"|"RISC-V" -> Some `RISCV
    | "X86_64" -> Some `X86_64
    | _ -> None

  let pp (a:t) = match a with
    | `AArch64 -> "AArch64"
    | `ARM -> "ARM"
    | `MIPS -> "MIPS"
    | `PPC -> "PPC"
    | `X86 -> "X86"
    | `RISCV -> "RISCV"
    | `X86_64 -> "X86_64"
    | `Unknown -> "Unknown"
end

type t = [
  System.arch
  | `C
  | `CPP
  | `LISA
  | `JAVA
]

let tags =
  "C"
  ::"CPP"
  ::"LISA"
  :: "JAVA"
  ::System.tags

let parse s = match System.parse s with
  | None -> begin
    match s with
      | "C"   -> Some `C
      | "CPP" | "C++"   -> Some `CPP
      | "Bell" ->
        Warn.warn_always "Bell is deprecated, use LISA instead";
        Some `LISA
      | "LISA" -> Some `LISA
      | "JAVA" | "Java" -> Some `JAVA
      | _ -> None
  end
  | a -> a

let pp = function
  | `C -> "C"
  | `CPP -> "C++"
  | `LISA -> "LISA"
  | `JAVA -> "Java"
  | #System.arch as a -> System.pp a

let aarch64 = `AArch64
let arm = `ARM
let mips = `MIPS
let ppc = `PPC
let x86 = `X86
let riscv = `RISCV
let c = `C
let cpp = `CPP
let lisa = `LISA
let x86_64 = `X86_64
let java = `JAVA

let compare = compare

let get_sysarch a ca = match a with
  | #System.arch as a -> a
  |`CPP|`LISA | `JAVA -> `Unknown
  | `C -> ca

let check_carch a = match a with
  | `Unknown ->
      Warn.user_error
        "Please specify your architecture with the -carch <arch> option"
  | #System.arch as a -> a
