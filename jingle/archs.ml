
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
type t = [ 
    (*| `X86
    | `MIPS
    | `GPU_PTX
    | OpenCL
     *)    
    | `Bell
    | `PPC
    | `ARM
    | `AArch64
    | `C 
   ]

let tags = ["X86";"PPC";"ARM";"AArch64";"MIPS";"GPU_PTX";"C";"OpenCL"; "Bell"]

let parse s = match s with
(*| "X86" -> Some `X86
| "MIPS" -> Some `MIPS
| "GPU_PTX" -> Some `GPU_PTX
| "OpenCL" -> Some `OpenCL
 *)
  | "C" -> Some `C
  | "PPC" -> Some `PPC
  | "Bell"|"BELL"|"LISA"-> Some `Bell
  | "ARM" -> Some `ARM
  | "AArch64" -> Some `AArch64
  | _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
(*| `X86 -> "X86"
| `MIPS -> "MIPS"
| `GPU_PTX -> "GPU_PTX"
| `OpenCL -> "OpenCL"
 *)
  | `PPC -> "PPC"
  | `C -> "C"
  | `ARM -> "ARM"
  | `AArch64 -> "AArch64"
  | `Bell -> "LISA"

(*let x86 = `X86
let mips = `MIPS
let gpu_ptx = `GPU_PTX
let opencl = `OpenCL
*)
let ppc = `PPC
let c = `C
let lisa = `Bell
let arm = `ARM
let aarch64 = `AArch64

