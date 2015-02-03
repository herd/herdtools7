(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*********)
(* Archs *)
(*********)

module System = struct
  type t =
    [ `X86
    | `PPC
    | `ARM
    | `MIPS
    | `GPU_PTX
    ]

  let tags = ["X86";"PPC";"ARM";"MIPS";"GPU_PTX"]

  let parse s = match s with
  | "X86" -> Some `X86
  | "PPC" -> Some `PPC
  | "ARM" -> Some `ARM
  | "MIPS" -> Some `MIPS
  | "GPU_PTX" -> Some `GPU_PTX
  | _ -> None

  let lex s = match parse s with
  | Some a -> a
  | None -> assert false


  let pp a = match a with
  | `X86 -> "X86"
  | `PPC -> "PPC"
  | `ARM -> "ARM"
  | `MIPS -> "MIPS"
  | `GPU_PTX -> "GPU_PTX"
end

type t = [ System.t | `C | `OpenCL ]

let tags = ["X86";"PPC";"ARM";"MIPS";"GPU_PTX";"C";"OpenCL"]

let parse s = match s with
| "X86" -> Some `X86
| "PPC" -> Some `PPC
| "ARM" -> Some `ARM
| "MIPS" -> Some `MIPS
| "GPU_PTX" -> Some `GPU_PTX
| "C" -> Some `C
| "OpenCL" -> Some `OpenCL
| _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
| `X86 -> "X86"
| `PPC -> "PPC"
| `ARM -> "ARM"
| `MIPS -> "MIPS"
| `GPU_PTX -> "GPU_PTX"
| `C -> "C"
| `OpenCL -> "OpenCL"

let arm = `ARM
let ppc = `PPC
let x86 = `X86
let mips = `MIPS
let gpu_ptx = `GPU_PTX
let c = `C
let opencl = `OpenCL
