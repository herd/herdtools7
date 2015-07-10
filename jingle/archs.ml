type t = [ 
    (*| `X86
    | `PPC
    | `MIPS
    | `GPU_PTX
    | OpenCL
     *)    
    | `Bell
    | `ARM
    | `AArch64
    | `C 
   ]

let tags = ["X86";"PPC";"ARM";"AArch64";"MIPS";"GPU_PTX";"C";"OpenCL"; "Bell"]

let parse s = match s with
(*| "X86" -> Some `X86
| "PPC" -> Some `PPC
| "MIPS" -> Some `MIPS
| "GPU_PTX" -> Some `GPU_PTX
| "OpenCL" -> Some `OpenCL
 *)
  | "C" -> Some `C
  | "Bell"|"BELL"|"LISA"-> Some `Bell
  | "ARM" -> Some `ARM
  | "AArch64" -> Some `AArch64
  | _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
(*| `X86 -> "X86"
| `PPC -> "PPC"
| `MIPS -> "MIPS"
| `GPU_PTX -> "GPU_PTX"
| `OpenCL -> "OpenCL"
 *)
  | `C -> "C"
  | `ARM -> "ARM"
  | `AArch64 -> "AArch64"
  | `Bell -> "LISA"

(*let ppc = `PPC
let x86 = `X86
let mips = `MIPS
let gpu_ptx = `GPU_PTX
let opencl = `OpenCL
 *)
let c = `C
let lisa = `Bell
let arm = `ARM
let aarch64 = `AArch64
