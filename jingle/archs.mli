type t = [ 
       (*| `X86
       | `PPC
       | `MIPS
       | `GPU_PTX
      *)    
       | `Bell
       | `ARM
       | `AArch64
       | `C 
     ]

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string

(*val ppc : t
val x86 : t
val mips : t
val gpu_ptx : t
val opencl : t
 *)
val c : t
val lisa : t
val arm : t
val aarch64 : t
