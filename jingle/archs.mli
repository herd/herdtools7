type t = [ 
       (*| `X86
       | `PPC
       | `ARM
       | `MIPS
       | `GPU_PTX
      *)    
       | `Bell
       | `AArch64
       | `C 
     ]

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string

(*val arm : t
val ppc : t
val x86 : t
val mips : t
val gpu_ptx : t
val opencl : t
 *)
val c : t
val lisa : t
val aarch64 : t
