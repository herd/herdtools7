module System : sig
  (* Current architecture *)
  type t =
    [ 
 (*   | `X86
    | `PPC
    | `ARM
    | `MIPS
    | `GPU_PTX
      *)    | `Bell
    | `AArch64
    ]

  val tags : string list
  val parse : string -> t option
  val lex : string -> t
  val pp : t -> string
end

type t = System.t (*[ | `C | `OpenCL ]*)

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string

(*val arm : t
val ppc : t
val x86 : t
val mips : t
val gpu_ptx : t
val c : t
val opencl : t
 *)val bell : t
val aarch64 : t
