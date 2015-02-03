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

module System : sig
  (* Current architecture *)
  type t =
    [ `X86
    | `PPC
    | `ARM
    | `MIPS
    | `GPU_PTX
    ]

  val tags : string list
  val parse : string -> t option
  val lex : string -> t
  val pp : t -> string
end

type t = [ System.t | `C | `OpenCL ]

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string

val arm : t
val ppc : t
val x86 : t
val mips : t
val gpu_ptx : t
val c : t
val opencl : t
