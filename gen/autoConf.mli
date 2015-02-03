(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


module type S = sig
  module A : AutoArch.S

(* Options for configuration *)
  val testing : string
  val safe : string
  val mode : AutoOpt.mode
  val mach : AutoOpt.mach
  val nprocs : int
  val diy_sz : int
  val litmus_opts : string
  val my_dir : string
  val dist_dir : string
  val run_opts : string list
  val verbose : int
  val interactive : bool
  val build : string (* command to build .exe files *)

(* Interpretation of cycles *)
  module I : AutoInterpret.S
  with type outcome = A.L.outcome
  and type relax = A.R.relax
  and type relax_set = A.R.Set.t
  and type count = int A.R.Map.t

(* Sumary of options (for checkpoint) *)
  val opt : AutoOpt.t
end

val mk_config : AutoOpt.t -> (module S)

