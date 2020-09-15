(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


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
