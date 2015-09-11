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

(* Main mode *)
type mode = Explo | Conform

val parse_mode : string -> mode option
val pp_mode : mode -> string

(* Execution of tests *)
type mach =
  | Local
  | Distant of string
  | Simul of string * string
  | Cross of string * string

val parse_mach : string -> mach option
val pp_mach : mach -> string


(* Interpretation *)

type interpretation = Single | Multi

val parse_interpretation : string -> interpretation option
val pp_interpretation : interpretation -> string

(* Checkpoint *)

val ckpt_name : string

(* Options *)
type t =
  {
   arch : Archs.t ;
   output : string ; (* Directory for all output *)
   testing : string option ; 
   safe : string option ;
   mode : mode ;
   mach : mach ;
   interpretation : interpretation ;
   work_dir : string ;
   nprocs : int option ;
   diy_sz : int option ;
   litmus_opts : string ;
   run_opts : string list ;
   verbose : int ;
   interactive : bool ;
   force_interactive : bool ;
   build : string ;
   stabilise : int ;
   compress : bool ;
   distrm : string ;
   distaddpath : string list ;
   diy_opts : string list;
   transitive : bool ;
  }

val pp_opt : out_channel -> t -> unit

val default : t

val incr_verbose : t -> t
val set_interactive : bool -> t -> t
val set_transitive : bool -> t -> t
val set_compress : bool -> t -> t
val set_arch : string -> t -> t
val set_mode : string -> t -> t
val set_nprocs : int -> t -> t
val get_nprocs : Archs.t -> t -> int
val set_diy_opts : string -> t -> t
val get_diy_opts : t -> string list
