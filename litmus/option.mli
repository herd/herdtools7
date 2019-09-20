(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***********************************************************)
(* Command line parsing modifies references of this module *)
(***********************************************************)

(* Helpers *)
type arg_triple =  string * Arg.spec * string
val argkm : string -> int ref -> string -> arg_triple
val argkm_withfun : string -> (int -> unit) -> string -> arg_triple
val argstringo : string option ref -> Arg.spec
val arginto : int option ref -> Arg.spec
val argstring : string -> string ref -> string -> arg_triple
val argstring_withfun :
    string -> (string -> unit) -> string -> arg_triple
val argint : string -> int ref -> string -> arg_triple
val argboolfun : string -> (bool -> unit) -> string -> arg_triple
val argbool : string -> bool ref -> string -> arg_triple
val argboolo : string -> bool option ref -> string -> arg_triple
val argfloato : string -> float option ref -> string -> arg_triple
(* Verbose *)
val verbose : int ref

(* Somehow special *)
val cross : bool ref
val set_tar : string -> unit
val set_cross : string -> unit
val is_out : unit -> bool
val get_tar : unit -> string

val logicalprocs : int list option ref
val set_logicalprocs : string -> unit

(* Direct acccess to references *)
val crossrun : Crossrun.t ref
val adbdir : string ref
val index : string option ref
val hexa : bool ref
val limit : bool ref
val no : string option ref
val hint : string option ref
val avail : int option ref
val size : int ref
val runs : int ref
val noccs : int ref
val timelimit : float option ref
val barrier : Barrier.t ref
val verbose_barrier : bool ref
val verbose_prelude : bool option ref
val driver : Driver.t ref
val threadstyle : ThreadStyle.t ref
val launch : Launch.t ref
val memory : Memory.t ref
val contiguous : bool ref
val stride : Stride.t ref
val preload : Preload.t ref
val collect : Collect.t ref
val safer : Safer.t ref
val cautious : bool ref
val affinity : Affinity.t ref
val force_affinity : bool ref
val smtmode : Smt.t ref
val smt : int ref
val nsockets : int ref
val alloc : Alloc.t ref
val doublealloc : bool ref
val noalign : Align.t option ref
val speedcheck : Speedcheck.t ref
val makevar : string list ref
val gcc : string ref
val c11 : bool ref
val c11_fence : bool ref
val ascall : bool ref
val stdio : bool option ref
val linkopt : string ref
val targetos : TargetOS.t ref
val gas : bool option ref
val set_gas : bool -> unit
val asmcomment : string option ref
val asmcommentaslabel : bool ref
val get_numeric_labels : unit -> bool
val timeloop : int ref
val set_timeloop : int -> unit
val kind : bool ref
val names : string list ref
val excl : string list ref
val rename : string list ref
val kinds : string list ref
val set_kinds : string -> unit
val conds : string list ref
val set_conds : string -> unit
val nstates : string list ref
val set_nstates : string -> unit
val exit_cond : bool ref
val sleep : int ref
val isync : bool ref
val syncconst : int
val syncmacro : int ref
val xy : bool ref
val morearch : MoreArch.t ref
val pldw : bool ref
val cacheflush : bool ref
val carch : Archs.System.t option ref
val mode : Mode.t ref
val usearch : UseArch.t ref
val variant : (Variant_litmus.t -> bool) ref

(* Arch dependent option *)
type opt

val get_default : [< Archs.t ] -> opt
val get_dependent : unit -> (opt -> opt)

val set_delay : int -> unit
val get_delay : opt -> int

val set_word : Word.t -> unit
val get_word : opt -> Word.t

val set_gccopts : string -> unit
val get_gccopts : opt -> string

val set_line : int -> unit
val get_line : opt -> int

val set_carch : Archs.System.t -> unit

(* *)
