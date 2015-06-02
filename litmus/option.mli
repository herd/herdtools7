(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***********************************************************)
(* Command line parsing modifies references of this module *)
(***********************************************************)

(* Helpers *)
val argkm : string -> int ref -> string -> string * Arg.spec * string
val argkm_withfun : string -> (int -> unit) -> string -> string * Arg.spec * string
val argstringo : string option ref -> Arg.spec
val arginto : int option ref -> Arg.spec
val argstring : string -> string ref -> string -> string * Arg.spec * string
val argstring_withfun :
    string -> (string -> unit) -> string -> string * Arg.spec * string
val argint : string -> int ref -> string -> string * Arg.spec * string
val argbool : string -> bool ref -> string -> string * Arg.spec * string
val argboolo : string -> bool option ref -> string -> string * Arg.spec * string
val argfloato : string -> float option ref -> string -> string * Arg.spec * string
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
val detached : bool ref
val launch : Launch.t ref
val memory : Memory.t ref
val contiguous : bool ref
val stride : int ref
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
val speedcheck : Speedcheck.t ref
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
val rename : string option ref
val kinds : string list ref
val set_kinds : string -> unit
val conds : string list ref
val set_conds : string -> unit
val nstates : string list ref
val set_nstates : string -> unit
val sleep : int ref
val isync : bool ref
val syncconst : int
val syncmacro : int ref
val xy : bool ref
val morearch : MoreArch.t ref
val pldw : bool ref
val carch : Archs.System.t option ref
val mode : Mode.t ref
val usearch : UseArch.t ref

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
