(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type CommonConfig = sig
  val show : PrettyConf.show
  val nshow : int option
  val restrict : Restrict.t
  val outcomereads : bool
  val badexecs : bool
  val badflag : string option
  val throughflag : string option
  include Mem.CommonConfig
  val statelessrc11 : bool
  val skipchecks : StringSet.t
  val dumpallfaults : bool
end

module type Config = sig
  include CommonConfig
  val byte : MachSize.sz
  val dirty : DirtyBit.t option
end

module TestResult : sig
  type observation = Never | Always | Sometimes

  val pp_observation : observation -> string

  type 'stset stats

  val states : 's stats -> 's
  (** Final states. *)

  val failed_candidates : 's stats -> int
  (** Number of candidate executions rejected by the memory model. *)

  val candidates : 's stats -> int
  (** Number of valid candidate executions, i.e. accepted by the memory model. *)

  val positive : 's stats -> int
  (** Number of valid candidate executions satisfying the test condition. *)

  val negative : 's stats -> int
  (** Number of valid candidate executions not satisfying the test condition. *)

  val observation : 's stats -> observation
  (** How the test condition is observed. *)

  val flagged : 's stats -> int list Flag.Map.t
  (** Flags attached to accounted candidate executions. *)

  val cutoff : 's stats -> string option
  (** Description of the first unrolling cutoff encountered, if any. *)

  val has_bad_execs : badflag:string option -> 'stset stats -> bool
  (** [has_bad_execs ~badflag stats] is [true] when [stats] contains an
      undefined execution, or an execution carrying [badflag] when one is
      configured. *)

  type ('conc, 'sets, 'rels) execution

  val concrete : ('conc, 'sets, 'rels) execution -> 'conc

  val passes_check : ('conc, 'sets, 'rels) execution -> bool
  (** Whether the execution satisfies the test proposition. *)

  val flags : ('conc, 'sets, 'rels) execution -> Flag.Set.t

  val sets : ('conc, 'sets, 'rels) execution -> 'sets

  val relations : ('conc, 'sets, 'rels) execution -> 'rels

  module Make (S : SemExtra.S) : sig
    type nonrec stats = S.A.StateSet.t stats
    type nonrec execution = (S.concrete, S.set_pp, S.rel_pp) execution
    type t = S.event_structure list * ((execution -> unit) -> stats)
  end
end

module Make (O: Config)(M:XXXMem.S) : sig
  type test_results = TestResult.Make(M.S).t

  val run : M.S.test -> M.S.test * test_results
end

module type PrinterConfig = sig
  module PC : PrettyConf.S
  val candidates : bool
  val showkind : bool
  val shortlegend : bool
  val verbose : int
  val show : PrettyConf.show
  val speedcheck : Speed.t
  val badflag : string option
end

module Printer (O : PrinterConfig) (S : SemExtra.S) : sig
  type stats := TestResult.Make(S).stats
  type execution := TestResult.Make(S).execution

  val pp_stats : time:float -> S.test -> stats -> Format.formatter -> unit
  val dump_exec_graph : Model.t -> S.test -> execution -> out_channel -> unit
end
