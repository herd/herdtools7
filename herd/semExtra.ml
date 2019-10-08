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

(** Centralized definition for types to be included by the XXXSem *)

(* Some configuration *)
module type Config = sig
  val optace : bool
  val debug : Debug_herd.t
  val variant : Variant.t -> bool
  val byte : MachSize.sz
  val endian : Endian.t option
  module PC : PrettyConf.S
end


module type S = sig
  module O : Config (* Options, for Sem consumer *)
  module A   : Arch_herd.S
  module E : Event.S with module A = A and module Act.A = A
  module M  : Monad.S
  with module A = A and module E = E and type evt_struct = E.event_structure
  module Cons : Constraints.S with module A = A

(* A good place to (re)define all these types *)
  type cst = A.V.Cst.v
  type v = A.V.v
  type proc = A.proc
  type instruction = A.instruction
  type global_loc = A.global_loc
  type location = A.location
  type state = A.state
  type program = A.program

  type prop = A.prop

  type constr = A.constr

  type nice_prog = A.nice_prog
  type start_points = A.start_points

  type test =
      (program, nice_prog, start_points,
       state, A.size_env, prop, location, A.LocSet.t) Test_herd.t

(* Get list of locations observed in outcomes *)
  type loc_set = A.LocSet.t
  val observed_locations : test -> loc_set
  val displayed_locations : test -> loc_set
  val size_env : test -> A.size_env

  type event = E.event
  type event_structure = E.event_structure
  type event_set = E.EventSet.t
  type event_rel = E.EventRel.t

  (* Abreviations *)
  val tr : event_rel -> event_rel
  val rt : event_rel -> event_rel
  val restrict :
      (event -> bool) -> (event -> bool) ->
        event_rel -> event_rel
  val doWW : event_rel -> event_rel
  val doWR : event_rel -> event_rel
  val doRR : event_rel -> event_rel
  val doRW : event_rel -> event_rel

  val seq : event_rel -> event_rel -> event_rel
  val seqs : event_rel list -> event_rel
  val union : event_rel -> event_rel -> event_rel
  val union3 : event_rel -> event_rel -> event_rel -> event_rel
  val unions : event_rel list -> event_rel

  (* relations packed to be shown on graphs *)
  type rel_pp = (string * event_rel) list

  (* Dependencies : ie complement for ace *)
  type procrels =
      { addr : event_rel;
        data : event_rel;
        depend : event_rel;
        data_commit : event_rel;
        ctrl : event_rel;
        ctrlisync : event_rel;
        success : event_rel;
        rf : event_rel;
        amo : event_rel; }

(*********)
(* RFMap *)
(*********)

 type write_to =
   | Final of location
   | Load of event

 type read_from =
    | Init
    | Store of event


  val write_to_compare : write_to -> write_to -> int
  val read_from_compare : read_from -> read_from -> int
  val read_from_equal : read_from -> read_from -> bool
  val event_rf_equal : event -> read_from -> bool

  module RFMap : Map.S with type key = write_to

  type rfmap = read_from RFMap.t

(* For pretty print, string arg is like the one of String.concat *)
  val pp_rfmap :
      out_channel -> string ->
        (out_channel -> write_to -> read_from -> unit) ->
          rfmap ->  unit

  val for_all_in_rfmap : (write_to -> read_from -> bool) -> rfmap -> bool

  val simplify_vars_in_rfmap : A.V.solution -> rfmap -> rfmap

(**************************************)
(* Complete, concrete event structure *)
(**************************************)


type concrete =
    {
     str   : event_structure ; (* event structure proper *)
     rfmap : rfmap ;           (* rfmap *)
     fs    : state ;           (* final state *)
     po  : event_rel ;         (* program order (in fact po + iico) *)
     pos : event_rel ;         (* Same location same processor accesses *)
(* Write serialization precursor ie uniproc induced constraints over writes *)
     pco : event_rel ;
(* View before relation deduced from rfmaps *)
     store_load_vbf : event_rel ; (* stores preceed their loads *)
     init_load_vbf : event_rel;   (* load from init preceed all stores *)
     last_store_vbf : event_rel;  (* stores to final state come last *)
     atomic_load_store : event_rel; (* eg load-and-link/store conditional *)
    }

  val conc_zero : concrete

(************)
(* Branches *)
(************)
  module B : Branch.S
  with type v = v and type 'a monad = 'a M.t

  type branch = B.t

(************)
(* Barriers *)
(************)

(* barrier + cumulativity *)
  type barrier = A.barrier
  type pp_barrier = { barrier:barrier ; pp:string; }

end

module Make(C:Config) (A:Arch_herd.S) (Act:Action.S with module A = A)
       : (S with module A = A and module E.Act = Act) =
  struct
    module O = C
    module A = A
    module V = A.V
    module E = Event.Make(C)(A)(Act)
    module CEM = struct (* Configure event monads *)
      let hexa =  C.PC.hexa
      let debug = C.debug
      let variant = C.variant
      let byte = C.byte
    end
    module M = EventsMonad.Make(CEM)(A)(E)
    module Cons = Constraints.Make (C.PC)(A)

(* A good place to (re)define all these types *)
    type cst = A.V.Cst.v
    type v = A.V.v
    type proc = A.proc
    type instruction = A.instruction
    type global_loc = A.global_loc
    type location = A.location
    type state = A.state

    type prop = A.prop
    type constr = A.constr

    type program = A.program
    type nice_prog = A.nice_prog
    type start_points = A.start_points

    type test =
      (program, nice_prog, start_points,
       state, A.size_env, prop, location, A.LocSet.t) Test_herd.t

(* List of relevant location *)
    type loc_set = A.LocSet.t
    let observed_locations t = t.Test_herd.observed
    and displayed_locations t = t.Test_herd.displayed
    and size_env t =  t.Test_herd.size_env

    type event = E.event

    let event_compare = E.event_compare

    type event_set = E.EventSet.t

    type event_structure = E.event_structure

    type event_rel = E.EventRel.t
  (* Abreviations *)
    let tr = E.EventRel.transitive_closure
    let rt = E.EventRel.remove_transitive_edges
    let restrict = E.EventRel.restrict_domains
    let doRR = restrict E.is_mem_load E.is_mem_load
    let doRW = restrict E.is_mem_load E.is_mem_store
    let doWW = restrict E.is_mem_store E.is_mem_store
    let doWR = restrict E.is_mem_store E.is_mem_load
    let seq = E.EventRel.sequence
    let seqs =  E.EventRel.sequences
    let union = E.EventRel.union
    let union3 = E.EventRel.union3
    let unions = E.EventRel.unions

        (* relations packed to be shown on graphs *)
    type rel_pp = (string * event_rel) list

  (* Dependencies : ie complement for ace *)
  type procrels =
      { addr : event_rel;
        data : event_rel;
        depend : event_rel;
        data_commit : event_rel;
        ctrl : event_rel;
        ctrlisync : event_rel;
        success : event_rel;
        rf : event_rel;
        amo : event_rel; }

(* Read-From maps exploitation *)
    type write_to =
      | Final of location
      | Load of event

    type read_from =
      | Init
      | Store of event

    let write_to_compare wt1 wt2 =  match wt1,wt2 with
    | Final loc1, Final loc2 -> A.location_compare loc1 loc2
    | Final _, Load _ -> -1
    | Load _,Final _  -> 1
    | Load e1, Load e2 -> event_compare e1 e2

    let read_from_compare rf1 rf2 =  match rf1,rf2 with
    | Init, Init -> 0
    | Init, Store _ -> -1
    | Store _,Init  -> 1
    | Store e1, Store e2 -> event_compare e1 e2

    let read_from_equal rf1 rf2 = read_from_compare rf1 rf2 = 0

    let event_rf_equal e rf = match rf with
    | Init -> false
    | Store e' -> E.event_equal e e'

    module RFMap =
      Map.Make
        (struct
          type t = write_to
          let compare = write_to_compare
        end)

    type rfmap = read_from RFMap.t

    let pp_rfmap chan delim pp rfm =
      let first = ref true in
      RFMap.iter
        (fun wt rf ->
          if not !first then output_string chan delim
          else first := false ;
          pp chan wt rf)
        rfm

    let for_all_in_rfmap pred rfm =
      RFMap.fold
        (fun wt rf k -> pred wt rf && k)
        rfm true

    let simplify_rf solns = function
      | Init -> Init
      | Store e -> Store (E.simplify_vars_in_event solns e)
    and simplify_wt solns = function
      | Final loc -> Final (A.simplify_vars_in_loc solns loc)
      | Load e -> Load (E.simplify_vars_in_event solns e)

    let simplify_vars_in_rfmap solns rfm =
      if V.Solution.is_empty solns then rfm
      else
        RFMap.fold
          (fun wt rf k ->
            RFMap.add
              (simplify_wt solns wt)
              (simplify_rf solns rf)
              k)
          rfm RFMap.empty

(**************************************)
(* Complete, concrete event structure *)
(**************************************)
type concrete =
    {
     str   : event_structure ; (* event structure proper *)
     rfmap : rfmap ;           (* rfmap *)
     fs    : state ;           (* final state *)
     po : event_rel ;
     pos : event_rel ;      (* Same location same processor accesses *)
     pco : event_rel ;
(* View before relation deduced from rfmaps *)
     store_load_vbf : event_rel ; (* stores preceed their loads *)
     init_load_vbf : event_rel;   (* load from init preceed all stores *)
     last_store_vbf : event_rel;  (* stores to final state come last *)
     atomic_load_store : event_rel; (* eg load-and-link/store conditional *)
    }

    let conc_zero =
      {
       str = E.empty_event_structure ;
       rfmap = RFMap.empty ;
       fs = A.state_empty ;
       po = E.EventRel.empty ;
       pos = E.EventRel.empty ;
       pco = E.EventRel.empty ;
       store_load_vbf  = E.EventRel.empty ;
       init_load_vbf = E.EventRel.empty ;
       last_store_vbf = E.EventRel.empty ;
       atomic_load_store = E.EventRel.empty ;
     }
(************)
(* Branches *)
(************)
    module B = Branch.Make(M)
    type branch = B.t

(************)
(* Barriers *)
(************)

    type barrier = A.barrier

    type pp_barrier = { barrier:barrier ; pp:string; }

  end

module ConfigToArchConfig(C:Config) : ArchExtra_herd.Config =
  struct
    let texmacros = C.PC.texmacros
    let hexa = C.PC.hexa
    let brackets = C.PC.brackets
    let variant = C.variant
    let byte = C.byte
    let endian = C.endian
  end
