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

(** Operations on events *)

open Printf

module type S = sig

  module A : Arch_herd.S

  module Act : Action.S

  type action = Act.action

  type eiid = int

(*
  eiid = unique event id
  iiid = id of instruction that generated this event; None for init writes
*)
  type event = {
      eiid : eiid;
      iiid : A.inst_instance_id option;
      action : action;  }

(* Only basic printing is here *)
  val pp_eiid       : event -> string
  val pp_action     : event -> string
  val debug_event : out_channel -> event -> unit

(***************************)
(* Procs and program order *)
(***************************)

  val proc_of       : event -> A.proc option
  val same_proc     : event -> event -> bool
  val same_proc_not_init : event -> event -> bool
  val progorder_of  : event -> A.program_order_index option

(* Is e1 before e2 w.r.t. prog order ? Nothing assumed on e1 and e2 *)
  val po_strict : event -> event -> bool
  val before_in_po : event -> event -> bool
  val po_eq : event -> event -> bool

(************************)
(* Predicates on events *)
(************************)

(* relative to memory *)
  val is_mem_store : event -> bool
  val is_mem_store_init : event -> bool
  val is_mem_load : event ->  bool
  val is_additional_mem_load : event ->  bool (* trylock... *)
  val is_mem : event -> bool
(* includes additional memory events,  eg lock, unlocks... *)
  val is_additional_mem : event -> bool
  val is_atomic : event -> bool
  val is_amo : event -> bool
  val get_mem_dir : event -> Dir.dirn
  val get_mem_size : event -> MachSize.sz

(* relative to the registers of the given proc *)
  val is_reg_store : event -> A.proc -> bool
  val is_reg_load : event -> A.proc -> bool
  val is_reg : event -> A.proc -> bool

(* Reg events, proc not specified *)
  val is_reg_store_any : event -> bool
  val is_reg_load_any : event -> bool
  val is_reg_any : event -> bool

(* Store/Load to memory or register *)
  val is_store : event -> bool
  val is_load : event -> bool

(* Access events of the same category *)
  val compatible_accesses : event -> event -> bool

(* Barriers *)
  val is_barrier : event -> bool
  val barrier_of : event -> A.barrier option

(*
  val same_barrier_id : event -> event -> bool
*)
  val is_isync : event -> bool

(* Commit *)
  val is_commit_bcc : event -> bool
  val is_commit_pred : event -> bool
  val is_commit : event -> bool

(**************)
(* Event sets *)
(**************)

  val event_compare : event -> event -> int
  val event_equal :  event -> event -> bool

  module OrderedEvent : MySet.OrderedType with type t = event

  module EventSet : MySet.S
  with type elt = event

  type event_set = EventSet.t
  val debug_events : out_channel -> event_set -> unit

  module EventSetSet : MySet.S
  with type elt = event_set

(*************)
(* Event map *)
(*************)

  module EventMap : Map.S with type key = event

(************************)
(* Event set restricted *)
(************************)

(* relative to memory *)
  val mem_stores_of : EventSet.t -> EventSet.t
  val mem_stores_init_of : EventSet.t -> EventSet.t
  val mem_loads_of : EventSet.t -> EventSet.t
  val mem_of : EventSet.t -> EventSet.t
  val atomics_of : EventSet.t -> EventSet.t

(* relative to the registers of the given proc *)
  val reg_stores_of : EventSet.t  -> A.proc -> EventSet.t
  val reg_loads_of : EventSet.t -> A.proc -> EventSet.t

(* Proc not checked *)
  val reg_stores : EventSet.t  ->  EventSet.t
  val reg_loads : EventSet.t -> EventSet.t

(* For all locations *)
  val stores_of : EventSet.t -> EventSet.t
  val loads_of :  EventSet.t -> EventSet.t

(* Barriers *)
  val barriers_of : EventSet.t -> EventSet.t

(* Commit *)
  val commits_of :  EventSet.t -> EventSet.t

(***********************)
(* Relations on events *)
(***********************)

  module EventRel : InnerRel.S
  with type elt0 = event
  and module Elts = EventSet

  type event_rel = EventRel.t

  val debug_rel : out_channel -> event_rel -> unit

  type event_structure = {
      procs : A.proc list ;
      events : EventSet.t ;                     (* really a set *)
      intra_causality_data : EventRel.t ;       (* really a partial order relation *)
      intra_causality_control : EventRel.t ;    (* really a partial order relation *)
      (* If style control inside structure *)
      control : EventRel.t ;
      (* Events that lead to the data port of a W *)
      data_ports : EventSet.t ;
      (* some special output port, i.e. store conditional success as reg write *)
      success_ports : EventSet.t ;
      (* Result of structure, by default maximal iico *)
      output : EventSet.t option ;
      (* Equivalence classes of events generated by the same memory accesses *)
      sca : EventSetSet.t ;
      (* Original events, before splitted in sub-accesses (mixed-size).
       * NB: not included in events above *)
      mem_accesses : EventSet.t ;
    }

  val procs_of    : event_structure -> A.proc list
  val locs_of     : event_structure -> A.location list

(* map f over all events in event_structure *)
  val map_event_structure :
      (event -> event) -> event_structure -> event_structure

  (*****************************************************************)
  (* Those projection return lists of event sets/relations by proc *)
  (*****************************************************************)

  (* project events by executing proc *)
  val proj_events : event_structure -> event_set list

  (* relation must operate on events of the same proc *)
  val proj_rel : event_structure -> event_rel -> event_rel list
  (* relation must be as before, or one of the related
     events is be a mem_store *)
  val proj_proc_view : event_structure -> event_rel -> event_rel list

  (* e1 < e2 in UNION (strict_po,iico) ? *)
  val strict_before_po_iico : event_structure -> event -> event -> bool


(********************)
(* Equation solving *)
(********************)

  val undetermined_vars_in_event_structure : event_structure -> A.V.ValueSet.t
  val simplify_vars_in_event : A.V.solution -> event -> event
  val simplify_vars_in_event_structure :
      A.V.solution -> event_structure -> event_structure

(*************************************)
(* Access to sub_components of events *)
(*************************************)
  val value_of : event -> A.V.v option (* Warning: fails on RMW actions *)
  val read_of : event -> A.V.v option
  val written_of : event -> A.V.v option
  val location_of   : event -> A.location option
  val location_reg_of : event -> A.reg option
  val global_loc_of    : event -> A.global_loc option

(****************************)
(* Convenience on locations *)
(****************************)

  val location_compare : event -> event -> int
  val same_location : event -> event -> bool
  val same_value : event -> event -> bool
  val is_visible_location : A.location -> bool


(************************)
(* Parallel composition *)
(************************)
  val (=|=) :
      event_structure -> event_structure -> event_structure option

(* sequential composition, add data dependency *)
  val (=*$=) :
      event_structure -> event_structure -> event_structure option

(* Identical, keep first event structure output as output... *)
  val (=$$=) :
      event_structure -> event_structure -> event_structure option

(* sequential composition, add control dependency *)
  val (=**=) :
      event_structure -> event_structure -> event_structure option

(* similar, additionally avoid some evts in target links *)
  val bind_ctrl_avoid : event_set ->  event_structure -> event_structure -> event_structure option

(* check memory tags *)
  val check_tags :
      event_structure -> event_structure -> event_structure -> event_structure

(* exchange composition :
   xch rx ry wx wy ->
      rx -data-> wy, ry -data-> wx
      rx -ctrl-> wx, ry -ctrl-> wy
*)
  val exch :
     event_structure -> event_structure ->
     event_structure -> event_structure ->
     event_structure

  val swp :
     event_structure -> event_structure ->
     event_structure -> event_structure ->
     event_structure -> event_structure

  val linux_exch :
      event_structure -> event_structure ->
        event_structure -> event_structure ->
          event_structure

  val amo :
      event_structure -> event_structure ->
        event_structure -> event_structure ->
          event_structure

  val linux_cmpexch_ok :  event_structure -> event_structure ->
    event_structure -> event_structure -> event_structure ->
      event_structure

  val linux_cmpexch_no :  event_structure -> event_structure ->
    event_structure -> event_structure

  val linux_add_unless_ok :  event_structure -> event_structure ->
    event_structure -> event_structure -> event_structure -> bool ->
      event_structure

  val linux_add_unless_no :  event_structure -> event_structure ->
    event_structure -> bool -> event_structure

  val riscv_sc :
      bool (* success *) ->
        event_structure -> event_structure -> event_structure ->
          event_structure ->  event_structure ->  event_structure ->
            event_structure

  val aarch64_cas_ok :
        event_structure -> event_structure -> event_structure ->
          event_structure ->  event_structure ->  event_structure ->
            event_structure

(* stu computation :
   stu rD rEA wEA wM ->
      rEA -data-> wEA,
      rEA -data-> wM,
      rD -data-> wM *)
  val stu :
     event_structure -> event_structure ->
     event_structure -> event_structure ->
     event_structure


(* Parallel, for different instructions *)
  val (+|+) :
      event_structure -> event_structure -> event_structure option

  val empty_event_structure   : event_structure
  val is_empty_event_structure : event_structure -> bool

(* Condition at instruction level *)
  val cond_comp :
      event_structure -> event_structure -> event_structure

end

module type Config = sig
  val variant : Variant.t -> bool
end

module Make  (C:Config) (AI:Arch_herd.S) (Act:Action.S with module A = AI) :
  (S with module A = AI and module Act = Act) =
struct

  module Act = Act
  module A = AI
  module V = AI.V

  type eiid = int

  type action = Act.action

    type event = {
        eiid : eiid;
        iiid : A.inst_instance_id option ;
        action : action;  }


    let pp_eiid e =
      if e.eiid < 26 then
        String.make 1 (Char.chr (Char.code 'a' + e.eiid))
      else "ev"^string_of_int e.eiid

    let pp_action e = Act.pp_action e.action

    let debug_event chan e =
      fprintf chan
        "(eeid=%s action=%s)" (pp_eiid e) (pp_action e)

(* Utility functions to pick out components *)
    let value_of e = Act.value_of e.action
    let read_of e = Act.read_of e.action
    let written_of e = Act.written_of e.action
    let location_of e = Act.location_of e.action

    let  location_reg_of e = match location_of e with
    | Some (A.Location_reg (_,r)) -> Some r
    | _ -> None

    let  global_loc_of e = match location_of e with
    | Some (A.Location_global a) -> Some a
    | _ -> None

    let location_compare e1 e2 = match location_of e1,location_of e2 with
    | Some loc1,Some loc2 -> A.location_compare loc1 loc2
    | _,_ -> assert false

(* Visible locations *)
    let is_visible_location  = function
      | A.Location_global _|A.Location_deref _ -> true
      | A.Location_reg _ -> false

    let same_location e1 e2 =
      if (location_of e1 = None || location_of e2 = None) then
        false
      else
        location_compare e1 e2 = 0

    let same_value e1 e2 = match value_of e1, value_of e2 with
    | Some v1,Some v2 -> V.compare v1 v2 = 0
    | _,_ -> assert false

    let proc_of e = match e.iiid with
    | Some i -> Some i.A.proc
    | None -> None

    let same_proc e1 e2 = match proc_of e1, proc_of e2 with
    | Some p1,Some p2 -> Misc.int_eq p1 p2
    | (None,Some _)|(Some _,None) -> false
    | None,None -> true

    let same_proc_not_init e1 e2 = match proc_of e1, proc_of e2 with
    | Some p1,Some p2 -> Misc.int_eq p1 p2
    | (None,Some _)|(Some _,None)
    | None,None -> false

    let progorder_of e = match e.iiid with
    | Some i -> Some i.A.program_order_index
    | None -> None


(************************)
(* Predicates on events *)
(************************)

    let before_in_po e1 e2 =
      proc_of e1 = proc_of e2 &&
      (progorder_of e1 < progorder_of e2 ||
       progorder_of e1 = progorder_of e2 )

    let po_strict e1 e2 =
      proc_of e1 = proc_of e2 && progorder_of e1 < progorder_of e2

    let po_eq e1 e2 =
      proc_of e1 = proc_of e2 && progorder_of e1 = progorder_of e2

(* relative to memory *)
    let is_mem_store e = Act.is_mem_store e.action

    let is_mem_store_init e = match e.iiid with
    | None -> true
    | Some _ -> false

    let is_mem_load e = Act.is_mem_load e.action
    let is_additional_mem_load e = Act.is_additional_mem_load e.action
    let is_mem e = Act.is_mem e.action
    let is_additional_mem e = Act.is_additional_mem e.action
    let is_atomic e = Act.is_atomic e.action
    let is_amo e = match e.iiid with
    | Some {A.inst=i; _} when A.is_amo i -> Act.is_mem_store e.action
    | _ -> false
    let get_mem_dir e = Act.get_mem_dir e.action
    let get_mem_size e = Act.get_mem_size e.action

(* relative to the registers of the given proc *)
    let is_reg_store e (p:int) = Act.is_reg_store e.action p
    let is_reg_load e (p:int) = Act.is_reg_load e.action p
    let is_reg e (p:int) = Act.is_reg e.action p

(* Store/Load anywhere *)
    let is_store e = Act.is_store e.action
    let is_load e = Act.is_load e.action
    let is_reg_any e = Act.is_reg_any e.action
    let is_reg_store_any e = Act.is_reg_store_any e.action
    let is_reg_load_any e = Act.is_reg_load_any e.action

(* Compatible events ie accesses of the same category *)
  let compatible_accesses e1 e2 =
    Act.compatible_accesses e1.action e2.action

(* Barriers *)
    let is_barrier e = Act.is_barrier e.action
    let barrier_of e = Act.barrier_of e.action
(*
    let same_barrier_id e1 e2 =
      Act.same_barrier_id e1.action e2.action
*)
  let is_isync e = Act.is_isync e.action

(* Commits *)
  let is_commit_bcc e = Act.is_commit_bcc e.action
  let is_commit_pred e = Act.is_commit_pred e.action
  let is_commit e =
    let act = e.action in
    Act.is_commit_bcc act ||  Act.is_commit_pred act

(******************************)
(* Build structures of events *)
(******************************)

    let event_compare e1 e2 = Misc.int_compare e1.eiid e2.eiid
    let event_equal e1 e2 = Misc.int_eq e1.eiid e2.eiid

    module OrderedEvent = struct
      type t = event
      let compare = event_compare
    end


    module EventSet = MySet.Make(OrderedEvent)
    type event_set = EventSet.t

    let debug_events chan es =
      fprintf chan "{" ;
      EventSet.pp chan ", " debug_event es ;
      fprintf chan "}" ;
      ()

  module EventSetSet = MySet.Make(EventSet)

(* relative to memory *)
    let mem_stores_of = EventSet.filter is_mem_store
    let mem_stores_init_of = EventSet.filter is_mem_store_init
    let mem_loads_of es = EventSet.filter is_mem_load es
    let mem_of es = EventSet.filter is_mem es
    let atomics_of es = EventSet.filter is_atomic es

(* relative to the registers of the given proc *)
    let reg_stores_of es p =
      EventSet.filter (fun e -> is_reg_store e p) es
    let reg_loads_of es p =
      EventSet.filter (fun e -> is_reg_load e p) es

(* Everywhere *)
    let reg_stores es = EventSet.filter is_reg_store_any es
    let reg_loads es =  EventSet.filter is_reg_load_any es
    let stores_of es = EventSet.filter is_store es
    let loads_of es = EventSet.filter is_load es

(* Barriers *)
    let barriers_of es =  EventSet.filter is_barrier es

(* Commits *)
    let commits_of es =  EventSet.filter is_commit es

    module EventMap = Map.Make(OrderedEvent)

(*************)
(* Relations *)
(*************)

    module EventRel = InnerRel.Make(OrderedEvent)
    type event_rel = EventRel.t

    let debug_event_in_rel chan e = fprintf chan "%s" (pp_eiid e)

    let debug_rel chan r =
      EventRel.pp chan ","
        (fun chan (e1,e2) -> fprintf chan "%a -> %a"
            debug_event_in_rel e1 debug_event_in_rel e2)
        r

    type event_structure = {
        procs : A.proc list ; (* will prove convenient *)
        events : EventSet.t ;        (* really a set *)
        intra_causality_data : EventRel.t ;   (* really a (partial order) relation *)
        intra_causality_control : EventRel.t ;(* really a (partial order) relation *)
        control : EventRel.t ;
        data_ports : EventSet.t ; success_ports : EventSet.t ;
        output : EventSet.t option ;
        sca : EventSetSet.t ;
        mem_accesses : EventSet.t ;
      }

    let procs_of es = es.procs

    let locs_of es = EventSet.fold (fun e k -> match location_of e with Some l -> l::k | None -> k) es.events []

    let map_event_structure f es =
      let map_rel = EventRel.map (fun (e1,e2) -> f e1,f e2)
      and map_set = EventSet.map f in
      { procs = es.procs ;
       events = map_set es.events ;
       intra_causality_data = map_rel  es.intra_causality_data ;
       intra_causality_control = map_rel es.intra_causality_control ;
       control = map_rel es.control ;
       data_ports = map_set es.data_ports ;
       success_ports = map_set es.success_ports ;
       output = Misc.app_opt map_set es.output ;
       sca = EventSetSet.map map_set es.sca ;
       mem_accesses = map_set es.mem_accesses ;

      }

    let empty =
      { procs = [] ; events = EventSet.empty ;
        intra_causality_data = EventRel.empty ;
        intra_causality_control = EventRel.empty ;
        control = EventRel.empty ;
        data_ports = EventSet.empty ; success_ports = EventSet.empty ;
        output = None ;
        sca = EventSetSet.empty ;
        mem_accesses = EventSet.empty ;
      }

  let is_empty_event_structure es =
    not (Misc.consp es.procs) &&
    EventSet.is_empty es.events &&
    EventRel.is_empty es.intra_causality_data &&
    EventRel.is_empty es.intra_causality_control &&
    EventRel.is_empty es.control &&
    EventSet.is_empty es.data_ports &&
    EventSet.is_empty es.success_ports &&
    Misc.is_none es.output

(****************************)
(* Projection of event set *)
(****************************)

    module Proj(S:MySet.S) = struct

      let rec add_env p e = function
        | [] -> assert false
        | (q,es as c)::env ->
            if Misc.int_eq p q then
              (q, S.add e es)::env
            else
              c::add_env p e env

      let proj procs_of ps es =
        let env = List.map (fun p -> p,S.empty) ps in
        let env =
          S.fold
            (fun e ->
              List.fold_right
                (fun p env -> add_env p  e env)
                (procs_of e))
            es env in
        List.map (fun (_p,es) -> es) env
    end

    module ProjSet = Proj(EventSet)

    let proj_events es =
      ProjSet.proj
        (fun e -> match proc_of e with
        | Some p -> [p]
        | None -> []) (procs_of es) es.events

    module ProjRel = Proj(EventRel)

    let proc_of_pair (e1,e2) =
      let p1 = proc_of e1 and p2 = proc_of e2 in
      match p1,p2 with
      | Some p1,Some p2 ->
          if Misc.int_eq p1 p2 then [p1]
          else []
      | _,_ -> []

    let proj_rel es rel =
      ProjRel.proj proc_of_pair (procs_of es) rel

    let proj_proc_view es rel =
      let proc_of (e1,e2) =
       let p1 = proc_of e1 and p2 = proc_of e2 in
       match p1,p2 with
       | Some p1, Some p2 ->
           if Misc.int_eq p1 p2 then [p1]
           else if is_mem_store e1 then [p2]
           else if is_mem_store e2 then [p1]
           else [] (* Can occur for X86CC -> no projected relation *)
      | None,Some p2 ->
           if is_mem_store e1 then [p2] else []
      | Some p1,None ->
          if is_mem_store e2 then [p1] else []
      | None,None -> [] in
      ProjRel.proj proc_of (procs_of es) rel

    let strict_before_po_iico es e1 e2 =
      let strict_po_reln =
        EventRel.of_pred es.events es.events po_strict in
      let iico_reln =
        EventRel.union
          es.intra_causality_data
          es.intra_causality_control in
      EventRel.mem_transitive (e1, e2) (EventRel.union strict_po_reln iico_reln)

    let undetermined_vars_in_event e =
      Act.undetermined_vars_in_action e.action

    let undetermined_vars_in_event_structure es =
      EventSet.fold
        (fun e k -> V.ValueSet.union (undetermined_vars_in_event e) k)
        es.events V.ValueSet.empty

    let simplify_vars_in_event soln e =
      {e with action = Act.simplify_vars_in_action soln e.action}

    let simplify_vars_in_event_structure soln es =
      if V.Solution.is_empty soln then es
      else map_event_structure (simplify_vars_in_event soln) es

(* Event structure manipulation *)
let minimals es =
  let intra_causality =
    EventRel.union
      es.intra_causality_data es.intra_causality_control in
  EventSet.filter
    (fun e ->
      EventRel.for_all
        (fun (_,e2) -> e.eiid <> e2.eiid) intra_causality)
    es.events

let minimals_avoid aset es =
  let intra_causality =
    let r =
      EventRel.union
        es.intra_causality_data es.intra_causality_control in
    EventRel.filter (fun (e,_) -> not (EventSet.mem e aset)) r in
  EventSet.filter
    (fun e ->
      EventRel.for_all
        (fun (_,e2) -> e.eiid <> e2.eiid) intra_causality)
    (EventSet.diff es.events aset)

let minimals_data es =
  let intra_causality = es.intra_causality_data in
  EventSet.filter
    (fun e ->
      EventRel.for_all
        (fun (_,e2) -> e.eiid <> e2.eiid) intra_causality)
    es.events

let minimals_control es =
  let intra_causality = es.intra_causality_control in
  EventSet.filter
    (fun e ->
      is_commit e &&
      EventRel.for_all
        (fun (_,e2) -> e.eiid <> e2.eiid) intra_causality)
    es.events

let maximals es =
  let intra_causality =
    EventRel.union es.intra_causality_data es.intra_causality_control in
  EventSet.filter
    (fun e ->
      EventRel.for_all
        (fun (e1,_) -> e.eiid <> e1.eiid) intra_causality)
    es.events

let maximals_data es =
  let intra_causality = es.intra_causality_data in
  EventSet.filter
    (fun e ->
      EventRel.for_all
        (fun (e1,_) -> e.eiid <> e1.eiid) intra_causality)
    es.events

let get_output es = match es.output with
| None -> maximals_data es
| Some o -> o


(**********************************)
(* Add together event structures  *)
(**********************************)

(* Checking events sets are disjoint *)

let check_disjoint do_it es1 es2 =
  assert (EventSet.disjoint es1.events es2.events) ;
  Some (do_it es1 es2)

(* Parallel composition *)
    let union_output es1 es2 = match es1.output,es2.output with
    | Some o1, Some o2 -> Some (EventSet.union o1 o2)
    | None,None -> None
    | None,Some o2 ->
        Some (EventSet.union (maximals_data es1) o2)
    | Some o1,None ->
        Some (EventSet.union o1 (maximals_data es2))

let para_comp es1 es2 =
  { procs = [] ;
    events = EventSet.union es1.events es2.events;
    intra_causality_data = EventRel.union
      es1.intra_causality_data es2.intra_causality_data ;
    intra_causality_control = EventRel.union
      es1.intra_causality_control  es2.intra_causality_control ;
    control = EventRel.union es1.control es2.control;
    data_ports = EventSet.union es1.data_ports es2.data_ports;
    success_ports = EventSet.union es1.success_ports es2.success_ports;
    output = union_output es1 es2;
    sca = EventSetSet.union es1.sca es2.sca;
    mem_accesses = EventSet.union es1.mem_accesses es2.mem_accesses; }

  let (=|=) = check_disjoint para_comp

(* Composition with intra_causality_data from first to second *)
  let data_comp mini_loc mkOut es1 es2 =
    { procs = [];  events = EventSet.union es1.events es2.events;
      intra_causality_data =
      EventRel.filter
        (fun (e1,e2) -> e1 != e2)
          (EventRel.union
            (EventRel.union es1.intra_causality_data
               es2.intra_causality_data)
            (EventRel.cartesian (get_output es1) (mini_loc es2))) ;
      intra_causality_control = EventRel.union
        es1.intra_causality_control es2.intra_causality_control ;
      control = EventRel.union es1.control es2.control;
      data_ports = EventSet.union es1.data_ports es2.data_ports;
      success_ports = EventSet.union es1.success_ports es2.success_ports;
      output = mkOut es1 es2;
      sca = EventSetSet.union es1.sca es2.sca;
      mem_accesses = EventSet.union es1.mem_accesses es2.mem_accesses; }

  let (=*$=) =
    check_disjoint (data_comp minimals_data (fun _ es -> es.output))

  let (=$$=) =
    check_disjoint (data_comp minimals_data (fun es _ -> Some (get_output es)))

  let comp_data_commit es1 es2 =
    check_disjoint (data_comp minimals_control (fun _ es -> es.output)) es1 es2

(* Composition with intra_causality_control from first to second *)
  let control_comp mini_loc es1 es2 =
    { procs = [] ;
      events =  EventSet.union es1.events es2.events;
      intra_causality_data = EventRel.union
        es1.intra_causality_data es2.intra_causality_data ;
      intra_causality_control = EventRel.union
        (EventRel.union es1.intra_causality_control
           es2.intra_causality_control)
        (EventRel.cartesian (maximals es1) (mini_loc es2));
      control = EventRel.union es1.control es2.control;
      data_ports = EventSet.union es1.data_ports es2.data_ports;
      success_ports = EventSet.union es1.success_ports es2.success_ports;
      output = union_output es1 es2;
      sca = EventSetSet.union es1.sca es2.sca;
      mem_accesses = EventSet.union es1.mem_accesses es2.mem_accesses; }

  let (=**=) = check_disjoint (control_comp minimals)

  let bind_ctrl_avoid aset es1 es2 =
    Some (control_comp (minimals_avoid aset) es1 es2)

(* Specific composition for checkint tags *)
      let check_tags a rtag commit =
        { procs= [];
          events = EventSet.union3 a.events rtag.events commit.events;
          intra_causality_data =
          EventRel.union3
            (EventRel.union3
               a.intra_causality_data rtag.intra_causality_data commit.intra_causality_data)
            (EventRel.cartesian (maximals a)
               (EventSet.union (minimals rtag) (minimals commit)))
            (EventRel.cartesian (maximals rtag) (minimals commit));
          intra_causality_control =
            EventRel.union3
            a.intra_causality_control rtag.intra_causality_control
            commit.intra_causality_control;
          control =
          EventRel.union3
            a.control rtag.control commit.control;
          data_ports =
          EventSet.union3
            a.data_ports rtag.data_ports commit.data_ports;
          success_ports =
          EventSet.union3
            a.success_ports rtag.success_ports commit.success_ports;
          output = Some (get_output commit);
          sca = EventSetSet.union3  a.sca rtag.sca commit.sca;
          mem_accesses =
          EventSet.union3  a.mem_accesses rtag.mem_accesses commit.mem_accesses;
        }
(* Multi composition for exchange *)

(* rsX/wsX are from/to the same location *)
  let exch_comp rs1 rs2 ws1 ws2 =
    { procs = [] ;
      events = EventSet.union4 rs1.events rs2.events ws1.events ws2.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.unions
           [rs1.intra_causality_data;rs2.intra_causality_data;
            ws1.intra_causality_data;ws2.intra_causality_data;];
         EventRel.cartesian (maximals rs1) (minimals ws2);
         EventRel.cartesian (maximals rs2) (minimals ws1);];
      intra_causality_control =
      EventRel.unions
        [EventRel.unions
           [rs1.intra_causality_control;rs2.intra_causality_control;
            ws1.intra_causality_control;ws2.intra_causality_control;];
         EventRel.cartesian (maximals rs1) (minimals ws1);
         EventRel.cartesian (maximals rs2) (minimals ws2);];
      control =
      EventRel.union4 rs1.control rs2.control ws1.control ws2.control;
      data_ports =
      EventSet.union4 rs1.data_ports rs2.data_ports ws1.data_ports ws2.data_ports;
      success_ports =
      EventSet.union4 rs1.success_ports rs2.success_ports ws1.success_ports ws2.success_ports;
      output = None;
      sca = EventSetSet.union4 rs1.sca rs2.sca ws1.sca ws2.sca;
      mem_accesses =
      EventSet.union4
        rs1.mem_accesses rs2.mem_accesses
        ws1.mem_accesses ws2.mem_accesses ; }

  let swp_comp rloc rmem rreg wmem wreg =
    { procs = [] ;
      events = EventSet.union5
        rloc.events rmem.events rreg.events wmem.events wreg.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union5
           rloc.intra_causality_data
           rmem.intra_causality_data rreg.intra_causality_data
           wmem.intra_causality_data wreg.intra_causality_data ;
         EventRel.cartesian (maximals rmem) (minimals wreg);
         EventRel.cartesian (maximals rreg) (minimals wmem);
         EventRel.cartesian (maximals rloc)
           (EventSet.union (minimals rmem) (minimals wmem))];
      intra_causality_control =
      EventRel.unions
        [EventRel.unions
           [rloc.intra_causality_control;
            rmem.intra_causality_control;rreg.intra_causality_control;
            wmem.intra_causality_control;wreg.intra_causality_control;];
         EventRel.cartesian (maximals rmem) (minimals wmem);
         EventRel.cartesian (maximals rreg) (minimals wreg);];
      control =
      EventRel.union5 rloc.control rmem.control rreg.control wmem.control wreg.control;
      data_ports =
      EventSet.union5
        rloc.data_ports
        rmem.data_ports rreg.data_ports wmem.data_ports wreg.data_ports;
      success_ports =
      EventSet.union5
        rloc.success_ports
        rmem.success_ports rreg.success_ports wmem.success_ports wreg.success_ports;
      output = None;
      sca = EventSetSet.union5 rloc.sca rmem.sca rreg.sca wmem.sca wreg.sca;
      mem_accesses =
      EventSet.union5
        rloc.mem_accesses
        rmem.mem_accesses rreg.mem_accesses
        wmem.mem_accesses wreg.mem_accesses ; }


(* disjointness is awful *)

  let exch rx ry wx wy =
    if
      EventSet.disjoint rx.events ry.events &&
      EventSet.disjoint rx.events wx.events &&
      EventSet.disjoint rx.events wy.events &&
      EventSet.disjoint ry.events wx.events &&
      EventSet.disjoint ry.events wy.events &&
      EventSet.disjoint wx.events wy.events
    then
      exch_comp rx ry wx wy
    else
      assert false

  let swp = swp_comp

  let linux_exch re rloc rmem wmem =
    let input_wmem = minimals wmem in
    let output_rloc = maximals rloc in
    { procs = [];
      events =
      EventSet.union4 re.events rloc.events rmem.events wmem.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union4
           re.intra_causality_data rloc.intra_causality_data
           rmem.intra_causality_data wmem.intra_causality_data;
         EventRel.cartesian (maximals re) input_wmem;
         EventRel.cartesian output_rloc input_wmem;
         EventRel.cartesian output_rloc (minimals rmem);];
      intra_causality_control =
      EventRel.union
        (EventRel.union4
           re.intra_causality_control rloc.intra_causality_control
           rmem.intra_causality_control wmem.intra_causality_control)
        (EventRel.cartesian (maximals rmem) (minimals wmem));
      control =
      EventRel.union4
        re.control rloc.control rmem.control wmem.control;
      data_ports =
      EventSet.union4
        re.data_ports rloc.data_ports rmem.data_ports wmem.data_ports;
      success_ports =
      EventSet.union4
        re.success_ports rloc.success_ports rmem.success_ports wmem.success_ports;
      output = Some (get_output rmem);
      sca = EventSetSet.union4 re.sca rloc.sca rmem.sca wmem.sca;
      mem_accesses =
      EventSet.union4
        re.mem_accesses rloc.mem_accesses rmem.mem_accesses wmem.mem_accesses;
    }

  let amo re rloc rmem wmem =
    let input_wmem = minimals wmem in
    let output_rloc = maximals rloc in
    { procs = [];
      events =
      EventSet.union4 re.events rloc.events rmem.events wmem.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union4
           re.intra_causality_data rloc.intra_causality_data
           rmem.intra_causality_data wmem.intra_causality_data;
         EventRel.cartesian (maximals rmem) input_wmem;
         EventRel.cartesian (maximals re) input_wmem;
         EventRel.cartesian output_rloc input_wmem;
         EventRel.cartesian output_rloc (minimals rmem);];
      intra_causality_control =
      (EventRel.union4
         re.intra_causality_control rloc.intra_causality_control
         rmem.intra_causality_control wmem.intra_causality_control);
      control =
      EventRel.union4
        re.control rloc.control rmem.control wmem.control;
      data_ports =
      EventSet.union4
        re.data_ports rloc.data_ports rmem.data_ports wmem.data_ports;
      success_ports =
      EventSet.union4
        re.success_ports rloc.success_ports rmem.success_ports wmem.success_ports;
      output = Some (get_output rmem);
      sca = EventSetSet.union4 re.sca rloc.sca rmem.sca wmem.sca;
      mem_accesses =
      EventSet.union4
        re.mem_accesses rloc.mem_accesses rmem.mem_accesses wmem.mem_accesses; }

(************************************)
(* Compare exchange, really complex *)
(************************************)

(* Success *)
  let linux_cmpexch_ok rloc rold rnew rmem wmem =
    let input_wmem = minimals wmem in
    let input_rmem = minimals rmem in
    let output_rloc = maximals rloc in
    { procs = [];
      events =
      EventSet.union5 rloc.events rold.events rnew.events
        rmem.events wmem.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union5
           rloc.intra_causality_data rold.intra_causality_data
           rnew.intra_causality_data
           rmem.intra_causality_data wmem.intra_causality_data;
         EventRel.cartesian (maximals rnew) input_wmem;
         EventRel.cartesian output_rloc input_wmem;
         EventRel.cartesian output_rloc input_rmem;];
      intra_causality_control =
      EventRel.unions
        [EventRel.union5
           rloc.intra_causality_control rold.intra_causality_control
           rnew.intra_causality_control
           rmem.intra_causality_control wmem.intra_causality_control;
         EventRel.cartesian (maximals rold) input_wmem;
         EventRel.cartesian (maximals rmem) input_wmem;];
      control=
      EventRel.union5 rloc.control rold.control rnew.control
        rmem.control wmem.control;
      data_ports=
      EventSet.union5 rloc.data_ports rold.data_ports rnew.data_ports
        rmem.data_ports wmem.data_ports;
      output=Some (get_output rmem);
      success_ports=
      EventSet.union5 rloc.success_ports rold.success_ports rnew.success_ports
        rmem.success_ports wmem.success_ports;
      sca = EventSetSet.union5 rloc.sca rold.sca rnew.sca rmem.sca wmem.sca;
      mem_accesses=
      EventSet.union5 rloc.mem_accesses rold.mem_accesses rnew.mem_accesses
        rmem.mem_accesses wmem.mem_accesses;
    }

(* Failure, a phantom write event that would iico_control depens
   upon rold may be an idea... Without it rold has no impact out evt_struct
   outcome... As another illustration of something lacking the cmpxchg introduces no
   new iico_control edge. *)

  let linux_cmpexch_no rloc rold rmem =
    let input_rmem = minimals rmem in
    let output_rloc = maximals rloc in
    { procs = [];
      events =
      EventSet.union3 rloc.events rold.events rmem.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union3
           rloc.intra_causality_data rold.intra_causality_data
           rmem.intra_causality_data;
         EventRel.cartesian output_rloc input_rmem;];
      intra_causality_control =
      EventRel.union3
        rloc.intra_causality_control rold.intra_causality_control
        rmem.intra_causality_control;
      control=
      EventRel.union3 rloc.control rold.control rmem.control;
      data_ports=
      EventSet.union3 rloc.data_ports rold.data_ports rmem.data_ports;
      success_ports=
      EventSet.union3 rloc.success_ports rold.success_ports rmem.success_ports;
      output=Some (get_output rmem);
      sca = EventSetSet.union3 rloc.sca rold.sca rmem.sca;
      mem_accesses=
      EventSet.union3 rloc.mem_accesses rold.mem_accesses rmem.mem_accesses;
    }

(**************)
(* Add unless *)
(**************)

  let linux_add_unless_ok loc a u rmem wmem retbool =
    let out_loc = maximals loc
    and in_rmem = minimals rmem
    and in_wmem = minimals wmem in
    { procs = [];
      events = EventSet.union5 loc.events a.events u.events rmem.events wmem.events;
      intra_causality_data =
      EventRel.unions
        [EventRel.union5
           loc.intra_causality_data a.intra_causality_data
           u.intra_causality_data
           rmem.intra_causality_data wmem.intra_causality_data;
         EventRel.cartesian out_loc in_wmem;
         EventRel.cartesian out_loc in_rmem;
         EventRel.cartesian (maximals a) in_wmem;
         EventRel.cartesian (maximals rmem) in_wmem;];
      intra_causality_control =
      EventRel.unions
        [EventRel.union5
           loc.intra_causality_control a.intra_causality_control
           u.intra_causality_control
           rmem.intra_causality_control wmem.intra_causality_control;
         EventRel.cartesian (maximals u) in_wmem;];
      control =
      EventRel.union5
        loc.control a.control u.control rmem.control wmem.control;
      data_ports =
      EventSet.union5
        loc.data_ports a.data_ports u.data_ports
        rmem.data_ports wmem.data_ports;
      success_ports =
      EventSet.union5
        loc.success_ports a.success_ports u.success_ports
        rmem.success_ports wmem.success_ports;
      output =
      Some
        (if retbool then EventSet.union (get_output rmem) (get_output u) else get_output rmem);
      sca = EventSetSet.union5 loc.sca a.sca u.sca rmem.sca wmem.sca;
      mem_accesses =
      EventSet.union5
        loc.mem_accesses a.mem_accesses u.mem_accesses
        rmem.mem_accesses wmem.mem_accesses;
    }

  let linux_add_unless_no loc u rmem retbool =
    let out_loc = maximals loc
    and in_rmem = minimals rmem in
    { procs = [];
      events =
      EventSet.union3 loc.events u.events rmem.events;
      intra_causality_data =
      EventRel.unions
        [loc.intra_causality_data; u.intra_causality_data;
         rmem.intra_causality_data;
         EventRel.cartesian out_loc in_rmem;];
      intra_causality_control =
      EventRel.union3
        loc.intra_causality_control u.intra_causality_control
        rmem.intra_causality_control;
      control =
      EventRel.union3 loc.control u.control rmem.control;
      data_ports =
      EventSet.union3 loc.data_ports u.data_ports rmem.data_ports;
      success_ports =
      EventSet.union3 loc.success_ports u.success_ports rmem.success_ports;
      output =
      Some
        (if retbool then EventSet.union (get_output rmem) (get_output u) else get_output rmem);
      sca = EventSetSet.union3 loc.sca u.sca rmem.sca;
      mem_accesses =
      EventSet.union3 loc.mem_accesses u.mem_accesses rmem.mem_accesses;
    }


(* RISCV Store conditional *)
  let riscv_sc success resa data addr wres wresult wmem =
    let dep_on_write =
      let d = Variant.get_default A.arch Variant.SwitchDepScWrite in
      if C.variant Variant.SwitchDepScWrite then not d else d in
    let in_wmem = minimals wmem
    and out_wmem = maximals wmem
    and in_wres = minimals wres
    and in_wresult = minimals wresult
    and out_data = maximals data
    and out_addr = maximals addr
    and out_resa = maximals resa in
    { procs = [];
      events =
      EventSet.union
        (EventSet.union3 resa.events data.events addr.events)
        (EventSet.union3 wres.events wresult.events wmem.events);
      intra_causality_data =
      EventRel.unions
        [EventRel.union3 resa.intra_causality_data
           data.intra_causality_data addr.intra_causality_data;
         EventRel.union3 wres.intra_causality_data
           wresult.intra_causality_data wmem.intra_causality_data;
         EventRel.cartesian
           (EventSet.union out_addr out_resa)
           (EventSet.union3 in_wmem in_wres
              (if C.variant Variant.FullScDepend || success
              then in_wresult else EventSet.empty));
         EventRel.cartesian out_data
           (if C.variant Variant.Success || not (C.variant Variant.FullScDepend) then in_wmem else
           EventSet.union in_wresult in_wmem); ];
      intra_causality_control =
      EventRel.union3
        (if dep_on_write then EventRel.cartesian out_wmem in_wresult
        else EventRel.empty)
        (EventRel.union3 resa.intra_causality_control
           data.intra_causality_control addr.intra_causality_control)
        (EventRel.union3 wres.intra_causality_control
           wresult.intra_causality_control wmem.control);
      control =
      EventRel.union
        (EventRel.union3 resa.control data.control addr.control)
        (EventRel.union3 wres.control wresult.control wmem.control);
      data_ports =
      EventSet.union
        (EventSet.union3 resa.data_ports data.data_ports addr.data_ports)
        (EventSet.union3 wres.data_ports wresult.data_ports wmem.data_ports);
      success_ports =
      EventSet.union
        (EventSet.union3 resa.success_ports data.success_ports addr.success_ports)
        (EventSet.union3 wres.success_ports wresult.success_ports wmem.success_ports);
      output = Some (EventSet.union (get_output wresult) (get_output wres));
      sca =
      EventSetSet.union
        (EventSetSet.union3 resa.sca data.sca addr.sca)
        (EventSetSet.union3 wres.sca wresult.sca wmem.sca);
      mem_accesses =
      EventSet.union
        (EventSet.union3 resa.mem_accesses data.mem_accesses addr.mem_accesses)
        (EventSet.union3 wres.mem_accesses wresult.mem_accesses wmem.mem_accesses);
    }

(* AArch64 CAS, success *)
  let aarch64_cas_ok rn rs rt wrs rm wm =
    let output_rn = maximals rn
    and output_rs = maximals rs
    and output_rm = maximals rm
    and input_wrs = minimals wrs
    and input_rm = minimals rm
    and input_wm = minimals wm in
    { procs = [] ;
      events =
      EventSet.union6
        rn.events rs.events rt.events
        wrs.events rm.events wm.events ;
      intra_causality_data =
      EventRel.union
        (EventRel.union6
           rn.intra_causality_data
           rs.intra_causality_data
           rt.intra_causality_data
           wrs.intra_causality_data
           rm.intra_causality_data
           wm.intra_causality_data)
        (EventRel.union4
           (EventRel.cartesian output_rn input_rm)
           (EventRel.cartesian output_rn input_wm)
           (EventRel.cartesian (maximals rt) input_wm)
           (EventRel.cartesian output_rm input_wrs));
      intra_causality_control =
      EventRel.union
        (EventRel.union6
           rn.intra_causality_control
           rs.intra_causality_control
           rt.intra_causality_control
           wrs.intra_causality_control
           rm.intra_causality_control
           wm.intra_causality_control)
        (EventRel.union
           (EventRel.cartesian output_rs input_wm)
           (EventRel.cartesian output_rm input_wm));
      control =
       (EventRel.union6
         rn.control rs.control rt.control
         wrs.control rm.control wm.control);
      data_ports =
      (EventSet.union6
          rn.data_ports rs.data_ports rt.data_ports
          wrs.data_ports rm.data_ports wm.data_ports);
      success_ports =
       (EventSet.union6
          rn.success_ports rs.success_ports rt.success_ports
          wrs.success_ports rm.success_ports wm.success_ports);
      sca =
      (EventSetSet.union6
         rn.sca rs.sca rt.sca
         wrs.sca rm.sca wm.sca);
      mem_accesses =
       (EventSet.union6
         rn.mem_accesses rs.mem_accesses rt.mem_accesses
         wrs.mem_accesses rm.mem_accesses wm.mem_accesses);
      output = Some (maximals wrs);
    }
(* Store update composition, read data, read EA, write EA and  write Mem *)

(* Dijointness not checked..., useless *)
let stu rD rEA wEA wM =
  assert
    (EventRel.is_empty rD.intra_causality_control &&
    EventRel.is_empty rEA.intra_causality_control &&
    EventRel.is_empty wEA.intra_causality_control &&
    EventRel.is_empty wM.intra_causality_control) ;
  { procs = [] ;
    events = EventSet.union4 rD.events rEA.events wEA.events wM.events;
    intra_causality_data = begin
      let drD = rD.intra_causality_data
      and drEA = rEA.intra_causality_data
      and dwEA = wEA.intra_causality_data
      and dwM = wM.intra_causality_data in
      EventRel.unions
        [EventRel.unions [drD; drEA; dwEA; dwM;];
         EventRel.cartesian (maximals rEA) (minimals wEA);
         EventRel.cartesian
           (EventSet.union (maximals rEA) (maximals rD)) (minimals wM);]
    end ;
    intra_causality_control = EventRel.empty;
    control =
      EventRel.union4 rD.control rEA.control wEA.control wM.control ;
    data_ports =
      EventSet.union4
        rD.data_ports rEA.data_ports wEA.data_ports wM.data_ports ;
    success_ports =
      EventSet.union4
        rD.success_ports rEA.success_ports wEA.success_ports wM.success_ports ;
    output = None;
    sca = EventSetSet.union4 rD.sca rEA.sca wEA.sca wM.sca;
    mem_accesses =
      EventSet.union4
        rD.mem_accesses rEA.mem_accesses wEA.mem_accesses wM.mem_accesses ;
  }

(*************************************************************)
(* Add together event structures from different instructions *)
(*************************************************************)

    let different_ins i1 i2 =  match i1,i2 with
    | Some i1,Some i2 -> A.inst_instance_compare i1 i2 <> 0
    | None,Some _
    | Some _,None
    | None,None -> true

    let disjoint_iiis es1 es2 =
      EventSet.for_all
        (fun e1 ->
          EventSet.for_all
            (fun e2 -> different_ins e1.iiid e2.iiid)
            es2.events)
        es1.events

    let check_both do_it es1 es2 =
      if
        not
          (EventSet.disjoint es1.events es2.events &&
           disjoint_iiis es1 es2)
      then assert false
      else Some (do_it es1 es2)

(* Parallel composition *)
    let (+|+) = check_both para_comp


  let empty_event_structure = empty

(* Instruction control *)

  let cond_comp es1 es2 =
    let r = para_comp es1 es2 in
    let control = EventRel.cartesian es1.events es2.events in
    { r with control =  EventRel.union control r.control; }
end
