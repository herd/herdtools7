(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Operations on events *)

open Printf

module type S = sig

  module A : Arch.S

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
  val progorder_of  : event -> A.program_order_index option

(* Is e1 before e2 w.r.t. prog order ? Nothing assumed on e1 and a2 *)
  val po_strict : event -> event -> bool
  val before_in_po : event -> event -> bool

(************************)
(* Predicates on events *)
(************************)

(* relative to memory *)
  val is_mem_store : event -> bool
  val is_mem_store_init : event -> bool
  val is_mem_load : event ->  bool
  val is_mem : event -> bool
  val is_atomic : event -> bool
  val get_mem_dir : event -> Dir.dirn

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

(* Barriers *)
  val is_barrier : event -> bool
  val barrier_of : event -> A.barrier option
  val same_barrier_id : event -> event -> bool
  val is_isync : event -> bool

(* Commit *)
  val is_commit : event -> bool

(* Local/Global Fences *)
  val is_local_fence : event -> bool
  val is_global_fence : event -> bool

(* Mutex operations *)
  val is_mutex_action : event -> bool

(* SC operations *)
  val is_sc_action : event -> bool

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

  module Atomicity : MySet.S with type elt = EventSet.t

  val pp_atomicity : out_channel -> Atomicity.t -> unit

  type event_structure = {
      procs : A.proc list ;
      events : EventSet.t ;                         (* really a set *)
      intra_causality_data : EventRel.t ;              (* really a partial order relation *)
      intra_causality_control : EventRel.t ;              (* really a partial order relation *)	  
      atomicity : Atomicity.t;                 (* really a PER *)
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
(* Value is ==, used for data dependencies *)
  val same_subst_value : event -> event -> bool
  val is_visible_location : A.location -> bool


(************************)
(* Parallel composition *)
(************************)
  val (=|=) :
      event_structure -> event_structure -> event_structure option      

(* sequential composition, add data dependency *)
  val (=*$=) :
      event_structure -> event_structure -> event_structure option      

(* sequential composition, add control dependency *)
  val (=**=) :
      event_structure -> event_structure -> event_structure option      

(* exchange composition :
   xch rx ry wx wy ->
      rx -data-> wy, ry -data-> wx
      rx -ctrl-> wx, ry -ctrl-> wy
*)
  val exch :
     event_structure -> event_structure ->
     event_structure -> event_structure ->
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
  val lock_all_events         : event_structure -> event_structure

end
      
module Make (AI:Arch.S) (Act:Action.S with module A = AI) : 
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
      | A.Location_global _ -> true
      | A.Location_reg _ -> false

    let same_location e1 e2 = 
      if (location_of e1 = None || location_of e2 = None) then
	false
      else
        location_compare e1 e2 = 0

    let same_value e1 e2 = match value_of e1, value_of e2 with
    | Some v1,Some v2 -> V.compare v1 v2 = 0
    | _,_ -> assert false

(* Used for detecting data dependencies,
   e1 is a reg load and e2 is a mem store, from the same instruction.
   As a consequence : values exists and are determined.
   Physical equality of values implies that e1 is the data port.
   Converse should hold in the current implementation... *)

    let same_subst_value e1 e2 =
(*      eprintf "SUBST: %a vs. %a:%!" debug_event e1 debug_event e2 ; *)
      let r = match read_of e1, written_of e2 with
      | Some (v1),Some (v2) -> v1 == v2
      | _,_ -> assert false in
(*      eprintf "%b\n" r  ; *)
      r


    let proc_of e = match e.iiid with
    | Some i -> Some i.A.proc
    | None -> None

    let same_proc e1 e2 = match proc_of e1, proc_of e2 with
    | Some p1,Some p2 -> Misc.int_eq p1 p2
    | (None,Some _)|(Some _,None) -> false
    | None,None -> true

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


(* relative to memory *)
    let is_mem_store e = Act.is_mem_store e.action 

    let is_mem_store_init e = match e.iiid with
    | None -> true
    | Some _ -> false

    let is_mem_load e = Act.is_mem_load e.action 
    let is_mem e = Act.is_mem e.action 
    let is_atomic e = Act.is_atomic e.action 
    let get_mem_dir e = Act.get_mem_dir e.action 

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

(* Barriers *)
    let is_barrier e = Act.is_barrier e.action 
    let barrier_of e = Act.barrier_of e.action
    let same_barrier_id e1 e2 = 
      Act.same_barrier_id e1.action e2.action
  let is_isync e = Act.is_isync e.action

(* Commits *)
   let is_commit e = Act.is_commit e.action

(* Local/Global Fences *)
   let is_local_fence e = Act.is_local_fence e.action
   let is_global_fence e = Act.is_global_fence e.action

(* Mutex operations *)
   let is_mutex_action e = Act.is_mutex_action e.action

(* SC operations *)
   (* For grabbing all the SC operations, so a total order
   through them can be found. *)
   let is_sc_action e = Act.is_sc_action e.action

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

    module Atomicity =
      MySet.Make
	(struct
	  type t = EventSet.t
	  let compare = EventSet.compare
	end)


    let pp_atomicity chan a =
      fprintf chan "{" ;
      Atomicity.pp chan "; "
	(fun chan es ->
	  fprintf chan "{" ;
	  EventSet.pp chan "; "
	    (fun chan e -> fprintf chan "eeid_%i" e.eiid)
	    es ;
	  fprintf chan "}")
	a ;
      fprintf chan "}"

(* Equivalence closure of atomicity classes *)
    let unions xss xs =  Atomicity.fold EventSet.union xss xs

    let equiv_closure xss yss =
(*
  eprintf "CLOSURE: %a %a -> " pp_atomicity xss pp_atomicity yss ;
 *)
      let r =
	Atomicity.fold
	  (fun xs acc ->
	    let non_inter,inter =
	      Atomicity.partition (EventSet.disjoint xs) acc in
	    Atomicity.add (unions inter xs) non_inter)
	  xss yss in
(*
  eprintf "%a\n" pp_atomicity r ;
 *)
      r
	
	

    type event_structure = {
	procs : A.proc list ; (* will prove convenient *)
	events : EventSet.t ;        (* really a set *)
	intra_causality_data : EventRel.t ;   (* really a (partial order) relation *)
	intra_causality_control : EventRel.t ;(* really a (partial order) relation *)
	atomicity : Atomicity.t ;                 (* a set of sets *)
      } 
	  
    let procs_of es = es.procs
	
    let locs_of es = EventSet.fold (fun e k -> match location_of e with Some l -> l::k | None -> k) es.events []

    let map_event_structure f es =
      let map_rel = EventRel.map (fun (e1,e2) -> f e1,f e2)
      and map_set = EventSet.map f in
      {
       procs = es.procs ;
       events = map_set es.events ;
       intra_causality_data = map_rel  es.intra_causality_data ;
       intra_causality_control = map_rel es.intra_causality_control ;
       atomicity = Atomicity.map map_set es.atomicity ;
     }

    let empty =
      { procs = [] ; events = EventSet.empty ;
	intra_causality_data = EventRel.empty ;
	intra_causality_control = EventRel.empty ;
	atomicity = Atomicity.empty ; }

      
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

let maximals es =
  let intra_causality =
    EventRel.union es.intra_causality_data es.intra_causality_control in 
  EventSet.filter 
    (fun e ->
      EventRel.for_all
	(fun (e1,_) -> e.eiid <> e1.eiid) intra_causality)
    es.events
(**********************************)      
(* Add together event structures  *)
(**********************************)

(* Checking events sets are disjoint *)

let check_disjoint do_it es1 es2 =
  if not (EventSet.disjoint es1.events es2.events) then None
  else Some (do_it es1 es2)
      
(* Parallel composition *)
let para_comp es1 es2 =
  { empty with
    events = EventSet.union es1.events es2.events;
    intra_causality_data = EventRel.union
      es1.intra_causality_data es2.intra_causality_data ;
    intra_causality_control = EventRel.union
      es1.intra_causality_control  es2.intra_causality_control ; 
    atomicity = equiv_closure
      es1.atomicity es2.atomicity; }

let (=|=) = check_disjoint para_comp
    
(* Composition with intra_causality_data from first to second *)
    let data_comp es1 es2 =
      { empty with events = EventSet.union es1.events es2.events;
	intra_causality_data = EventRel.union
	  (EventRel.union es1.intra_causality_data
	     es2.intra_causality_data)
	  (EventRel.cartesian (maximals es1) (minimals es2)) ;
        intra_causality_control = EventRel.union
	  es1.intra_causality_control es2.intra_causality_control ;
	atomicity = equiv_closure es1.atomicity es2.atomicity }

    let (=*$=) = check_disjoint data_comp

(* Composition with intra_causality_control from first to second *)
    let control_comp es1 es2 =
      { empty with events =  EventSet.union es1.events es2.events;
        intra_causality_data = EventRel.union
	  es1.intra_causality_data es2.intra_causality_data ;
	intra_causality_control = EventRel.union
	  (EventRel.union es1.intra_causality_control
	     es2.intra_causality_control)
	  (EventRel.cartesian (maximals es1) (minimals es2));
	atomicity = equiv_closure es1.atomicity es2.atomicity; }

    let (=**=) = check_disjoint control_comp

(* Multi composition for exchange *)
(* rsX/wsX are from/to the same location *)    
    let exch_comp rs1 rs2 ws1 ws2 =
      { empty with
        events = EventSet.unions [rs1.events;rs2.events;ws1.events;ws2.events;];
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
	atomicity =
        equiv_closure
          (equiv_closure rs1.atomicity rs2.atomicity)
          (equiv_closure ws1.atomicity ws2.atomicity);}

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

(* Store update composition, read data, read EA, write EA and  write Mem *)

(* Dijointness not checked..., useless *)
let stu rD rEA wEA wM =
  assert
    (EventRel.is_empty rD.intra_causality_control &&
    EventRel.is_empty rEA.intra_causality_control &&
    EventRel.is_empty wEA.intra_causality_control &&
    EventRel.is_empty wM.intra_causality_control) ;
  assert
    (Atomicity.is_empty rD.atomicity &&
     Atomicity.is_empty rEA.atomicity &&
     Atomicity.is_empty wEA.atomicity &&
     Atomicity.is_empty wM.atomicity) ;
  { empty with
    events = EventSet.unions [rD.events;rEA.events;wEA.events;wM.events;];
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
	  (EventSet.disjoint es1.events es2.events  &&
	   disjoint_iiis es1 es2)
      then assert false
      else Some (do_it es1 es2)

(* Parallel composition *)
    let (+|+) = check_both para_comp

    let empty_event_structure = empty

    let make_load_store_atomic es =
      let module M = Map.Make(OrderedEvent) in
      let env =
        EventSet.fold
	  (fun e k ->
	   if is_mem e then 
	     M.add e { e with action = Act.make_action_atomic e.action } k
	   else k)
          es.events M.empty in
      map_event_structure
        (fun e -> try M.find e env with Not_found -> e)
        es

    let lock_all_events es =
      if EventSet.is_empty es.events then es
      else
        let es = make_load_store_atomic es in
	{ es with atomicity = Atomicity.singleton es.events} 


  end

