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

(** Define CPP11 actions *)

open Printf

module Make (A : Arch.S) : sig

  type action =    
    | Access of Dir.dirn * A.location * A.V.v * CPP11Base.mem_order
    | Fence of CPP11Base.mem_order
(* LM: ??? RMW (location, read, written, mo) *)
    | RMW of A.location * A.V.v * A.V.v * CPP11Base.mem_order
    | Blocked_RMW of A.location
    | Lock of A.location * bool (* true = success, false = blocked *)
    | Unlock of A.location

  include Action.S with type action := action and module A = A

end = struct
  module A = A
  module V = A.V
  open Dir

  type action = 
    | Access of dirn * A.location * V.v * CPP11Base.mem_order
    | Fence of CPP11Base.mem_order
    | RMW of A.location * V.v * V.v * CPP11Base.mem_order
    | Blocked_RMW of A.location
    | Lock of A.location * bool (* true = success, false = blocked *)
    | Unlock of A.location

 
  let mk_init_write l v = Access (W,l,v,CPP11Base.NA)

  let pp_action a = match a with
    | Access (d,l,v,CPP11Base.NA) ->
	sprintf "%s%s=%s"
          (pp_dirn d)
          (A.pp_location l)
	  (V.pp_v v)
    | Access (d,l,v,mo) ->
	sprintf "%s(%s)%s=%s"
          (pp_dirn d)
          (CPP11Base.pp_mem_order_short mo)
          (A.pp_location l)
	  (V.pp_v v)
    | Fence mo -> 
       sprintf "F(%s)"
	  (CPP11Base.pp_mem_order_short mo)
    | RMW (l,v1,v2,mo) ->
       	sprintf "RMW(%s)%s(%s>%s)"
          (CPP11Base.pp_mem_order_short mo)
          (A.pp_location l)
	  (V.pp_v v1) (V.pp_v v2)
    | Blocked_RMW l ->
       sprintf "BRMW%s"
	  (A.pp_location l)
    | Lock (l,o) ->
      sprintf "L%s%s"
	(if o then "S" else "B")
        (A.pp_location l)
    | Unlock l ->
      sprintf "U%s"
        (A.pp_location l)

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_) -> Some v
    | _ -> None

    let read_of a = match a with
    | Access (R,_ , v,_) 
    | RMW (_,v,_,_)
        -> Some v
    | _ -> None

    let written_of a = match a with
    | Access (W,_ , v,_) 
    | RMW (_,_,v,_)
        -> Some v
    | _ -> None

    let location_of a = match a with
    | Access (_, l, _,_) 
    | Lock (l,_)
    | Unlock l
    | RMW (l,_,_,_)
    | Blocked_RMW l -> Some l
    | _ -> None

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_)
    | RMW (A.Location_global _,_,_,_)
      -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_)
    | RMW (A.Location_global _,_,_,_)
      -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_) -> true
    | RMW _ | Fence _ | Blocked_RMW _ -> true
    | _ -> false

    (* The following definition of is_atomic
       is quite arbitrary. *)
    let is_atomic a = match a with
    | Access (_,A.Location_global _,_,mo) -> mo != CPP11Base.NA
    | RMW _ -> true
    | _ -> false

    let get_mem_dir a = match a with
    | Access (d,A.Location_global _,_,_) -> d
    | _ -> assert false

(* relative to the registers of the given proc *)
    let is_reg_store a (p:int) = match a with
    | Access (W,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false

    let is_reg_load a (p:int) = match a with
    | Access (R,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false

    let is_reg a (p:int) = match a with
    | Access (_,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false


(* Store/Load anywhere *)
    let is_store a = match a with
    | Access (W,_,_,_)
    | RMW _
      -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_)
    | RMW _ -> true
    | _ -> false

    let is_reg_any a = match a with
    | Access (_,A.Location_reg _,_,_) -> true
    | _ -> false

    let is_reg_store_any a = match a with
    | Access (W,A.Location_reg _,_,_) -> true
    | _ -> false

    let is_reg_load_any a = match a with
    | Access (R,A.Location_reg _,_,_) -> true
    | _ -> false

(* Barriers *)
    let is_barrier _ = false
    let barrier_of _ = None
    let same_barrier_id _ _ = false

(* Commits *)
   let is_commit _ = false

(* Local/Global Fences *)
   let is_local_fence _ = false
   let is_global_fence _ = false

(* Fences *)
   let is_fence a = match a with
     | Fence _ -> true
     | _ -> false

(* RMWs *)
   let is_rmw a = match a with
     | RMW _ -> true
     | _ -> false

(* Blocked RMWs *)
   let is_blocked_rmw a = match a with
     | Blocked_RMW _ -> true
     | _ -> false

(* Mutex operations *)
   let is_lock a = match a with
     | Lock _ -> true
     | _ -> false

   let is_successful_lock a = match a with
     | Lock (_,true) -> true
     | _ -> false

   let is_unlock a = match a with
     | Unlock _ -> true
     | _ -> false

   let is_mutex_action a = match a with
     | Lock _ | Unlock _ -> true
     | _ -> false

   let mo_matches target a = match a with
     | Access(_,_,_,mo)
     | RMW (_,_,_,mo) 
     | Fence mo -> mo=target
     | _ -> false

   let is_sc_action a = mo_matches CPP11Base.SC a

(* Architecture-specific sets *)
   let arch_sets = [
     "rmw", is_rmw; "brmw", is_blocked_rmw;
     "lk", is_lock; "ls", is_successful_lock;
     "ul", is_unlock; "F", is_fence;
     "acq", mo_matches CPP11Base.Acq;
     "sc", mo_matches CPP11Base.SC;
     "rel", mo_matches CPP11Base.Rel; 
     "acq_rel", mo_matches CPP11Base.Acq_Rel;
     "rlx", mo_matches CPP11Base.Rlx;
     "con", mo_matches CPP11Base.Con;
     "na", mo_matches CPP11Base.NA;
   ]

  let arch_fences = []

  let is_isync _ = assert false
  let pp_isync = "???"

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | RMW(l,v1,v2,_) ->
         let undet_loc = match A.undetermined_vars_in_loc l with
	   | None -> V.ValueSet.empty
	   | Some v -> V.ValueSet.singleton v in
         let undet_loc = 
	   (if V.is_var_determined v1 then undet_loc 
	    else V.ValueSet.add v1 undet_loc) in
         let undet_loc =
           (if V.is_var_determined v2 then undet_loc
	    else V.ValueSet.add v2 undet_loc) in
         undet_loc
      | Blocked_RMW l
      | Lock(l,_) 
      | Unlock l -> 
	 (match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v) 
      | Fence _ -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,mo) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',mo)
      | RMW(l,v1,v2,mo) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
	let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo)
      | Blocked_RMW l ->
	 let l' = A.simplify_vars_in_loc soln l in
        Blocked_RMW l'
      | Lock(l,o) ->
        let l' = A.simplify_vars_in_loc soln l in
        Lock(l',o)
      | Unlock l  ->
        let l' = A.simplify_vars_in_loc soln l in
        Unlock l'
      | Fence _ -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic _ = assert false

end

