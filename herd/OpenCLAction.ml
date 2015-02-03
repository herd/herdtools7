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

(** Define OpenCL actions *)

open Printf

module Make (A : Arch.S) : sig

  type fence_type = 
    | Normal_fence
    | Entry_fence of string
    | Exit_fence of string

 type action =    
    | Access of Dir.dirn * A.location * A.V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope * bool (* is_failed_rmw *)
    | Fence of OpenCLBase.gpu_memory_space list * OpenCLBase.mem_order * OpenCLBase.mem_scope * fence_type
    | RMW of A.location * A.V.v * A.V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Blocked_RMW of A.location

  include Action.S with module A = A and type action := action
end = struct
  module A = A
  module V = A.V
  open Dir

  type fence_type = 
    | Normal_fence
    | Entry_fence of string
    | Exit_fence of string

  type action = 
    | Access of dirn * A.location * V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope * bool (* is_failed_rmw *)
    | Fence of OpenCLBase.gpu_memory_space list * OpenCLBase.mem_order * OpenCLBase.mem_scope * fence_type
    | RMW of A.location * V.v * V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Blocked_RMW of A.location
 
  let mk_init_write l v = Access (W,l,v,OpenCLBase.NA,OpenCLBase.S_workitem,false)

    let pp_fence_type = function
      | Normal_fence -> ""
      | Exit_fence lbl -> ", exit " ^ lbl
      | Entry_fence lbl -> ", entry " ^ lbl

  let pp_action  a = match a with
    | Access (d,l,v,mo,s,b) ->
	sprintf "%s%s(%s,%s)%s=%s"
          (pp_dirn d)
          (if b then "f" else "") (* failed rmw *)
          (OpenCLBase.pp_mem_order_short mo)
          (OpenCLBase.pp_mem_scope s)
          (A.pp_location l)
	  (V.pp_v v)
    | Fence (mr,mo,s,ft) -> 
       sprintf "%sF(%s,%s%s)"
         (OpenCLBase.pp_gpu_memory_spaces_short mr)
         (OpenCLBase.pp_mem_order_short mo)
         (OpenCLBase.pp_mem_scope s)
         (pp_fence_type ft)
    | RMW (l,v1,v2,mo,s) ->
       	sprintf "RMW(%s,%s)%s(%s>%s)"
          (OpenCLBase.pp_mem_order_short mo)
          (OpenCLBase.pp_mem_scope s)
          (A.pp_location l)
	  (V.pp_v v1) (V.pp_v v2)
    | Blocked_RMW l ->
       sprintf "BRMW%s"
	  (A.pp_location l)

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_,_,_) -> Some v
    | _ -> None

    let read_of a = match a with
    | Access (R,_ , v,_,_,_)
    | RMW (_,v,_,_,_)
      -> Some v
    | _ -> None

    let written_of = function
    | Access (W,_ , v,_,_,_)
    | RMW (_,_,v,_,_)
      -> Some v
    | _ -> None


    let location_of a = match a with
    | Access (_, l, _,_,_,_) 
    | RMW (l,_,_,_,_)
    | Blocked_RMW l -> Some l
    | _ -> None

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_,_,_)
    | RMW (A.Location_global _,_,_,_,_)
        -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_,_,_)
    | RMW (A.Location_global _,_,_,_,_)
        -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_,_,_) -> true
    | RMW _ | Fence _ | Blocked_RMW _ -> true
    | _ -> false

    (* The following definition of is_atomic
       is quite arbitrary. *)
    let is_atomic a = match a with
    | Access (_,A.Location_global _,_,mo,_,_) -> mo != OpenCLBase.NA
    | RMW _ -> true
    | _ -> false

    let get_mem_dir a = match a with
    | Access (d,A.Location_global _,_,_,_,_) -> d
    | _ -> assert false

(* relative to the registers of the given proc *)
    let is_reg_store a (p:int) = match a with
    | Access (W,A.Location_reg (q,_),_,_,_,_) -> p = q
    | _ -> false

    let is_reg_load a (p:int) = match a with
    | Access (R,A.Location_reg (q,_),_,_,_,_) -> p = q
    | _ -> false

    let is_reg a (p:int) = match a with
    | Access (_,A.Location_reg (q,_),_,_,_,_) -> p = q
    | _ -> false


(* Store/Load anywhere *)
    let is_store a = match a with
    | Access (W,_,_,_,_,_)
    | RMW _
      -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_,_,_)
    | RMW _
         -> true
    | _ -> false

    let is_reg_any a = match a with
    | Access (_,A.Location_reg _,_,_,_,_) -> true
    | _ -> false

    let is_reg_store_any a = match a with
    | Access (W,A.Location_reg _,_,_,_,_) -> true
    | _ -> false

    let is_reg_load_any a = match a with
    | Access (R,A.Location_reg _,_,_,_,_) -> true
    | _ -> false

(* Barriers *)
    let is_barrier _ = false
    let barrier_of _ = None

    let same_barrier_id a1 a2 = 
      let lbl_of = function
        | Normal_fence -> None
        | Entry_fence lbl -> Some lbl
        | Exit_fence lbl -> Some lbl
      in
      match a1, a2 with
      | Fence (_,_,_,ft1), Fence (_,_,_,ft2) ->
        (ft1 != Normal_fence) && (lbl_of ft1 = lbl_of ft2)
      | _ -> false

(* Commits *)
   let is_commit _ = false

(* Fences *)
   let is_fence a = match a with
     | Fence _ -> true
     | _ -> false

   let is_global_fence a = match a with
     | Fence (rs,_,_,_) -> List.mem OpenCLBase.GlobalMem rs
     | _ -> false

   let is_local_fence a = match a with
     | Fence (rs,_,_,_) -> List.mem OpenCLBase.LocalMem rs
     | _ -> false

   let is_entry_fence a = match a with
     | Fence (_,_,_,Entry_fence _) -> true
     | _ -> false
   
   let is_exit_fence a = match a with
     | Fence (_,_,_,Exit_fence _) -> true
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
   let is_mutex_action _a = false

   let mo_matches target a = match a with
     | Access(_,_,_,mo,_,_)
     | RMW (_,_,_,mo,_) 
     | Fence (_,mo,_,_) -> mo=target
     | _ -> false

   let is_sc_action a = mo_matches OpenCLBase.SC a

   let scope_matches target a = match a with
     | Access(_,_,_,_,s,_)
     | RMW (_,_,_,_,s) 
     | Fence (_,_,s,_) -> s=target
     | _ -> false
       
   let is_failed_rmw a = match a with
     | Access(_,_,_,_,_,b) -> b
     | _ -> false

(* Architecture-specific sets *)
   let arch_sets = [
     "rmw", is_rmw; 
     "brmw", is_blocked_rmw;
     "F", is_fence;
(*   "gF", is_global_fence;
     "lF", is_local_fence; *)
     "exit_fence", is_exit_fence;
     "entry_fence", is_entry_fence;
     "failed_rmw", is_failed_rmw;
   ] @ List.map (fun (k,v) -> (k,mo_matches v)) [
     "memory_order_acquire", OpenCLBase.Acq;
     "memory_order_seq_cst", OpenCLBase.SC;
     "memory_order_release", OpenCLBase.Rel; 
     "memory_order_acq_rel", OpenCLBase.Acq_Rel;
     "memory_order_relaxed", OpenCLBase.Rlx;
     "na", OpenCLBase.NA;
   ] @ List.map (fun (k,v) -> (k,scope_matches v)) [
     "memory_scope_work_item", OpenCLBase.S_workitem;
     "memory_scope_sub_group", OpenCLBase.S_subgroup;
     "memory_scope_work_group", OpenCLBase.S_workgroup;
     "memory_scope_device", OpenCLBase.S_device;
     "memory_scope_all_svm_devices", OpenCLBase.S_all_svm_devices;
   ]

  let arch_fences = []

  let is_isync _ = assert false
  let pp_isync = "???"

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_,_,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | RMW(l,v1,v2,_,_) ->
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
      | Blocked_RMW l -> 
	 (match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v) 
      | Fence _ -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,mo,s,b) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',mo,s,b)
      | RMW(l,v1,v2,mo,s) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
	let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo,s)
      | Blocked_RMW l ->
	 let l' = A.simplify_vars_in_loc soln l in
        Blocked_RMW l'
      | Fence _ -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic _ = assert false

end

