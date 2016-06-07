(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf

module Make (A : Arch.S) : sig

  type action =    
    | Access of Dir.dirn * A.location * A.V.v * MemOrderOrAnnot.t
    | Fence of MemOrderOrAnnot.t
(* LM: ??? RMW (location, read, written, mo) *)
    | RMW of A.location * A.V.v * A.V.v * MemOrder.t
    | Lock of A.location * bool (* true = success, false = blocked *)
    | Unlock of A.location

  include Action.S with type action := action and module A = A

end = struct
  module A = A
  module V = A.V
  open Dir
  open MemOrderOrAnnot

  type action = 
    | Access of dirn * A.location * V.v * MemOrderOrAnnot.t
    | Fence of MemOrderOrAnnot.t
    | RMW of A.location * V.v * V.v * MemOrder.t
    | Lock of A.location * bool (* true = success, false = blocked *)
    | Unlock of A.location


  let mk_init_write l v = Access (W,l,v,AN [])

  let par f x = sprintf "(%s)" (f x)
  let bra f x = sprintf "[%s]" (f x)

  let pp_action a = match a with
    | Access (d,l,v,mo) ->
	sprintf "%s%s%s=%s"
          (pp_dirn d)
          (match mo with
          | MO mo -> par MemOrder.pp_mem_order_short mo
          | AN [] -> ""
          | AN a -> bra pp_annot a)
          (A.pp_location l)
	  (V.pp_v v)
    | Fence mo -> 
       sprintf "F%s"
	  (match mo with
          | MO mo -> par MemOrder.pp_mem_order_short mo
          | AN a ->  bra pp_annot a)
    | RMW (l,v1,v2,mo) ->
       	sprintf "RMW(%s)%s(%s>%s)"
          (MemOrder.pp_mem_order_short mo)
          (A.pp_location l)
	  (V.pp_v v1) (V.pp_v v2)
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
    | RMW (l,_,_,_) -> Some l
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
    | RMW _ -> true
    | _ -> false

    (* The following definition of is_atomic
       is quite arbitrary. *)

    let old_is_atomic a = match a with
    | Access (_,A.Location_global _,_,AN _) -> false
    | Access (_,A.Location_global _,_,MO _) -> true
    | RMW _ -> true
    | _ -> false

(* LM: Indeed: *)
    let is_atomic _ = false

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
  let is_barrier = function
    | Fence _ -> true
    | _ -> false

  let barrier_of _ = assert false

  let same_barrier_id _ _ = assert false

(* (No) commits *)
   let is_commit_bcc _ = false
   let is_commit_pred _ = false

(* RMWs *)
   let is_rmw a = match a with
     | RMW _ -> true
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

   let mo_matches target a = match a with
     | Access(_,_,_,MO mo)
     | RMW (_,_,_,mo)
     | Fence (MO mo) -> mo=target
     | _ -> false

(* Architecture-specific sets *)
  let is_fence = function
    | Fence _ -> true
    | _ -> false

   let arch_sets = [
     "RMW", is_rmw;
     "LK", is_lock; "LS", is_successful_lock;
     "UL", is_unlock;
     "ACQ", mo_matches MemOrder.Acq;
     "SC", mo_matches MemOrder.SC;
     "REL", mo_matches MemOrder.Rel; 
     "ACQ_REL", mo_matches MemOrder.Acq_Rel;
     "RLX", mo_matches MemOrder.Rlx;
     "CON", mo_matches MemOrder.Con;
     "A",old_is_atomic;
     "NA",(fun a -> not (old_is_atomic a));
     "Start", is_fence;
   ]

  let arch_fences = []

  let is_isync _ = raise Misc.NoIsync
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

    let annot_in_list str ac = match ac with
    | Access (_,_,_,AN a)
    | Fence (AN a) -> List.exists (fun a -> Misc.string_eq str a) a 
    | Access (_, _, _, MO _)|Fence (MO _)|RMW (_, _, _, _)
    | Lock (_, _)|Unlock _ -> false
end

