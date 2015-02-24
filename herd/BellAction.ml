(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Implementation of the action interface for Bell *)

module Make (A : Arch.S) : sig

  type action =    
    | Access of
        Dir.dirn * A.location * A.V.v *
          bool * string list 
    | Barrier of string list
    | Commit

  include Action.S with module A = A and type action := action

end = struct

  module A = A
  module V = A.V
  open Dir

  type action = 
    | Access of
        dirn * A.location * V.v *
          bool (* atomicity flag *) * string list
          
    | Barrier of string list
    | Commit
 
(* I think this is right... *)
  let mk_init_write l v = Access(W,l,v,false,[])

  let pp_action a = match a with    
    | Access (d,l,v,ato,s) ->
	Printf.sprintf "%s(%s) %s%s=%s"
          (pp_dirn d)
          (BellBase.string_of_annot_list s)
          (A.pp_location  l)
	  (if ato then "*" else "")
	  (V.pp_v v)
    | Barrier (s) ->
      Printf.sprintf "f(%s)" (BellBase.string_of_annot_list s)
    | Commit -> "Commit"

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ ,v,_,_) -> Some v
    | _ -> None

    let read_of = value_of
    and written_of = value_of

    let location_of a = match a with
    | Access (_, l, _,_,_) -> Some l
    | _ -> None

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_,_) -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_,_) -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_,_)   -> true
    | _ -> false

    let is_atomic a = match a with
      | Access (_,_,_,true,_) -> 
	 assert (is_mem a); true
      | _ -> false

    let get_mem_dir a = match a with
    | Access (d,A.Location_global _,_,_,_) -> d
    | _ -> assert false

(* relative to the registers of the given proc *)
    let is_reg_store a (p:int) = match a with
    | Access (W,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false

    let is_reg_load a (p:int) = match a with
    | Access (R,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false

    let is_reg a (p:int) = match a with
    | Access (_,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false


(* Store/Load anywhere *)
    let is_store a = match a with
    | Access (W,_,_,_,_) -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_,_) -> true
    | _ -> false

    let is_reg_any a = match a with
    | Access (_,A.Location_reg _,_,_,_) -> true
    | _ -> false

    let is_reg_store_any a = match a with
    | Access (W,A.Location_reg _,_,_,_) -> true
    | _ -> false

    let is_reg_load_any a = match a with
    | Access (R,A.Location_reg _,_,_,_) -> true
    | _ -> false

(* Barriers *)
    let is_barrier a = match a with
    | Barrier _ -> true
    | _ -> false

    let barrier_of _a = None

    let same_barrier_id _ _ = false

(* Commits *)
   let is_commit a = match a with
   | Commit -> true
   | _ -> false

(* Local/Global Fences *)
   let is_local_fence _ = false
   let is_global_fence _ = false

(* Mutex operations *)
   let is_mutex_action _ = false

   let is_sc_action _ = false

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | Barrier _|Commit -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,ato,s) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',ato,s)
      | Barrier _ | Commit -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic a = match a with
      | Access (d,l,v,_,s) -> Access (d,l,v,true,s)
      | _ -> a

    (* Update the arch_sets based on the bell file *)

    let list_contains s st =
      List.mem st s

    let annot_in_list st ac = match ac with 
      | Access(_,_,_,_,s)
      | Barrier(s) -> (list_contains s st)	
      | _ -> false

    let pp_isync = ""
    let is_isync _a = false
    let arch_sets = []
    let arch_fences = []
      
      
end

