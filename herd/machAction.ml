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

(** Implementation of the action interface for machine models *)

module type A = sig
  include Arch.S
	    
  type lannot 

  val empty_annot : lannot
  val barrier_sets : (string * (barrier -> bool)) list
  val annot_sets : (string * (lannot -> bool)) list
  val is_atomic : lannot -> bool
  val is_isync : barrier -> bool 
  val pp_isync : string
  val pp_annot : lannot -> string
end

module Make (A : A) : sig

  type action =    
    | Access of Dir.dirn * A.location * A.V.v * A.lannot
    | Barrier of A.barrier
    | Commit of bool (* true = bcc / false = pred *)

  include Action.S with type action := action and module A = A

end = struct

  module A = A
  module V = A.V
  open Dir

  type action = 
    | Access of dirn * A.location * V.v * A.lannot
    | Barrier of A.barrier
    | Commit of bool
 
  let mk_init_write l v = Access(W,l,v,A.empty_annot)

  let pp_action a = match a with
    | Access (d,l,v,an) ->
	Printf.sprintf "%s%s%s=%s"
          (pp_dirn d)
          (A.pp_location l)
	  (A.pp_annot an)
	  (V.pp_v v)
    | Barrier b -> A.pp_barrier b
    | Commit bcc -> if bcc then "Commit" else "Pred"

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_) -> Some v
    | _ -> None

    let read_of = value_of
    and written_of = value_of

    let location_of a = match a with
    | Access (_, l, _,_) -> Some l
    | _ -> None

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_) -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_) -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_) -> true
    | _ -> false

    let is_atomic a = match a with
      | Access (_,_,_,annot) -> 
	 is_mem a && A.is_atomic annot
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
    | Access (W,_,_,_) -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_) -> true
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
    let is_barrier a = match a with
    | Barrier _ -> true
    | _ -> false

    let barrier_of a = match a with
    | Barrier b -> Some b
    | _ -> None

  let same_barrier_id _ _ = assert false

(* Commits *)

   let is_commit_bcc a = match a with
   | Commit b -> b
   | _ -> false

   let is_commit_pred a = match a with
   | Commit b -> not b
   | _ -> false

(* Architecture-specific sets *)

  let arch_sets =
    let bsets = 
      List.map
	(fun (tag,p) -> 
	 let p act = match act with
	   | Barrier b -> p b
	   | _ -> false
	 in tag,p) A.barrier_sets
    and asets = 
      List.map
	(fun (tag,p) -> 
	 let p act = match act with
	   | Access(_,_,_,annot) -> p annot
	   | _ -> false
	 in tag,p) A.annot_sets
    in
    bsets @ asets

  let arch_fences = []

  let is_isync act = match act with
  | Barrier b -> A.is_isync b
  | _ -> false

  let pp_isync = A.pp_isync

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | Barrier _|Commit _ -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,an) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',an)
      | Barrier _ | Commit _ -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic a = match a with
      | Access (d,l,v,an) -> Access (d,l,v,an)
      | _ -> a

    let annot_in_list _str _ac = false

end

