(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Implementation of the action interface for Bell *)

module Make (A : Arch_herd.S) : sig

  type action =
    | Access of
        Dir.dirn * A.location * A.V.v *
          bool * string list * MachSize.sz
    | Barrier of string list * (Label.Set.t * Label.Set.t) option
    | Commit

  include Action.S with module A = A and type action := action

end = struct

  module A = A
  module V = A.V
  open Dir

  type action =
    | Access of
        dirn * A.location * V.v *
          bool (* atomicity flag *) * string list * MachSize.sz

    | Barrier of string list * (Label.Set.t * Label.Set.t) option
    | Commit

(* I think this is right... *)
  let mk_init_write l sz v = Access(W,l,v,false,[],sz)

(*  Quite ad-hoc, should devise a more general mechanism *)
  let tr_annot = function
    | "rcu_read_unlock" -> "rcu-unlock"
    | "rcu_read_lock" -> "rcu-lock"
    | "sync" -> "sync-rcu"
    | s -> s

  let pp_annots = function
    | [] -> ""
    | xs ->
        let xs = List.map  tr_annot xs in
        "[" ^ BellBase.string_of_annot_list xs ^ "]"

  let pp_action a = match a with
  | Access (d,l,v,ato,s,_sz) ->
      Printf.sprintf "%s%s%s%s=%s"
        (pp_dirn d)
        (pp_annots s)
        (A.pp_location  l)
        (if ato then "*" else "")
        (V.pp_v v)
  | Barrier (s,o) ->
      (match o with
      | None ->
          Printf.sprintf "F%s" (pp_annots s)
      | Some(s1, s2) ->
          Printf.sprintf "F%s{%s}{%s}"
            (pp_annots s)
            (BellBase.string_of_labels s1)
            (BellBase.string_of_labels s2)
      )
  | Commit -> "Commit"

(* Utility functions to pick out components *)
  let value_of a = match a with
  | Access (_,_ ,v,_,_,_) -> Some v
  | _ -> None

  let read_of = value_of
  and written_of = value_of

  let location_of a = match a with
  | Access (_,loc, _,_,_,_) -> Some loc
  | _ -> None

(* relative to memory *)
  let is_mem_store a = match a with
  | Access (W,A.Location_global _,_,_,_,_) -> true
  | _ -> false

  let is_mem_load a = match a with
  | Access (R,A.Location_global _,_,_,_,_z) -> true
  | _ -> false

  let is_additional_mem_load _ = false

  let is_mem a = match a with
  | Access (_,A.Location_global _,_,_,_,_)   -> true
  | _ -> false

  let is_additional_mem _ = false

  let is_atomic a = match a with
  | Access (_,_,_,true,_,_) ->
      assert (is_mem a); true
  | _ -> false

  let get_mem_dir a = match a with
  | Access (d,A.Location_global _,_,_,_,_) -> d
  | _ -> assert false

  (* No mixed-size *)
  let get_mem_size a = match a with
  | Access (_,A.Location_global _,_,_,_,sz) -> sz
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
  | Access (W,_,_,_,_,_) -> true
  | _ -> false

  let is_load a = match a with
  | Access (R,_,_,_,_,_) -> true
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

  let compatible_accesses a1 a2 =
    (is_mem a1 && is_mem a2) || (is_reg_any a1 && is_reg_any a2)

  (* Barriers *)
  let is_barrier a = match a with
  | Barrier _ -> true
  | _ -> false

  let is_total_barrier a = match a with
  | Barrier (_,None) -> true
  | _ -> false

  let barrier_of _a = None

  let same_barrier_id _ _ = false

(* Commits *)
  let is_commit_bcc  a = match a with
  | Commit -> true
  | _ -> false

  let is_commit_pred  _a = false (* No predicated instructions... *)

(* Equations *)

  let undetermined_vars_in_action a =
    match a with
    | Access (_,l,v,_,_,_) ->
        let undet_loc = match A.undetermined_vars_in_loc l with
        | None -> V.ValueSet.empty
        | Some v -> V.ValueSet.singleton v in
        if V.is_var_determined v then undet_loc
        else V.ValueSet.add v undet_loc
    | Barrier _|Commit -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | Access (d,l,v,ato,s,sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v' = V.simplify_var soln v in
        Access (d,l',v',ato,s,sz)
    | Barrier _ | Commit -> a

(*************************************************************)
(* Add together event structures from different instructions *)
(*************************************************************)

(* Update the arch_sets based on the bell file *)

  let list_contains s st = List.mem st s

  let annot_in_list st ac = match ac with
  | Access(_,_,_,_,s,_)
  | Barrier(s,_) -> (list_contains s st)
(*jade: il manque les branches ici; et peut etre les rmw sauf s'ils sont dans Access?*)
  | _ -> false

  let pp_isync = ""
  let is_isync _a = false

  let arch_sets =
    ["X",is_atomic;
     "RMW",is_atomic;
     "Ftotal",is_total_barrier;]

  let arch_fences = []


end
