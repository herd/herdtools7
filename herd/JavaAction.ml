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

module Make (A : Arch_herd.S) : sig

  type action =
    | Access of Dir.dirn * A.location * A.V.v * AccessModes.t * MachSize.sz
    | Fence of AccessModes.t
    | RMW of A.location * A.V.v * A.V.v * AccessModes.t * MachSize.sz
    | CutOff of string

  include Action.S with type action := action and module A = A

end = struct

  module A = A
  module V = A.V
  open Dir
  open Printf

  type action =
    | Access of Dir.dirn * A.location * A.V.v * AccessModes.t * MachSize.sz
    | Fence of AccessModes.t
    | RMW of A.location * A.V.v * A.V.v * AccessModes.t * MachSize.sz
    | CutOff of string

  
  let mk_init_write l sz v = Access (W, l, v, AccessModes.NA, sz)

  let par f x = sprintf "(%s)" (f x) (*for access modes*)

  let pp_action a =
    match a with
    | Access (d, l, v, am, _) ->
        (sprintf "%s%s%s=%s"
                (pp_dirn d)
                (par AccessModes.pp_access_modes am)
                (A.pp_location l)
                (V.pp_v v))

    | Fence b ->
        sprintf "Fence%s" (par AccessModes.pp_access_modes b)


    | RMW (l, v1, v2, am, _) ->
        (sprintf "RMW(%s)%s(%s>%s)"
                (AccessModes.pp_access_modes am)
                (A.pp_location l)
                (V.pp_v v1)
                (V.pp_v v2))
    | CutOff m -> (sprintf "CutOff: %s" m)

  let is_isync _ = raise Misc.NoIsync
  let pp_isync = "???"

  let is_barrier a = match a with
  | Fence _ -> true
  | _ -> false

  let barrier_of _ = assert false
  let same_barrier_id _ _ = assert false

  let is_rmw a = match a with
  | RMW _ -> true
  | _ -> false

  let is_reg a (p:int) = match a with
  | Access (_,A.Location_reg (q,_),_,_,_) -> p = q
  | _ -> false

  let is_mem a = match a with
  | Access (_,A.Location_global _,_,_,_) -> true
  | RMW (A.Location_global _,_,_,_,_) -> true
  | _ -> false

  let is_ifetch _ = false

  let is_additional_mem _ = false

  let is_additional_mem_load _ = false

  let is_atomic a = match a with
  | Access (_,A.Location_global _,_,_,_) -> true
  | RMW _ -> true
  | _ -> false

  let to_fault _ = None

  let is_reg_store a (p:int) = match a with
  | Access (W,A.Location_reg (q,_),_,_,_) -> p = q
  | _ -> false

  let is_reg_load a (p:int) = match a with
  | Access (R,A.Location_reg (q,_),_,_,_) -> p = q
  | _ -> false

  (* let is_reg_any a = match a with
  | Access (_,A.Location_reg _,_,_,_) -> true
  | _ -> false *)

  let get_mem_dir a = match a with
  | Access (d,A.Location_global _,_,_,_) -> d
  | _ -> assert false

  let get_mem_size a = match a with
  | Access (_,A.Location_global _,_,_,sz) -> sz
  | _ -> assert false

  let is_pte_access _ = false

  let is_mem_store a = match a with
  | Access (W,A.Location_global _,_,_,_)
  | RMW (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_mem_load a = match a with
  | Access (R,A.Location_global _,_,_,_)
  | RMW (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let value_of a = match a with
  | Access (_,_,v,_,_) -> Some v
  | _ -> None

  let read_of a = match a with
  | Access (R,_ , v,_,_)
  | RMW (_,v,_,_,_)
    -> Some v
  | _ -> None

  let written_of a = match a with
  | Access (W,_ , v,_,_)
  | RMW (_,_,v,_,_)
    -> Some v
  | _ -> None

  let location_of a = match a with
  | Access (_, l, _,_,_) -> Some l
  | RMW (l,_,_,_,_) -> Some l
  | Fence _ -> None
  | _ -> None

(* Store/Load anywhere *)
  let is_store a = match a with
  | Access (W,_,_,_,_)
  | RMW _
    -> true
  | _ -> false

  let is_load a = match a with
  | Access (R,_,_,_,_)
  | RMW _ -> true
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

  let compatible_accesses a1 a2 =
    (is_mem a1 && is_mem a2) || (is_reg_any a1 && is_reg_any a2)


  let arch_rels = []
  let arch_dirty = []

  let is_fault _ = false
  let is_tag _ = false
  let cutoff msg = CutOff msg
  let is_cutoff = function
    | CutOff _ -> true
    | _ -> false
  let as_cutoff = function
    | CutOff msg -> Some msg
    | _ -> None

  let is_bcc _ = false
  let is_pred ?cond:_ _ = false
  let is_commit _ = false

  include Explicit.NoAction

  let annot_in_list _ _ = false



  let mo_matches target a = 
  match a with
  | Access(_,_,_,mo,_)
  | RMW (_,_,_,mo,_)
  | Fence (mo) -> (mo = target)
  | _ -> false


let undetermined_vars_in_action a =
    match a with
    | Access (_,l,v,_,_)->
        V.ValueSet.union
          (A.undetermined_vars_in_loc l)
          (V.undetermined_vars v)

    | RMW (l,v1,v2,_,_) ->
        V.ValueSet.union3
          (A.undetermined_vars_in_loc l)
          (V.undetermined_vars v1)
          (V.undetermined_vars v2)

    | Fence _ -> V.ValueSet.empty
    | CutOff _ -> assert false

  let simplify_vars_in_action soln a =
    match a with
    | Access (d,l,v,mo,sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v' = V.simplify_var soln v in
        Access (d,l',v',mo,sz)
    | RMW(l,v1,v2,mo,sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
        let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo,sz)
    | Fence _ -> a
    | CutOff _ -> assert false


  let arch_sets = [
    "RMW", (fun e -> is_rmw e);
    "REL", mo_matches AccessModes.Release;
    "V", mo_matches AccessModes.Volatile;
    "ACQ", mo_matches AccessModes.Acquire;
    "RA", (fun e ->
        mo_matches AccessModes.Acquire e ||
        mo_matches AccessModes.Release e);
    "O", mo_matches AccessModes.Opaque
 ]

  (* let arch_fences = [] *)

end
