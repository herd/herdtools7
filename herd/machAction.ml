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
  include Arch_herd.S

  type lannot
  val empty_annot : lannot
  val barrier_sets : (string * (barrier -> bool)) list
  val annot_sets : (string * (lannot -> bool)) list
  val is_atomic : lannot -> bool
  val is_isync : barrier -> bool
  val pp_isync : string
  val pp_annot : lannot -> string
end

module type Config = sig
  val hexa : bool
end
module Make (C:Config) (A : A) : sig

  type action =
    | Access of Dir.dirn * A.location * A.V.v * A.lannot * MachSize.sz
    | TagAccess of Dir.dirn * A.location * A.V.v
    | Barrier of A.barrier
    | Commit of bool (* true = bcc / false = pred *)
(* Atomic modify, (location,value read, value written, annotation *)
    | Amo of A.location * A.V.v * A.V.v * A.lannot * MachSize.sz
(* NB: Amo used in some arch only (ie RISCV) *)
    | Fault of A.inst_instance_id * A.location

  include Action.S with type action := action and module A = A

end = struct

  module A = A
  module V = A.V
  open Dir

  type action =
    | Access of dirn * A.location * V.v * A.lannot * MachSize.sz
    | TagAccess of Dir.dirn * A.location * A.V.v
    | Barrier of A.barrier
    | Commit of bool
    | Amo of A.location * A.V.v * A.V.v * A.lannot * MachSize.sz
    | Fault of A.inst_instance_id * A.location

  let mk_init_write l sz v = match v with
  | A.V.Val (Constant.Tag _) -> TagAccess (W,l,v)
  | _ ->  Access(W,l,v,A.empty_annot,sz)

  let pp_action a = match a with
  | Access (d,l,v,an,sz) ->
      Printf.sprintf "%s%s%s%s=%s"
        (pp_dirn d)
        (A.pp_location l)
        (A.pp_annot an)
        (if sz = MachSize.Word then "" else MachSize.pp_short sz)
        (V.pp C.hexa v)
  | TagAccess (d,l,v) ->
      Printf.sprintf "%s%s=%s"
        (pp_dirn d)
        (A.pp_location l)
        (V.pp C.hexa v)
  | Barrier b -> A.pp_barrier_short b
  | Commit bcc -> if bcc then "Commit" else "Pred"
  | Amo (loc,v1,v2,an,sz) ->
      Printf.sprintf "RMW(%s)%s%s(%s>%s)"
        (A.pp_annot an)
        (A.pp_location loc) (MachSize.pp_short sz)
        (V.pp C.hexa v1) (V.pp C.hexa v2)
  | Fault (ii,loc) ->
      Printf.sprintf "Fault(proc:%s,poi:%s,loc:%s)"
        (A.pp_proc ii.A.proc)
        (A.pp_prog_order_index ii.A.program_order_index)
        (A.pp_location loc)

(* Utility functions to pick out components *)
  let value_of a = match a with
  | TagAccess (_,_,v)|Access (_,_ , v,_,_)
    -> Some v
  | Barrier _|Commit _|Amo _|Fault _
    -> None

  let read_of a = match a with
  | TagAccess (R,_,v)
  | Access (R,_,v,_,_)
  | Amo (_,v,_,_,_)
    -> Some v
  | TagAccess (W,_,_)|Access (W, _, _, _,_)|Barrier _|Commit _|Fault _
    -> None

  and written_of a = match a with
  | TagAccess (W,_,v)
  | Access (W,_,v,_,_)
  | Amo (_,_,v,_,_)
    -> Some v
  | TagAccess(R,_,_)|Access (R, _, _, _,_)|Barrier _|Commit _|Fault _
    -> None

  let location_of a = match a with
  | TagAccess (_,l,_)
  | Access (_, l, _,_,_)
  | Amo (l,_,_,_,_)
  | Fault (_,l)
    -> Some l
  | Barrier _|Commit _ -> None

(* relative to memory *)
  let is_mem_store a = match a with
  | TagAccess (W,A.Location_global _,_)
  | Access (W,A.Location_global _,_,_,_)
  | Amo (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_mem_load a = match a with
  | TagAccess (R,A.Location_global _,_)
  | Access (R,A.Location_global _,_,_,_)
  | Amo (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_additional_mem_load _ = false

  let is_mem a = match a with
  | TagAccess (_,A.Location_global _,_)
  | Access (_,A.Location_global _,_,_,_)
  | Amo (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_additional_mem _ = false

  let is_atomic a = match a with
  | Access (_,_,_,annot,_) ->
      is_mem a && A.is_atomic annot
  | _ -> false

  let is_tag = function
    | TagAccess _ -> true
    | Access _ | Barrier _ | Commit _ | Amo _ | Fault _ -> false

  let get_mem_dir a = match a with
  | TagAccess (d,A.Location_global _,_)
  | Access (d,A.Location_global _,_,_,_) -> d
  | _ -> assert false

  let get_mem_size a = match a with
  | Access (_,A.Location_global _,_,_,sz) -> sz
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
  | TagAccess (W,_,_)|Access (W,_,_,_,_)|Amo _ -> true
  | TagAccess (R,_,_)|Access (R,_,_,_,_)|Barrier _|Commit _|Fault _ -> false

  let is_load a = match a with
  | TagAccess(R,_,_)|Access (R,_,_,_,_)|Amo _ -> true
  | TagAccess(W,_,_)|Access (W,_,_,_,_)|Barrier _|Commit _|Fault _ -> false

  let compatible_categories loc1 loc2 = match loc1,loc2 with
  | (A.Location_global _,A.Location_global _)
  | (A.Location_reg _,A.Location_reg _)
  | (A.Location_deref _,A.Location_deref _)
    -> true
  | (A.Location_global _,(A.Location_deref _|A.Location_reg _))
  | (A.Location_deref _,(A.Location_global _|A.Location_reg _))
  | (A.Location_reg _,(A.Location_global _|A.Location_deref _))
    -> false

  let compatible_accesses a1 a2 = match a1,a2 with
  | (TagAccess _,TagAccess _) -> true
  | (Access (_,loc1,_,_,_)|Amo (loc1,_,_,_,_)),
     (Access (_,loc2,_,_,_)|Amo (loc2,_,_,_,_))
    ->
      compatible_categories loc1 loc2
  | (TagAccess _,(Access _|Amo _))
  | ((Access _|Amo _),TagAccess _)
    -> false
  | _,_ -> assert false

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
          | Access(_,_,_,annot,_)|Amo (_,_,_,annot,_) -> p annot
          | _ -> false
          in tag,p) A.annot_sets
    in
    ("T",is_tag) :: bsets @ asets

  let arch_fences = []

  let is_isync act = match act with
  | Barrier b -> A.is_isync b
  | _ -> false

  let pp_isync = A.pp_isync

(* Equations *)
  let add_v_undet v vs =
    if V.is_var_determined v then vs
    else  V.ValueSet.add v vs

  let undetermined_vars_in_action a =
    match a with
    | TagAccess (_,l,v)
    | Access (_,l,v,_,_) ->
        let undet_loc = match A.undetermined_vars_in_loc l with
        | None -> V.ValueSet.empty
        | Some v -> V.ValueSet.singleton v in
        add_v_undet v undet_loc
    | Amo (loc,v1,v2,_,_) ->
        let undet = match A.undetermined_vars_in_loc loc with
        | None -> V.ValueSet.empty
        | Some v -> V.ValueSet.singleton v in
        add_v_undet v1 (add_v_undet v2 undet)
   | Barrier _|Commit _|Fault _ -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | TagAccess (d,l,v) ->
        let l = A.simplify_vars_in_loc soln l in
        let v = V.simplify_var soln v in
        TagAccess (d,l,v)
    | Access (d,l,v,an,sz) ->
        let l = A.simplify_vars_in_loc soln l in
        let v = V.simplify_var soln v in
        Access (d,l,v,an,sz)
    | Amo (loc,v1,v2,an,sz) ->
        let loc =  A.simplify_vars_in_loc soln loc in
        let v1 = V.simplify_var soln v1 in
        let v2 = V.simplify_var soln v2 in
        Amo (loc,v1,v2,an,sz)
    | Fault (ii,loc) ->
        let loc = A.simplify_vars_in_loc soln loc in
        Fault(ii,loc)
    | Barrier _ | Commit _ -> a

  let annot_in_list _str _ac = false

end
