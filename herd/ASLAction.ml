(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Dir

module type Config = sig
  val hexa: bool
end

module type S = sig
  include Arch_herd.S
  val is_local : reg -> bool
  val is_pc : reg -> bool
end

module Make (C: Config) (A : S) = struct
  module A = A
  module V = A.V

  type action =
    | Access of
        dirn * A.location * A.V.v * MachSize.sz
        * ( AArch64Annot.t * AArch64Explicit.explicit * Access.t)
    | Fault of A.inst_instance_id * A.location * Dir.dirn * A.I.FaultType.t
    | Barrier of A.barrier
    | Branching of string option
    | CutOff of string
    | NoAction

  let mk_init_write loc sz v =
    let acc = A.access_of_location_init loc in
    Access (W, loc, v, sz, (AArch64Annot.N, AArch64Explicit.Exp,acc))

  let pp_action = function
    | Access (d, l, v, _sz,(a,e,_)) ->
        Printf.sprintf "%s%s=%s%s%s"
          (pp_dirn d) (A.pp_location l) (V.pp C.hexa v)
          (let open AArch64Annot in
           match a with
           | N -> ""
           | _ -> AArch64Annot.pp a)
          (let open AArch64Explicit in
           match e with
           | Exp -> ""
           | _ -> AArch64Explicit.pp e)
    | Fault (_,loc,d,t) ->
        Printf.sprintf "Fault(%s,%s,%s)"
          (Dir.pp_dirn d)
          (A.pp_location loc) (A.I.FaultType.pp t)
    | Barrier b -> A.pp_barrier_short b
    | Branching txt ->
       Printf.sprintf "Branching(%s)"
         (Misc.app_opt_def "" Misc.identity txt)
    | CutOff msg -> Printf.sprintf "CutOff:%s" msg
    | NoAction -> ""

  let is_local = function
    | Access (_, A.Location_reg (_, r), _, _, _) -> A.is_local r
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false

  (** Write to PC *)
  let is_wpc = function
    | Access (Dir.W, A.Location_reg (_, r), _, _, _) -> A.is_pc r
    | _ -> false

  (* Some architecture-specific sets and relations, with their definitions *)
  let arch_sets = [ ("ASLLocal", is_local); ("WPC",is_wpc); ]
  let arch_rels = []
  let arch_dirty = []
  let is_isync _ = false
  let pp_isync = "ISYNC"

  (**************************************)
  (* Access to sub_components of events *)
  (**************************************)

  let value_of =
    function
    | Access (_, _, v, _, _) -> Some v
    | Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> None

  let read_of =
    function
    | Access (R, _, v, _, _) -> Some v
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> None

  let written_of =
    function
    | Access (W, _, v, _, _) -> Some v
    | Access _|Fault _|Barrier _| Branching _|CutOff _|NoAction
      -> None

  let location_of =
    function
    | Access (_, l, _, _, _) -> Some l
    | Branching _|Fault _|Barrier _|CutOff _|NoAction
     -> None

  (************************)
  (* Predicates on events *)
  (************************)

  (* relative to memory *)
  let is_mem_store = function
    | Access (W, A.Location_global _, _, _, _) -> true
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_mem_load = function
    | Access (R, A.Location_global _, _, _, _) -> true
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_additional_mem_load = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_mem = function
    | Access (_, A.Location_global _, _, _, _) -> true
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_ifetch = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false
  let is_tag = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false
  let is_additional_mem  = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false
  let is_atomic = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false
  let is_fault = function
    | Fault _ -> true
    | Access _| Branching _|Barrier _|CutOff _|NoAction
      -> false

  let to_fault = function
    | Fault (i,loc,_d,t) ->
        let loc = A.global loc in
        Some ((i.A.proc,i.A.labels),loc,Some t,None)
    | Access _| Branching _|Barrier _|CutOff _|NoAction
      -> None

  let get_mem_dir = function
    | Access (d, A.Location_global _, _, _, _) -> d
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> assert false

  let get_mem_size = function
    | Access (_, A.Location_global _, _, sz, _) -> sz
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> assert false

  let is_pte_access = function
    | Access (_,_,_,_,(_,_,Access.PTE)) -> true
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_explicit = function
    | Access (_,_,_,_,(_,e,_)) -> AArch64Explicit.is_explicit_annot e
    | Branching _|Fault _|Barrier _|CutOff _|NoAction
      -> false

  let is_not_explicit = function
    | Access (_,_,_,_,(_,e,_)) -> AArch64Explicit.is_not_explicit_annot e
    | Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  (* relative to the registers of the given proc *)
  let is_reg_store = function
    | Access (W, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      ->
       fun _ -> false

  let is_reg_load = function
    | Access (R, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      ->
       fun _ -> false


  let is_reg = function
    | Access (_, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> fun _ -> false

  (* Reg events, proc not specified *)
  let is_reg_store_any = function
    | Access (W, A.Location_reg _, _, _, _) -> true
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false

  let is_reg_load_any = function
    | Access (R, A.Location_reg _, _, _, _) -> true
      | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false


  let is_reg_any = function
    | Access (_, A.Location_reg _, _, _, _) -> true
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false

  (* Store/Load to memory or register *)
  let is_store =
    function
    | Access (W, _, _, _, _) -> true
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false

  let is_load =
    function
    | Access (R, _, _, _, _) -> true
    | Access _|Fault _|Barrier _|Branching _|CutOff _|NoAction
      -> false

  (* Compatible accesses *)
  let compatible_accesses _a1 _a2 = true

  (* for bell annotations *)
  let annot_in_list _str _act = false

  (* Barriers *)
  let is_barrier = function
    | Barrier _  -> true
    | Access _|Fault _|Branching _|CutOff _|NoAction
      -> false

  let barrier_of = function
    | Barrier b -> Some b
    | Access _|Fault _|Branching _|CutOff _|NoAction
      -> None

  let same_barrier_id _a1 _a2 = assert false

  (* Commits *)
  let is_bcc  = function
    | Access _|Fault _|Branching _|Barrier _|CutOff _|NoAction
      -> false

  let is_pred ?(cond=None) = function
    | Branching cond0 ->
       Option.is_none cond || Option.equal String.equal cond cond0
    | Access _|Fault _|Barrier _|CutOff _|NoAction -> false

  let is_commit = function
    | Branching _ -> true
    | Access _|Fault _|Barrier _|CutOff _|NoAction -> false

  (* Unrolling control *)
  let cutoff msg = CutOff msg
  and is_cutoff = function
    | CutOff _ -> true
    | Access _|Fault _|Barrier _|Branching _|NoAction
      -> false
  and as_cutoff = function
    | CutOff msg -> Some msg
    | Access _|Fault _|Barrier _|Branching _|NoAction
      -> None

  (********************)
  (* Equation solving *)
  (********************)

  let undetermined_vars_in_action = function
    | Access (_, l, v, _, _) ->
        V.ValueSet.union (A.undetermined_vars_in_loc l) (V.undetermined_vars v)
    | Fault (_,loc,_,_) -> A.undetermined_vars_in_loc loc
    | Barrier _ | Branching _| CutOff _ | NoAction -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | Access (d, l, v, sz, a) ->
        Access
          (d, A.simplify_vars_in_loc soln l,
           V.simplify_var soln v, sz, a)
    | Fault (i,loc,d,t) ->
        Fault (i,A.simplify_vars_in_loc soln loc,d,t)
    | Barrier _ | Branching _ | CutOff _ | NoAction -> a
end

module FakeModuleForCheckingSignatures (C: Config) (A : S) : Action.S
       with module A = A =  Make (C) (A)
