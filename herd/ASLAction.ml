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

module type S = sig
  include Arch_herd.S

  val is_local : reg -> bool
end

module Make (A : S) = struct
  module A = A
  module V = A.V

  type action =
    | Access of dirn * A.location * A.V.v * MachSize.sz * AArch64Annot.t
    | Barrier of A.barrier
    | TooFar of string
    | NoAction

  let mk_init_write loc sz v = Access (W, loc, v, sz, AArch64Annot.N)

  let pp_action = function
    | Access (d, l, v, _sz,a) ->
        Printf.sprintf "%s%s=%s%s"
          (pp_dirn d) (A.pp_location l) (V.pp false v)
          (let open AArch64Annot in
           match a with
           | N -> ""
           | _ -> AArch64Annot.pp a)
    | Barrier b -> A.pp_barrier_short b
    | TooFar msg -> Printf.sprintf "TooFar:%s" msg
    | NoAction -> ""

  let is_local = function
    | Access (_, A.Location_reg (_, r), _, _, _) -> A.is_local r
    | _ -> false

  (* Some architecture-specific sets and relations, with their definitions *)
  let arch_sets = [ ("ASLLocal", is_local) ]
  let arch_rels = []
  let arch_dirty = []
  let is_isync _ = false
  let pp_isync = "ISYNC"

  (**************************************)
  (* Access to sub_components of events *)
  (**************************************)

  let value_of = function Access (_, _, v, _, _) -> Some v | _ -> None
  let read_of = function Access (R, _, v, _, _) -> Some v | _ -> None
  let written_of = function Access (W, _, v, _, _) -> Some v | _ -> None
  let location_of = function Access (_, l, _, _, _) -> Some l | _ -> None

  (************************)
  (* Predicates on events *)
  (************************)

  (* relative to memory *)
  let is_mem_store = function
    | Access (W, A.Location_global _, _, _, _) -> true
    | _ -> false

  let is_mem_load = function
    | Access (R, A.Location_global _, _, _, _) -> true
    | _ -> false

  let is_additional_mem_load _action = false

  let is_mem = function
    | Access (_, A.Location_global _, _, _, _) -> true
    | _ -> false

  let is_tag _action = false
  let is_additional_mem _action = false
  let is_atomic _action = false
  let is_fault _action = false
  let to_fault _action = None

  let get_mem_dir = function
    | Access (d, A.Location_global _, _, _, _) -> d
    | _ -> assert false

  let get_mem_size = function
    | Access (_, A.Location_global _, _, sz, _) -> sz
    | _ -> assert false

  let is_pte_access _action = false
  let is_explicit _action = true
  let is_not_explicit _action = false

  (* relative to the registers of the given proc *)
  let is_reg_store = function
    | Access (W, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | _ -> fun _ -> false

  let is_reg_load = function
    | Access (R, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | _ -> fun _ -> false

  let is_reg = function
    | Access (_, A.Location_reg (p, _), _, _, _) -> Proc.equal p
    | _ -> fun _ -> false

  (* Reg events, proc not specified *)
  let is_reg_store_any = function
    | Access (W, A.Location_reg _, _, _, _) -> true
    | _ -> false

  let is_reg_load_any = function
    | Access (R, A.Location_reg _, _, _, _) -> true
    | _ -> false

  let is_reg_any = function
    | Access (_, A.Location_reg _, _, _, _) -> true
    | _ -> false

  (* Store/Load to memory or register *)
  let is_store = function Access (W, _, _, _, _) -> true | _ -> false
  let is_load = function Access (R, _, _, _, _) -> true | _ -> false

  (* Compatible accesses *)
  let compatible_accesses _a1 _a2 = true

  (* for bell annotations *)
  let annot_in_list _str _act = false

  (* Barriers *)
  let is_barrier = function
    | Barrier _  -> true
    | _ -> false

  let barrier_of = function
    | Barrier b -> Some b
    | _ -> None

  let same_barrier_id _a1 _a2 = assert false

  (* Commits *)
  let is_bcc _action = false
  let is_pred ?cond:_ _action = false
  let is_commit _action = false

  (* Unrolling control *)
  let toofar msg = TooFar msg
  let is_toofar = function TooFar _ -> true | _ -> false

  (********************)
  (* Equation solving *)
  (********************)

  let undetermined_vars_in_action = function
    | Access (_, l, v, _, _) ->
        V.ValueSet.union (A.undetermined_vars_in_loc l) (V.undetermined_vars v)
    | Barrier _ | TooFar _ | NoAction -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | Access (d, l, v, sz, a) ->
        Access
          (d, A.simplify_vars_in_loc soln l,
           V.simplify_var soln v, sz, a)
    | Barrier _ | TooFar _ | NoAction -> a
end

module FakeModuleForCheckingSignatures (A : S) : Action.S
       with module A = A =  Make (A)
