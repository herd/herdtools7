(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  module V : Value.S

  type arch_pred

  val eq_satisfiable : V.Cst.v -> V.Cst.v -> arch_pred option
  val compare_predicate : arch_pred -> arch_pred -> int
  val enforce_no_collision : arch_pred -> arch_pred
  val pp_predicate : arch_pred -> string

  val fold_predicate_vars : (V.v -> 'a -> 'a) -> arch_pred -> 'a -> 'a
  val map_predicate : (V.v -> V.v) -> arch_pred -> arch_pred
  val pred_assume_collision : V.v -> V.v -> arch_pred
  val pred_assume_no_collision : V.v -> V.v -> arch_pred

  type arch_solver_state
  val empty_solver : arch_solver_state
  val add_predicate : arch_pred -> arch_solver_state -> arch_solver_state option
  val normalize : V.Cst.v -> arch_solver_state -> V.Cst.v
  val compare_solver_state : arch_solver_state -> arch_solver_state -> int

  type finalized_solver_state
  val render_solver_state : arch_solver_state -> finalized_solver_state option
  val pp_solver_state : finalized_solver_state -> string
  val compare_finalized_solver_state :
    finalized_solver_state -> finalized_solver_state -> int
end

module No (V : Value.S) : S with module V = V = struct
  module V = V

  type arch_pred = unit

  let eq_satisfiable _ _ = None
  let compare_predicate _ _ = 0
  let enforce_no_collision _ = ()
  let pp_predicate _ = ""

  let fold_predicate_vars _ _ acc = acc
  let map_predicate _ pred = pred
  let pred_assume_collision _ _ = ()
  let pred_assume_no_collision _ _ = ()

  type arch_solver_state = unit
  let empty_solver = ()
  let add_predicate _ _ = Some ()
  let normalize cst _ = cst
  let compare_solver_state _ _ = 0

  type finalized_solver_state = unit
  let render_solver_state _ = None
  let pp_solver_state _ = ""
  let compare_finalized_solver_state _ _ = 0
end

module Pac (V : Value.S) : S with module V = V = struct
  module V = V

  type arch_pred =
    | AssumeCollision of V.v * V.v
    | AssumeNoCollision of V.v * V.v

  let compare_predicate p1 p2 =
    match p1, p2 with
    | AssumeCollision (v1,v2), AssumeCollision (w1,w2)
    | AssumeNoCollision (v1,v2), AssumeNoCollision (w1,w2) ->
        Misc.pair_compare V.compare V.compare (v1,v2) (w1,w2)
    | AssumeCollision _, AssumeNoCollision _ -> -1
    | AssumeNoCollision _, AssumeCollision _ -> 1

  let pp_predicate = function
    | AssumeCollision (v1,v2) ->
        Printf.sprintf "AssumeCollision(%s,%s)" (V.pp_v v1) (V.pp_v v2)
    | AssumeNoCollision (v1,v2) ->
        Printf.sprintf "AssumeNoCollision(%s,%s)" (V.pp_v v1) (V.pp_v v2)

  let enforce_no_collision = function
    | AssumeCollision (v1,v2) -> AssumeNoCollision (v1,v2)
    | AssumeNoCollision (v1,v2) -> AssumeNoCollision (v1,v2)

  let fold_predicate_vars f pred acc =
    match pred with
    | AssumeCollision (v1,v2)
    | AssumeNoCollision (v1,v2) -> f v1 (f v2 acc)

  let map_predicate f pred =
    match pred with
    | AssumeCollision (v1,v2) -> AssumeCollision (f v1, f v2)
    | AssumeNoCollision (v1,v2) -> AssumeNoCollision (f v1, f v2)

  let eq_satisfiable c1 c2 =
    match Constant.collision c1 c2 with
    | Some _ -> Some (AssumeCollision (V.Val c1, V.Val c2))
    | None -> None

  let pred_assume_collision v1 v2 = AssumeCollision (v1, v2)
  let pred_assume_no_collision v1 v2 = AssumeNoCollision (v1, v2)

  type arch_solver_state = PAC.solver_state

  let empty_solver = PAC.empty_solver
  let normalize = Constant.normalize
  let compare_solver_state s1 s2 = PAC.compare_solver_state s1 s2

  type finalized_solver_state = PAC.solver_state
  let render_solver_state st = Some st
  let pp_solver_state st = PAC.pp_solver st
  let compare_finalized_solver_state s1 s2 =
    PAC.compare_finalized_solver_state s1 s2

  let add_predicate pred st =
    let add_assume b v1 v2 =
      match v1, v2 with
      | V.Val c1, V.Val c2 -> begin
          match Constant.collision c1 c2 with
          | Some (p1,p2) ->
              if b then PAC.add_equality p1 p2 st
              else PAC.add_inequality p1 p2 st
          | None ->
              if V.equal v1 v2 then
                if b then Some st else None
              else
                if b then None else Some st
        end
      | _, _ -> Some st
    in
    match pred with
    | AssumeCollision (v1, v2) -> add_assume true v1 v2
    | AssumeNoCollision (v1, v2) -> add_assume false v1 v2
end
