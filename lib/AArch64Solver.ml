(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make (V: Value.S) = struct
  include V

  type solver_state = PAC.solver_state

  let empty_solver = PAC.empty_solver

  let pp_solver_state st = PAC.pp_solver st

  let normalize cst st = Constant.normalize cst st

  let compare_solver_state s1 s2 =
  (* Comparison is only used to group the final states before pretty printing so
   * we only compare the equalities of the internal collision solvers *)
    PAC.compare_solver_state s1 s2

  let add_predicate b pred st =
    match pred with
    | AArch64Op.Eq (p1,p2) ->
        let add_pred =
          if b then PAC.add_equality else PAC.add_inequality in
        add_pred p1 p2 st
end
