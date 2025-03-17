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
module Make (C : sig
  val is_morello : bool
end) : Value.AArch64 = struct
  module AArch64I = AArch64Instr.Make (C)
  module AArch64Cst = SymbConstant.Make (Int64Scalar) (AArch64PteVal) (AArch64I)
  module NoCst = SymbConstant.Make (Int64Scalar) (PteVal.No) (AArch64I)
  module NoArchOp = ArchOp.No(NoCst)
  module AArch64Op' = AArch64Op
  module AArch64Op = AArch64Op.Make(Int64Scalar)(NoArchOp)
  module AArch64Value = SymbValue.Make (AArch64Cst) (AArch64Op)
  include AArch64Solver.Make(AArch64Value)

  type solver_state = PAC.solver_state

  let empty_solver = PAC.empty_solver

  let pp_solver_state st = PAC.pp_solver st

  let add_equality c1 c2 st =
    match Constant.collision c1 c2 with
    | Some (p1, p2) -> begin
      match PAC.add_equality p1 p2 st with
      | Some st -> Some st
      | None -> None
    end
    | None -> if Cst.eq c1 c2 then Some st else None

  let add_inequality c1 c2 st =
    match Constant.collision c1 c2 with
    | Some (p1, p2) -> begin
      match PAC.add_inequality p1 p2 st with
      | Some st -> Some st
      | None -> None
    end
    | None -> if Cst.eq c1 c2 then None else Some st

  let add_predicate _ _ _ = failwith ""

  let normalize cst st = Constant.normalize cst st

  let compare_solver_state s1 s2 =
  (* Comparison is only used to group the final states before pretty printing so
   we only compare the equalities of the internal collision solvers *)
    PAC.compare_solver_state s1 s2

  let empty_solver = PAC.empty_solver

  let pp_solver_state st = PAC.pp_solver st

  let add_equality c1 c2 st =
    match Constant.collision c1 c2 with
    | Some (p1, p2) -> begin
      match PAC.add_equality p1 p2 st with
      | Some st -> Some st
      | None -> None
    end
    | None -> if Cst.eq c1 c2 then Some st else None

  let add_inequality c1 c2 st =
    match Constant.collision c1 c2 with
    | Some (p1, p2) -> begin
      match PAC.add_inequality p1 p2 st with
      | Some st -> Some st
      | None -> None
    end
    | None -> if Cst.eq c1 c2 then None else Some st

  let add_predicate _ _ _ = failwith ""

  let normalize cst st = Constant.normalize cst st

  let compare_solver_state s1 s2 =
  (* Comparison is only used to group the final states before pretty printing so
   we only compare the equalities of the internal collision solvers *)
    PAC.compare_solver_state s1 s2

  let add_predicate b pred st =
    let add_pred =
      if b then PAC.add_equality else PAC.add_inequality in
    match pred with
    | AArch64Op'.Eq (p1,p2) ->
        begin
          match add_pred p1 p2 st with
          | Some st -> Some st
          | None -> None
        end
end
