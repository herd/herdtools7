(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Collect registers from tests *)



module Make(A:Arch_tools.S) = struct

  let collect_pseudo f = A.pseudo_fold (fun k ins -> A.fold_regs f k ins)

  let collect_code c =
    let rs,_ =
      List.fold_left
	(collect_pseudo  (A.RegSet.add,(fun _ () -> ())))
	(A.RegSet.empty,())
        c in
    rs

  let add_proc_reg p r m =
    let rs = A.ProcMap.safe_find A.RegSet.empty p m in
    A.ProcMap.add p (A.RegSet.add r rs) m

  let collect_location loc m = match loc with
  | A.Location_reg (p,r) -> add_proc_reg p r m
  | A.Location_global _|A.Location_deref _ -> m

  let collect_state_atom (loc,_) = collect_location loc

  let collect_atom a regs =
    let open ConstrGen in
    match a with
    | LV (loc,_) -> collect_location loc regs
    | LL (loc1,loc2) ->  collect_location loc1 (collect_location loc2 regs)
    | FF _ -> regs

  let collect_state st = List.fold_right collect_state_atom st

  let collect_prop = ConstrGen.fold_prop collect_atom

  let collect_filter = function
    | None -> Misc.identity
    | Some p -> collect_prop p

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs = List.fold_right (fun (loc,_) -> collect_location loc)

  open MiscParser

  let collect t = 
    let m =
      List.fold_left
        (fun m ((p,_),cs) ->
          A.ProcMap.add p (collect_code cs) m)
        A.ProcMap.empty t.prog in
    let m = collect_state t.init m in
    let m = collect_locs t.locations m in
    let m = collect_filter t.filter m in
    let m = collect_constr t.condition m in
    m
end
