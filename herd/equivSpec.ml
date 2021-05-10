(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(S:Sem.Semantics) = struct

  let dbg = false

  module Instance = struct
    type t = S.A.inst_instance_id
    let compare = S.A.inst_instance_compare
  end

  module InstMap = MyMap.Make(Instance)
  module InstSet = MySet.Make(Instance)


  (* Utilities on instance graphs coded as neighbours maps *)
  let succs i m = InstMap.safe_find InstSet.empty i m
  let add_succ i1 i2 m = InstMap.add i1 (InstSet.add i2 (succs i1 m)) m
  let subrel m1 m2 =
      try
        InstMap.iter
          (fun i is ->
            let js = succs i m2 in
            if not (InstSet.subset is js) then raise Exit)
          m1 ;
        true
      with Exit -> false

  open Printf

  let pp_instance i = sprintf "{P%i:%02i}" i.S.A.proc i.S.A.program_order_index

  let pp_rel chan m =
    InstMap.iter
      (fun i js ->
        fprintf chan "%s ->" (pp_instance i) ;
        InstSet.iter
          (fun j -> fprintf chan " %s" (pp_instance j))
          js ;
        fprintf chan "\n")
      m

  (* fold f over pairs of distinct elements *)
  let rec fold_pairs f xs k = match xs with
  | [] -> k
  | x::xs ->
      List.fold_left
        (fun k y -> f x y (f y x k))
        (fold_pairs f xs k) xs

  let build rf evts =
    let open S in
    (* Build a map from (instruction) instances to events of that instance *)
    let m =
      E.EventSet.fold
        (fun e m -> match e.E.iiid with
        | E.IdInit|E.IdSpurious -> m
        | E.IdSome i ->
            InstMap.add i
              (E.EventSet.add e (InstMap.safe_find E.EventSet.empty i m)) m)
        evts InstMap.empty in
    let is =  InstMap.fold (fun i _ k -> i::k) m  [] in
    (* Utilities *)
    let inst2evts i = try InstMap.find i m with Not_found -> assert false in

    (* lift rf to instances *)
    let rf_rel =
      E.EventRel.fold
        (fun (w,r) m -> match r.E.iiid with
        | E.IdInit|E.IdSpurious -> assert false
        | E.IdSome ir -> match w.E.iiid with
          | E.IdInit|E.IdSpurious -> m
          | E.IdSome iw -> add_succ ir iw m)
        rf InstMap.empty in

    if dbg then eprintf "RF-REG:\n%a\n" pp_rel rf_rel ;

    let same_instr i1 i2 = i1.A.inst == i2.A.inst in

    let matches m is js =
      let ok i j = InstSet.mem i (succs j m) in
      InstSet.for_all
        (fun i -> InstSet.exists (ok i) js)
        is in
    let step m =
      InstMap.fold
        (fun i js k ->
          let rf_is = succs i rf_rel in
          InstSet.fold
            (fun j k ->
              let rf_js = succs j rf_rel in
              if matches m rf_is rf_js && matches m rf_js rf_is
              then add_succ i j k
              else k)
            js k)
        m InstMap.empty in
    let rec fix m =
      if dbg then eprintf "**FIX\n%a\n" pp_rel m ;
      let next = step m in
      if subrel m next then m
      else fix next in

    if dbg then eprintf "Instances: %s\n"
      (String.concat " " (List.map pp_instance is)) ;

    let m0 =
      fold_pairs
        (fun i j k ->
          if same_instr i j then add_succ i j k
          else k)
        is InstMap.empty in
    let equiv = fix m0 in
    let equiv =
      InstMap.fold
        (fun i js k ->
          let evts_i = inst2evts i in
          InstSet.fold
            (fun j k ->
              E.EventRel.cartesian evts_i (inst2evts j)::k)
            js k)
        equiv [] in
    E.EventRel.unions equiv
end
