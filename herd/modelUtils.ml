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

module Make(O:Model.Config) (S:SemExtra.S) = struct
  module E = S.E
  module U = MemUtils.Make(S)

  let memtag = O.variant Variant.MemTag

(*******************************************)
(* Complete re-computation of dependencies *)
(*******************************************)
  let evt_relevant x =
    E.is_mem x || E.is_commit x || E.is_barrier x || E.is_additional_mem x

  let is_mem_load_total e = E.is_mem_load e || E.is_additional_mem_load e
  let is_load_total e = E.is_load e || E.is_additional_mem_load e

  let make_procrels is_isync conc =
    let is_data_port =
      let data_ports = conc.S.str.E.data_ports in
      fun e -> E.EventSet.mem e data_ports in
    let iico = conc.S.str.E.intra_causality_data
    and po = U.po_iico conc.S.str
    and rf_regs = U.make_rf_regs conc in
    let iico_rmw = E.EventRel.inter conc.S.atomic_load_store iico
    and iico_regs =
      E.EventRel.restrict_rel
        (fun  e1 e2  -> not (evt_relevant e1 || evt_relevant e2)) iico in
    let dd_inside = S.tr (E.EventRel.union rf_regs iico_regs) in
    let dd_pre =
(* Most dependencies start with a mem load, a few with a mem store + ctrl
   RISCV *)
      S.seq
        (E.EventRel.union
           (E.EventRel.restrict_domain E.is_mem_store
              conc.S.str.E.intra_causality_control)
           (E.EventRel.restrict_domain is_mem_load_total iico))
        dd_inside in
    let success =
      if O.variant Variant.Success then
        S.seq
          (E.EventRel.restrict_domain
             (fun e1 -> E.EventSet.mem e1 conc.S.str.E.success_ports)
             dd_inside)
          (E.EventRel.restrict_codomain E.is_mem iico)
      else E.EventRel.empty in
    let data_dep =
(* Data deps are (1) dd to commits (2) data deps to stores *)
      let last_data =
        E.EventRel.restrict_rel
          (fun e1 e2 ->
            E.is_commit e2 ||
            (E.is_mem_store e2 && is_data_port e1)) iico in
      S.union
        (S.seq dd_pre last_data)
        (iico_rmw) (* Internal data dep of RMW's *)
    and addr_dep =
(* Address deps are (1) dd to loads, (2) non-data deps to stores, (3) extra
   abstract "memory" events (such as lock, unlock, etc. that have an address
   port *)
      let last_addr =
        E.EventRel.restrict_rel
          (fun e1 e2 ->
            E.is_mem_load e2 ||
            (E.is_mem_store e2 && not (is_data_port e1)) ||
            E.is_additional_mem e2)
           (* Patch: a better solution would be a direct iico from read address register to access *)
          (if memtag then E.EventRel.transitive_closure iico else iico) in
      S.seq dd_pre last_addr in
    let ctrl_one = (* For bcc: from commit to event by po *)
      S.restrict E.is_commit_bcc evt_relevant po
    and ctrl_two = (* For predicated instruction from commit to event by iico *)
      S.restrict E.is_commit_pred evt_relevant (U.iico conc.S.str)
    and ctrl_three = (* For structured if from event to event by instruction control *)
      S.restrict is_load_total evt_relevant conc.S.str.E.control in
    let ctrl =
      E.EventRel.union3 ctrl_one ctrl_two ctrl_three in
    let ctrl_dep =
      (* All dependencies, including to reg loads *)
      let dd = S.union3 dd_pre addr_dep data_dep in
      S.restrict
        (fun e -> is_mem_load_total e || E.is_mem_store e)
        evt_relevant
        (S.union (S.seq dd ctrl) ctrl_three) in
    let po =
      S.restrict evt_relevant evt_relevant po in
    let data_commit =
      E.EventRel.restrict_codomain E.is_commit data_dep in
    let data_dep =  E.EventRel.restrict_codomain E.is_mem data_dep in
    let ctrlisync =
      try
        let r1 = S.restrict is_mem_load_total is_isync ctrl_dep
        and r2 = S.restrict is_isync E.is_mem po in
        S.seq r1 r2
      with Misc.NoIsync -> S.E.EventRel.empty in
    let rf = U.make_rf conc in
    let amo =
      E.EventRel.filter
        (fun (r,w) -> E.po_eq r w)
        conc.S.atomic_load_store in
    { S.addr=addr_dep; data=data_dep; ctrl=ctrl_dep; depend=dd_pre;
      ctrlisync;
      data_commit;
      success;
      rf; amo;}

  let pp_procrels pp_isync pr =
    let pp =  ["data",pr.S.data; "addr",pr.S.addr;] in
    match pp_isync with
    | None -> ("ctrl",pr.S.ctrl)::pp
    | Some isync ->
        let ctrl =  E.EventRel.diff pr.S.ctrl pr.S.ctrlisync in
        (Printf.sprintf "ctrl%s" isync,pr.S.ctrlisync)::
        ("ctrl",ctrl)::pp


(***************************)
(* A few factorized checks *)
(***************************)
  open Model

  let pp test conc legend vb_pp =
    let module PP = Pretty.Make(S) in
    Printf.eprintf "%s\n%!" legend ;
    PP.show_legend test  legend conc vb_pp

  let pp_failure test conc legend vb_pp =
    if O.debug then pp test conc legend vb_pp

(* Through *)
  let check_through =
    match O.through with
    | ThroughAll|ThroughInvalid -> fun _ -> true
    | ThroughNone -> fun ok -> ok

(* Uniproc *)
  let check_uniproc test conc rf fr co =
    let rel = S.unions [fr;rf;co;conc.S.pos] in
    let r = E.EventRel.is_acyclic rel in
    if S.O.optace then assert r ;
    let r = let open Model in
    match O.through with
    | ThroughNone|ThroughInvalid ->  r
    | ThroughAll -> true in
    if not r then
      pp_failure
        test conc
        (Printf.sprintf "%s: Uniproc violation" test.Test_herd.name.Name.name)
        [("co",S.rt co); ("fr",fr); ("pos",S.rt conc.S.pos)] ;
    r

  let check_atom test conc fr co =
    let ws = U.collect_mem_stores conc.S.str in
    let r =
      E.EventRel.for_all
        (fun (r,w) ->
          assert (E.same_location r w) ;
          let loc =
            match E.location_of w with
            | Some loc -> loc | None  -> assert false in
          let ws = U.LocEnv.find loc ws in
          not
            (List.exists
               (fun w' ->
                 E.proc_of r <> E.proc_of w' &&
                 E.EventRel.mem (r,w') fr &&
                 E.EventRel.mem (w',w) co)
               ws))
        conc.S.atomic_load_store in
    let r = check_through r in
    if not r then
      pp_failure
        test conc
        (Printf.sprintf "%s: Atomicity violation" test.Test_herd.name.Name.name)
        ["co",S.rt co; "fr",fr;"r*/w*",conc.S.atomic_load_store;];
    r
end
