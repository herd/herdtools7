(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module Make(O:Model.Config) (S:SemExtra.S) = struct
  module E = S.E
  module U = MemUtils.Make(S)

(*******************************************)
(* Complete re-computation of dependencies *)
(*******************************************)
  let evt_relevant x =  E.is_mem x || E.is_commit x || E.is_barrier x

  let make_procrels is_isync conc =
      let iico = conc.S.str.E.intra_causality_data
      and po = U.po_iico conc.S.str
      and rf_regs = U.make_rf_regs conc in

      let dd = S.tr (E.EventRel.union rf_regs iico) in
      let data_dep = S.restrict E.is_mem evt_relevant dd in
      let addr_dep =
(* Address deps are (1) dd to loads, (2) non-data deps to stores *)
        let r1 =
          E.EventRel.filter (fun (_,e2) -> E.is_mem_load e2) data_dep in

        let is_data (e1,e2) = 
          if E.is_reg_load_any e1 && E.is_mem_store e2 then begin
          (* See comments in event.ml, definition of same_subst_value *)
            E.same_subst_value e1 e2
          (* JW: I removed the "assert false" in the following line
             because the assertion fails if a single instruction
             gives rise to multiple memory accesses (as is the
             case for a failed CAS in C++11) *)
	  end else (*assert false*) false in
        let r2 =
          E.EventRel.sequence
            (S.restrict E.is_mem E.is_reg_load_any dd)
            (E.EventRel.filter
               (fun (_,e2 as p) -> E.is_mem_store e2 && not (is_data p))
               iico) in
        E.EventRel.union r1 r2 in
      let ctrl =
        S.restrict E.is_commit evt_relevant po in
      let ctrl_dep =
        S.restrict E.is_mem_load (fun x -> evt_relevant x)
          (S.seq dd ctrl) in
      let data_dep = E.EventRel.diff data_dep addr_dep in
      let po =
        S.restrict evt_relevant evt_relevant po in
      let data_commit =
        S.restrict E.is_mem_load E.is_commit data_dep in
      let ctrlisync =
        let r1 = S.restrict E.is_mem_load is_isync ctrl_dep
        and r2 = S.restrict is_isync E.is_mem po in
        S.seq r1 r2 in
      let rf = U.make_rf conc in
      { S.addr=addr_dep; data=data_dep; ctrl=ctrl_dep;
        ctrlisync;
        data_commit;
        rf;}

    let pp_procrels pp_isync pr =
      let ctrl = 
        E.EventRel.diff pr.S.ctrl pr.S.ctrlisync in
      ["data",pr.S.data; "ctrl",ctrl; "addr",pr.S.addr;
       Printf.sprintf "ctrl%s" pp_isync,pr.S.ctrlisync;]

(***************************)
(* A few factorized checks *)
(***************************)
  open Model

  let pp_failure test conc legend vb_pp =
    if  O.debug then begin
      let module PP = Pretty.Make(S) in
      Printf.eprintf "%s\n%!" legend ;
      PP.show_legend test  legend conc vb_pp
    end

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
        (Printf.sprintf "%s: Uniproc violation" test.Test.name.Name.name)
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
         (Printf.sprintf "%s: Atomicity violation" test.Test.name.Name.name)
         ["co",S.rt co; "fr",fr;"r*/w*",conc.S.atomic_load_store;];
    r
end
