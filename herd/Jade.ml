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

(** Jade's model for PPC/ARM *)

open Printf	

module type Config =  sig
  val opt : Model.jade_opt
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:AllBarrier.S with type a = S.barrier)
    =
  struct

(****************************)
(* Convenient abbreviations *)
(****************************)
    open Model

    module S = S
    module A = S.A
    module E = S.E
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)
    module JU = JadeUtils.Make(O)(S)(B)

    let check_event_structure test conc kont res =
      let prb = JU.make_procrels conc in
      let pr = prb.JU.pr in
      let vb_pp = lazy (JU.vb_pp_procrels prb) in
      let rf = pr.S.rf in
      let rfe = U.ext rf in
(* Rf cumulativities *)
      let rf_cumul r =
        let a = S.seq rfe r in
        S.unions [a; S.seq r rfe; S.seq a rfe] in
(* All barriers *)
      let _sync = Lazy.force prb.JU.sync
      and _lwsync =
        E.EventRel.filter
          (fun (e1,e2) ->
            not (E.is_mem_store e1 && E.is_mem_load e2))
          (Lazy.force prb.JU.lwsync)
      and _eieio = S.doWW (Lazy.force prb.JU.eieio)  in
      let _dsb = Lazy.force prb.JU.dsb
      and _dmb = Lazy.force prb.JU.dmb in
      let _dsbst = S.doWW (Lazy.force prb.JU.dsbst) in
      let _dmbst = S.doWW (Lazy.force prb.JU.dmbst) in
      let light =
        if O.opt.jstrongst then
          S.unions [_lwsync; _eieio;]
        else
          S.unions [_lwsync;_eieio;_dsbst;_dmbst;] in
      let strong =
        if O.opt.jstrongst then
          S.unions [_sync;_dsb;_dmb;_dmbst;_dsbst]
        else
          S.unions [_sync;_dsb;_dmb;] in
      let fences = S.unions [light;strong;] in
      let fencesext = rf_cumul fences in
(* PPO *)
      let ppo =  JU.ppo test conc vb_pp pr in
      let ppoext  = rf_cumul ppo in
      let hb = S.unions [ppo;ppoext;fences;fencesext;] in
      let vb_pp =
        lazy begin
          ("ppo",S.rt ppo)::
          ("sync",S.rt _sync)::
          ("lwsync",S.rt _lwsync)::
          ("eieio",S.rt _eieio)::
          ("dsb",S.rt _dsb)::
          ("dmb",S.rt _dmb)::
          ("dsb.st",S.rt _dsbst)::
          ("dmb.st",S.rt _dmbst)::
           (if true || O.verbose > 0 then
             (fun k ->
               ("ghb",S.rt hb)::k)
           else
             (fun k -> k))
             (Lazy.force vb_pp) end in
      let ok_hb =
        StringSet.mem "thinair" O.skipchecks ||
        E.EventRel.is_acyclic hb in
      let ok_hb = MU.check_through ok_hb in
      if not ok_hb then begin
       MU.pp_failure
          test conc
          (sprintf "%s: ghb violation" test.Test.name.Name.name)
          (Lazy.force vb_pp) ;
        res
      end else
        let id = JU.id conc in
        let star r = S.union id (S.tr r) in
        let hbstar = star hb in
        let propbase =
          S.seq
            (S.unions [fences;fencesext;])
            hbstar in
(* For each coherence order.... *)
        let process_co co res =
(* Remaining communication relations *)
          let co = S.tr co in
          let fr = U.make_fr conc co in
(* Uniproc check *)
          let ok_uniproc =
            StringSet.mem "uniproc" O.skipchecks ||
            MU.check_uniproc test conc rf fr co in
          if not ok_uniproc then res
          else
            let coe = U.ext co and fre = U.ext fr in
            let chapo =
              S.unions
                [rfe;coe;fre;S.seq coe rfe;S.seq fre rfe;] in
            let comstar = S.union id chapo in
(* Prop *)
            let prop =
              S.union
                (S.doWW propbase)
                (S.seqs [comstar ; star propbase; strong ; hbstar;]) in
            let vb_pp =
              lazy begin
                ("co",S.rt co)::("fr",fr)::
                ("prop-base",S.rt propbase)::
                ("prop",S.rt prop)::
                Lazy.force vb_pp
              end in
            let ok_co =
              StringSet.mem "propagation" O.skipchecks ||
              E.EventRel.is_acyclic (S.union co prop)  in
            let ok_co =  MU.check_through ok_co in
            if not ok_co then begin
              MU.pp_failure
                test conc
                (sprintf "%s: acyclic(co U prop) violation"
                   test.Test.name.Name.name)
                (Lazy.force vb_pp) ;
              res
             end else
              let ok_mp =
                StringSet.mem "causality" O.skipchecks ||
                E.EventRel.is_irreflexive (S.seqs [fre;prop;hbstar;]) in
              let ok_mp = MU.check_through ok_mp in
              if not ok_mp then begin
                MU.pp_failure
                  test conc
                  (sprintf "%s: irreflexive(fre;prop;hb*) violation"
                     test.Test.name.Name.name)
                  (Lazy.force vb_pp) ;
                res
              end else
                kont conc conc.S.fs vb_pp None res in
        U.apply_process_co test  conc process_co res

  end
