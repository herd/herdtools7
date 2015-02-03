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

(** X86 TSO model *)

open Printf	

module Make
    (O:Model.Config)
    (S:Sem.Semantics)
    (B:AllBarrier.S with type a = S.barrier)
    =
  struct

(****************************)
(* Convenient abbreviations *)
(****************************)

    module S = S
    module A = S.A
    module E = S.E
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)

    let is_that_fence b x =  match E.barrier_of x with
    | Some a -> B.a_to_b a = b
    | None -> false

    let is_relevant e = E.is_mem e || E.is_barrier e

    let check_event_structure test conc kont res =      
(* Po for TSO *)
      let po = S.restrict is_relevant is_relevant conc.S.po in
      let po_ghb =
        E.EventRel.filter
          (fun (e1,e2) ->
            (E.is_mem_load e1 && E.is_mem e2) ||
            (E.is_mem_store e1 && E.is_mem_store e2))
          po in
(* Barriers *)
      let tso_fence is_fence =
        let r1 = S.restrict E.is_mem_store is_fence po
        and r2 = S.restrict is_fence E.is_mem_load po in
        S.seq r1 r2 in
      let _mfence = tso_fence (is_that_fence B.MFENCE)
      and _sync = tso_fence (is_that_fence B.SYNC)
      and _dmb = tso_fence (is_that_fence B.DMB)
      and _dsb = tso_fence (is_that_fence B.DSB) in
      let strong = S.unions [_mfence;_sync;_dsb;_dmb;] in
(* Implicit barriers *)
      let implied =
        let open Misc in
        let r1 =
          S.restrict
            E.is_mem_store (E.is_mem_load &&& E.is_atomic) po
        and r2 =
          S.restrict
            (E.is_mem_store &&& E.is_atomic) E.is_mem_load po in
        S.union r1 r2 in
(* RF *)
      let rf = U.make_rf conc in
      let rfe = U.ext rf in
(* PP *)
      let vb_pp =
        lazy begin
          ("mfence",_mfence)::
          ("sync",_sync)::
          ("dsb",_dsb)::
          ("dmb",_dmb)::
          ("implied",implied)::
          []
        end in
      let tname = test.Test.name.Name.name in
      let process_co co res =
(* Remaining communication relations *)
          let co = S.tr co in
          let fr = U.make_fr conc co in
(* Uniproc check *)
          let ok_uniproc = MU.check_uniproc test conc rf fr co in
          if not ok_uniproc then res
          else
(* Atomicity check *)
            let ok_atom = MU.check_atom test conc fr co in
            if not ok_atom then res
(* GHB check *)
            else
              let ghb =
                S.unions [fr;rfe;co;po_ghb;strong;implied;] in
              let vb_pp =
                lazy begin
                  ("co",S.rt co)::
                  ("fr",fr)::
                  ("GHB",
                   if O.verbose > 0 then S.rt ghb else E.EventRel.empty)::
                  Lazy.force vb_pp
                end in
              let ok_ghb = E.EventRel.is_acyclic ghb in
              let ok_ghb = MU.check_through ok_ghb in
              if not ok_ghb then begin
                MU.pp_failure test conc
                  (sprintf "%s: GHB violation" tname)
                  (Lazy.force vb_pp) ;
                res
              end else
                kont conc conc.S.fs vb_pp None res in
      U.apply_process_co test  conc process_co res
  end
