(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Support for Jade's PPC/ARM model *)

open Printf

module Make(O:Model.Config) (S : SemExtra.S) (B:AllBarrier.S with type a = S.barrier) =
  struct

(* Usual prelude *)
    module S = S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)

(* Real work *)
    let evt_relevant x = E.is_mem x || E.is_commit x || E.is_barrier x

    type com = { co : S.event_rel; rf : S.event_rel; }

    type procrels =
        { pr : S.procrels;
          lwsync : S.event_rel Lazy.t;
          sync : S.event_rel Lazy.t;
          isync : S.event_rel Lazy.t;
          eieio : S.event_rel Lazy.t;
          isb : S.event_rel Lazy.t;
          dmb : S.event_rel Lazy.t;
          dsb : S.event_rel Lazy.t;
          dmbst : S.event_rel Lazy.t;
          dsbst : S.event_rel Lazy.t;
          mfence : S.event_rel Lazy.t;
          sfence : S.event_rel Lazy.t;
          lfence : S.event_rel Lazy.t;
	  membar_cta : S.event_rel Lazy.t;
	  membar_gl  : S.event_rel Lazy.t;
	  membar_sys : S.event_rel Lazy.t;
        }

    let is_that_fence b x =  match E.barrier_of x with
    | Some a -> B.a_to_b a = b
    | None -> false

    let sep_by_that_fence b po =
      let r1 =
        E.EventRel.restrict_domains E.is_mem (is_that_fence b) po
      and r2 =
        E.EventRel.restrict_domains (is_that_fence b) E.is_mem po in
      E.EventRel.sequence r1 r2

    let sep_by_that_fence_lazy b po =
      lazy (sep_by_that_fence b po)

    let is_rdw conc e1 e2 =
      assert (E.same_location e1 e2) ;
      E.is_mem_load e1 && E.is_mem_load e2 &&
      U.rext conc e2 &&
      not (U.same_source conc e1 e2)

    let is_detour conc e1 e2 =
      assert (E.same_location e1 e2) ;
      E.is_mem_store e1 && E.is_mem_load e2 &&
      U.rext conc e2 (* uniproc => e1 -co-> rf(e2) *)

    let id conc =
      let evts = E.EventSet.filter evt_relevant conc.S.str.E.events in
      E.EventRel.of_list (List.map (fun e -> e,e) (E.EventSet.elements evts))

    let is_isync e = is_that_fence B.ISYNC e || is_that_fence B.ISB e

    let make_procrels conc =
      let pr = MU.make_procrels is_isync conc in
      let po = conc.S.po in
      { pr;
        sync=sep_by_that_fence_lazy B.SYNC po;
        lwsync=sep_by_that_fence_lazy B.LWSYNC po;
        isync=sep_by_that_fence_lazy B.ISYNC po;
        eieio=sep_by_that_fence_lazy B.EIEIO po;
        dsb=sep_by_that_fence_lazy B.DSB po;
        dmb=sep_by_that_fence_lazy B.DMB po;
        dsbst=sep_by_that_fence_lazy B.DSBST po;
        dmbst=sep_by_that_fence_lazy B.DMBST po;
        isb=sep_by_that_fence_lazy B.ISB po;
        mfence=sep_by_that_fence_lazy B.MFENCE po;
        sfence=sep_by_that_fence_lazy B.SFENCE po;
        lfence=sep_by_that_fence_lazy B.LFENCE po;
	membar_cta = sep_by_that_fence_lazy B.MEMBAR_CTA po;
	membar_gl  = sep_by_that_fence_lazy B.MEMBAR_GL po;
	membar_sys = sep_by_that_fence_lazy B.MEMBAR_SYS po;
      }


    let vb_pp_procrels pr = MU.pp_procrels B.pp_isync pr.pr

(**************)
(* Direct ppo *)
(**************)

    let mem = E.EventRel.mem 

    let add_tag tag k = match k with
    | "" -> tag
    | _ -> sprintf "%s.%s" tag k

    let pp_rels test legend conc vb_pp ii ic ci cc =
      if O.debug && O.verbose > 1 then begin
        let all = S.unions [ii;ic;ci;cc;] in
        let vb_pp =
          E.EventRel.fold
            (fun p k ->
              let tag = "" in
              let tag = if mem p cc then add_tag "CC" tag else  tag in
              let tag = if mem p ci then add_tag "CI" tag else  tag in
              let tag = if mem p ic then add_tag "IC" tag else  tag in
              let tag = if mem p ii then add_tag "II" tag else  tag in
              (tag,E.EventRel.singleton p)::k)
            all (Lazy.force vb_pp) in
      eprintf "%s\n%!" legend ;
      let module  PP = Pretty.Make(S) in
      PP.show_legend test  legend conc vb_pp
      end

    let tr_ppo test conc vb_pp =
      let rec fix_rec k ii ic ci cc =
        pp_rels test
          (sprintf "%s: iteration %i" test.Test.name.Name.name k)
          conc vb_pp ii ic ci cc ;
        let ii1 = S.unions [ii; ci; S.seq ic ci; S.seq ii ii]
        and ic1 = S.unions [ii; ci; ic; cc; S.seq ic cc; S.seq ii ic;]
        and ci1 = S.unions [ci; S.seq ci ii; S.seq cc ci;]
        and cc1 = S.unions [ci;cc; S.seq ci ic; S.seq cc cc;] in
        if
          E.EventRel.compare ii ii1 = 0 &&
          E.EventRel.compare ic ic1 = 0 &&
          E.EventRel.compare ci ci1 = 0 &&
          E.EventRel.compare cc cc1 = 0
        then begin
          ii,ic,ci,cc
        end else fix_rec (k+1) ii1 ic1 ci1 cc1 in
      fix_rec 0

        
    let ppo test conc vb_pp pr =
      let dd =
        S.restrict E.is_mem E.is_mem (S.unions [pr.S.data;pr.S.addr;]) in
      let rfi = U.internal pr.S.rf in
      let rdw =
        E.EventRel.filter
          (fun (e1,e2) -> is_rdw conc e1 e2)
          conc.S.pos in
      let ctrlisync = S.restrict E.is_mem E.is_mem pr.S.ctrlisync in
      let ctrl = S.restrict E.is_mem E.is_mem pr.S.ctrl in
      let detour =
        E.EventRel.filter
          (fun (e1,e2) -> is_detour conc e1 e2)
          conc.S.pos in
      let addrpo =
        S.restrict E.is_mem_load E.is_mem (S.seq pr.S.addr conc.S.po) in
      let ii = S.unions [dd;rfi;rdw]
      and ic = E.EventRel.empty
      and ci = S.unions [ctrlisync;detour;]
      and cc = S.unions [dd;conc.S.pos;ctrl;addrpo;] in
      let ii,ic,_ci,_cc = tr_ppo test conc vb_pp ii ic ci cc in
      let ppoR = S.restrict E.is_mem_load E.is_mem_load ii
      and ppoW = S.restrict E.is_mem_load E.is_mem_store ic in
      S.union ppoR ppoW
  end
