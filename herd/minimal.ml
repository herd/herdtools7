(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** A really generic minimal model *)

open Printf

module type Cfg = sig
  val uniproc : bool
  include Model.Config
end


module Make (O:Cfg) (S:Sem.Semantics)
    =
  struct
    module S = S
    module E = S.E
    module A = S.A
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)

    let debug_proc chan p = fprintf chan "%i" p
    let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
    let debug_set chan s =
      output_char chan '{' ;
      E.EventSet.pp chan "," debug_event s ;
      output_char chan '}'

    let debug_events = debug_set

    let debug_rel chan r =
      E.EventRel.pp chan ","
        (fun chan (e1,e2) -> fprintf chan "%a -> %a"
            debug_event e1 debug_event e2)
        r

(*****************)
(* Configuration *)
(*****************)
    let uniproc =
      let open Model in
      if O.uniproc then match O.through with
      | ThroughInvalid|ThroughNone -> true
      | ThroughAll -> false
      else false

    let show_poloc = S.O.PC.showpoloc
    let show_fr = S.O.PC.showfr

(***********************************************)
(* Compute various relations (eg dependencies) *)
(***********************************************)

    let is_that_fence b e = match E.barrier_of e with
    | Some a -> a = b.S.barrier
    | None -> false

    let make_procrels conc =
      let is_isync = match S.isync with
      | None -> fun _ -> false
      | Some b -> is_that_fence b in
      MU.make_procrels is_isync conc

    let make_barrier b po =
      let r1 = S.restrict E.is_mem (is_that_fence b) po
      and r2 = S.restrict (is_that_fence b) E.is_mem po in
      b.S.pp,S.rt (S.seq r1 r2)

    let all_barriers conc pr =
      List.fold_right
        (fun b k -> make_barrier b conc.S.po::k)
        S.barriers
        (match S.isync with
        | None -> []
        | Some b ->
            let pp, r = make_barrier b conc.S.po in
            [pp,S.rt (E.EventRel.diff r pr.S.ctrlisync)])

(****************)
(* Model proper *)
(****************)
        
    let pp_failure test conc legend vb_pp =
      if  O.debug && O.verbose > 1 then begin
        let module PP = Pretty.Make(S) in
        eprintf "%s\n%!" legend ;
        PP.show_legend test  legend conc vb_pp
      end

    let check_uniproc test conc pr fr ws =
      let com = E.EventRel.unions [pr.S.rf; fr;ws;]
      and pos = conc.S.pos in
      let r = E.EventRel.union pos com in
      let ok = E.EventRel.is_acyclic r in
      if not ok then begin
        pp_failure
          test conc
          (sprintf "%s: Uniproc violation" test.Test.name.Name.name)
          [("co",S.rt ws); ("fr",fr); ("pos",S.rt conc.S.pos)]
      end ;
      ok



    let pp_isync = function
      | None -> "??"
      | Some b -> b.S.pp

    let check_event_structure  test conc kont res =
      let pr = make_procrels conc in
      let pp_relns =
        lazy begin
          (* Dependencies *)
          ("data",pr.S.data)::
          ("addr",pr.S.addr)::
          ("ctrl",E.EventRel.diff pr.S.ctrl pr.S.ctrlisync)::
          ("ctrl" ^ pp_isync S.isync,pr.S.ctrlisync)::
          ("pos",
          (if show_poloc then
            S.rt conc.S.pos else E.EventRel.empty))::          
          all_barriers conc pr
        end  in
      let proc_ws ws0 res =
        let ws = E.EventRel.transitive_closure ws0 in
        let fr = U.make_fr conc ws in
        let pp_relns =
          lazy begin
            ("fr",(if show_fr then fr else E.EventRel.empty))::
            ("co",ws0)::
            Lazy.force pp_relns
          end in
        (* LM: uniproc check controled by option *)
        if 
          not uniproc ||          
          check_uniproc test conc pr fr ws
        then
	  kont conc conc.S.fs pp_relns None res
        else res in
      U.apply_process_co test conc proc_ws res
  end
