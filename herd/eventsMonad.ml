(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

(** A monad for event structures *)

module type Config = sig
  val hexa : bool
  val debug : Debug_herd.t
  val variant : Variant.t -> bool
end

module Make (C:Config)
    (A:Arch_herd.S)
    (E:Event.S with module A = A and module Act.A = A) :
    (Monad.S with module A = A and module E = E
and type evt_struct = E.event_structure) =
  struct

    open Printf

    module A = A
    module E = E
    module V = A.V
    module VC =
      Valconstraint.Make
        (struct
          let hexa = C.hexa
          let debug = C.debug
          let keep_failed_as_undetermined = C.variant Variant.ASL_AArch64
          let old_solver = C.variant Variant.OldSolver
        end)
        (A)

    let dbg = C.debug.Debug_herd.mem
    let dbg_monad = C.debug.Debug_herd.monad
    let do_deps = C.variant Variant.Deps

(* LM Use lists for polymorphism.
   It is assumed that list elts are pairwise distinct.
 *)
    module Evt = struct
      type 'a elt = ('a * VC.cnstrnts * E.event_structure)
      type 'a t = 'a elt list

      let empty = []

      let singleton x = [x]

      let is_empty = function
        | [] -> true
        | _::_ -> false

      let as_singleton = function
        | [x] -> x
        | _ -> assert false

      let as_singleton_nospecul = fun (x,y) ->
        let x = as_singleton x in
        assert (y = None);
        x

      let add x s = x::s

      let map = List.map

      let fold f xs y0 =
        List.fold_left (fun x y -> f y x) y0 xs

      let foldfold f xs ys z0 =
        List.fold_left
          (fun z x -> List.fold_left (fun z y -> f x y z) z ys)
          z0 xs

      let union = (@)

      let elements (x:'a t) = x

      let rec check_same_value p x = match x with
      | [] -> true
      | elt::s ->
          p elt && check_same_value p s

      let wrap_check p x = match x with
      | [] -> assert false
      | elt::s ->
          assert(check_same_value (p elt) s); elt

      let rec combine_rec k xs ys = match xs,ys with
      | [],[] -> k
      | x::xs,y::ys -> combine_rec ((x,y)::k) xs ys
      | ([],_::_)|(_::_,[]) -> assert false (* By construction *)

      let combine xs ys = combine_rec [] xs ys

    end

(*
Monad type:
  + Argument is event identifier
  + Returned value is a pair, whose first element is
   concrete branch and second collects speculated branch.
   None means no speculation at all
*)


    type eid =
        { id : int ;  (* event identifier proper *)
          sub : int } (* identifier amongst one instruction instance *)

    let bump_eid { id; sub; } = { id=id+1; sub=sub+1; }

    type 'a t =
        eid -> eid * ('a Evt.t * 'a Evt.t option)

    let map_elt (f : 'a Evt.elt -> 'b Evt.elt) (m : 'a t) : 'b t =
     fun eid ->
      let eid', (evt, evt_spec) = m eid in
      (eid', (Evt.map f evt, Option.map (Evt.map f) evt_spec))

 (* Code monad slight differs as regardes agument *)
 (* Threading by instruction instance identifier and event id proper *)

    type 'a code =
        (int * int) -> (int * int) * ('a Evt.t * 'a Evt.t option)

    let zeroT : 'a t
        = (fun eiid_next -> (eiid_next, (Evt.empty, None)))

    let zerocodeT : 'a code
        = (fun eiid_next -> (eiid_next, (Evt.empty, None)))

    let unitT (v : 'a) : 'a t =
      fun eiid_next ->
        eiid_next, (Evt.singleton (v, [], E.empty_event_structure), None)

    let warnT msg (v : 'a) : 'a t =
      fun eiid_next ->
      eiid_next,
      (Evt.singleton (v, [VC.Warn msg], E.empty_event_structure), None)

    let failT (e:exn) (v : 'a) : 'a t =
      fun eiid_next ->
      eiid_next,
      (Evt.singleton (v, [VC.Failed e], E.empty_event_structure), None)

    let ignore _ = unitT ()

    let unitcodeT (v : 'a) : 'a code =
      fun eiid_next ->
        eiid_next, (Evt.singleton (v, [], E.empty_event_structure), None)

    let failcodeT (e:exn) (v : 'a)  : 'a code =
      fun eiid_next ->
      eiid_next,
      (Evt.singleton (v, [VC.Failed e], E.empty_event_structure), None)

    let warncodeT (e : string) (v : 'a)  : 'a code =
      fun eiid_next ->
      eiid_next,
      (Evt.singleton (v, [VC.Warn e], E.empty_event_structure), None)

  (* This very special combinator permits to get monad m's result,
   while postponing the usage of corresponding event structure.
   It proves convenient to express complex dependencies.
   Not compatible with speculation *)

    let delay_kont
        = fun tag (m:'a t) kont (eiid:eid) ->
          let eiid,(acts,specs) = m eiid in
          assert (specs=None) ;
          let eiid,acts =
            Evt.fold
              (fun (v,cls,es) (eiid,acts) ->
                if dbg_monad then begin
                    eprintf "Delay %s output is %a\n" tag E.debug_output es
                  end ;
               let delayed : 'a t =
                  fun eiid -> eiid,(Evt.singleton (v,[],es),None) in
                let eiid,(acts2,specs) = kont v delayed eiid in
                assert (specs=None) ;
                let acts =
                  Evt.fold
                    (fun (v,cls2,es) acts -> Evt.add (v,cls@cls2,es) acts)
                    acts2 acts in
                eiid,acts)
              acts (eiid,Evt.empty) in
          eiid,(acts,None)

    let delay (m:'a t) eiid =
      delay_kont "delay" m (fun v delayed -> unitT (v,delayed)) eiid

    let set_standard_input_output m eiid =
      let (eiid,(sact,sspec)) = m eiid in
      let set_std (v,cl,es) =
        let es =
          { es with
            E.input = None; data_input = None;
            output = None; ctrl_output = None; } in
        v,cl,es in
      let set_stds = Evt.map set_std in
      let sact = set_stds sact
      and sspec = Misc.map_opt set_stds sspec in
      eiid,(sact,sspec)

    let (=**=) = E.(=**=)
    let (=*$=) = E.(=*$=)
    let (=$$=) = E.(=$$=)
    let (=*$$=) = E.(=*$$=)
    let (+|+) = E.(+|+)
    let (=|=) = E.para_comp true

(* Bind the result *)
    let data_comp comp_str s f =
      (fun eiid ->
        let (eiid,(sact,sspec)) = s eiid in
        assert(sspec = None);
        let eiid,(acts,specs) =
          Evt.fold (fun (v1, vcl1, es1) (eiid,(acts,specs)) ->
            let eiid,(b_acts,b_specs) = f v1 eiid in
            let acts =
              Evt.fold (fun (v2,vcl2,es2) acts ->
                match comp_str es1 es2 with
                | None -> acts
                | Some es -> Evt.add (v2,vcl2@vcl1,es) acts)
                b_acts acts in
            let specs = match b_specs with
            | None -> specs
            | Some b_specs ->
                Evt.fold
                  (fun (v2,vcl2,es2) specs ->
                    match comp_str (E.do_speculate es1) es2 with
                    | None -> specs
                    | Some es -> Evt.add (v2,vcl2@vcl2,es) specs)
                  b_specs specs in
            eiid,(acts,specs))
            sact (eiid,(Evt.empty,Evt.empty)) in
        let specs =
          if Evt.is_empty specs then None else Some specs in
        eiid,(acts,specs))

    let (>>=) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (=*$=) s f

    let data_input_next s f = data_comp E.data_input_next s f

    let data_input_union s f = data_comp E.data_input_union s f

    let (>>==) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (=$$=) s f

    let data_output_union : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (E.data_output_union) s f

    let asl_data s f = data_comp E.data_po_seq s f

    let (>>*=) : 'a t -> ('a -> 'b t) -> ('b) t
      = fun s f -> data_comp (=**=) s f

    let control_input_union s f = data_comp E.control_input_union s f
    let control_input_next s f = data_comp E.control_input_next s f

    let (>>*==) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (=*$$=) s f

    let bind_control_set_data_input_first s f = data_comp E.bind_control_set_data_input_first s f

    let bind_ctrl_avoid ma s f = fun eiid ->
      let eiid,(mact,spec) = ma eiid in
      assert(spec = None) ;
      let _,cl,es = Evt.as_singleton mact in
      assert (cl=[]) ;
      data_comp (E.bind_ctrl_avoid es.E.events) s f eiid

    let bind_ctrl_seq_data s f = data_comp E.bind_ctrl_sequence_data s f

    let asl_ctrl s f = data_comp E.bind_ctrl_sequence_data_po s f

    let bind_data_to_minimals s f =  data_comp E.data_to_minimals s f

    let bind_data_to_output s f = data_comp E.data_to_output s f

(* Triple composition *)
    let comp_comp comp_str m1 m2 m3 eiid =
      let eiid,(acts1,spec1) = m1 eiid in
      let eiid,(acts2,spec2) = m2 eiid in
      assert (spec1=None); assert (spec2=None);
      let eiid,acts =
        Evt.foldfold
          (fun (v1,cl1,es1) (v2,cl2,es2) (eiid,acts) ->
            let eiid,(acts3,spec3) = m3 v1 v2 eiid in
            assert (spec3=None) ;
            let acts =
              Evt.fold
                (fun (v3,cl3,es3) acts ->
                  let es = comp_str es1 es2 es3 in
                  Evt.add (v3,cl1@cl2@cl3,es) acts)
                acts3 acts in
            eiid,acts)
          acts1 acts2 (eiid,Evt.empty) in
      eiid,(acts,None)

    let bind_ctrldata_data m1 m2 m3 eiid =
      comp_comp E.bind_ctrldata_data m1 m2 m3 eiid

    let bind_ctrldata m1 m3 =
      bind_ctrldata_data m1 (unitT ()) (fun a () -> m3 a)

    let (>>**==) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp E.bind_ctrldata_first_outputs s f

    let bind_ctrl_first_outputs s f = data_comp E.bind_ctrl_first_outputs s f

    let bind_order s f = data_comp E.bind_order s f

(* Ad-hoc short-circuit *)
    let short p1 p2 m =
      fun eiid ->
      let eiid,(acts,specs) = m eiid in
      let acts =
        Evt.map
          (fun (v,cls,es) ->
             let data =
               let data =
                 E.EventRel.filter
                   (fun (e1,e2) -> p1 e1 && p2 e2)
                   (E.EventRel.cartesian es.E.events es.E.events) in
               E.EventRel.union es.E.intra_causality_data data in
            v,cls,{ es with E.intra_causality_data=data; })
      acts in
       eiid,(acts,specs)

(* Ad-hoc upOne *)
    let upOneRW p m =
      fun eiid ->
        let eiid,(acts,specs) = m eiid in
        let acts =
          Evt.map
            (fun (v,cls,es) ->
              let data = es.E.intra_causality_data in
              let data =
                E.EventRel.fold
                  (fun (e1,e0) k ->
                    if p e1 && E.is_load e1 then
                      E.EventRel.fold
                        (fun (f0,e2) k ->
                          if
                            E.event_equal e0 f0 &&
                            p e2 && E.is_store e2
                          then
                            E.EventRel.add
                              (e1,e2)
                              (E.EventRel.remove (e0,e2) k)
                          else k)
                        data k
                    else k)
                  data data in
              v,cls,{ es with E.intra_causality_data=data; })
            acts in
        eiid,(acts,specs)

(* Exchange combination *)
    let exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'c t) ->  ('b * 'c) t
        = fun rx ry wx wy ->
          fun eiid ->
            let eiid,rxact = rx eiid in
            let eiid,ryact = ry eiid in
            let (vrx,vclrx,esrx) = Evt.as_singleton_nospecul rxact
            and (vry,vclry,esry) = Evt.as_singleton_nospecul ryact in
            let eiid,wxact = wx vry eiid in
            let eiid,wyact = wy vrx eiid in
            let vwx,vclwx,eswx = Evt.as_singleton_nospecul wxact
            and vwy,vclwy,eswy = Evt.as_singleton_nospecul wyact in
            let es =
              E.exch esrx esry eswx eswy in
            eiid,(Evt.singleton((vwx,vwy),vclrx@vclry@vclwx@vclwy,es),None)

(* Exchange combination *)
(* NB: first boolean -> physical memory access *)
    let swp_or_amo : bool -> A.V.op_t option -> ('loc t) ->
      ('loc -> V.v t) -> V.v t -> ('loc -> V.v -> unit t) -> (V.v -> unit t)
        -> unit t  = fun is_phy op rloc rmem rreg wmem wreg ->
          fun eiid ->
        let eiid,(locm,spec) = rloc eiid in
        assert (spec=None) ;
        let eiid,acts =
          Evt.fold
            (fun (loc,vlcloc,esloc) (eiid,acts) ->
              let eiid,expm = rreg eiid in
              let  (v,vclexp,esexp) = Evt.as_singleton_nospecul expm in
              let eiid,rmemm = rmem loc eiid in
              let r = match op with None -> v | Some _ -> V.fresh_var () in
              let eiid,wmemm = wmem loc r eiid in
              let w,vclrmem,esrmem =  Evt.as_singleton_nospecul rmemm
              and (),vclwmem,eswmem = Evt.as_singleton_nospecul wmemm in
              let vlop =
                match op with
                | None -> Misc.identity
                | Some op -> fun k -> VC.Assign (r,VC.Binop (op,w,v))::k in
              let eiid,wreg = wreg w eiid in
              let (),vclwreg,eswreg = Evt.as_singleton_nospecul wreg in
              let es =
                E.swp_or_amo is_phy op esloc esrmem esexp eswmem eswreg in
              let act = (),vlop (vlcloc@vclexp@vclrmem@vclwmem@vclwreg),es in
              eiid,Evt.add act acts)
            locm (eiid,Evt.empty) in
        eiid,(acts,None)

    let swp is_phy rloc rmem rreg wmem wreg =
      swp_or_amo is_phy None rloc rmem rreg wmem wreg
    let amo_strict is_phy op rloc rmem rreg wmem wreg =
      swp_or_amo is_phy (Some op) rloc rmem rreg wmem wreg

(* linux exchange *)
    let linux_exch : 'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t = fun rloc rexpr rmem wmem ->
      fun eiid ->
        let eiid,locm = rloc eiid in
        let eiid,expm = rexpr eiid in
        let (loc,vlcloc,esloc) =  Evt.as_singleton_nospecul locm
        and (v,vclexp,esexp) = Evt.as_singleton_nospecul expm in
        let eiid,rmemm = rmem loc eiid in
        let eiid,wmemm = wmem loc v eiid in
        let w,vclrmem,esrmem =  Evt.as_singleton_nospecul rmemm
        and (),vclwmem,eswmem = Evt.as_singleton_nospecul wmemm in
        let es = E.linux_exch esexp esloc esrmem eswmem in
        eiid,
        (Evt.singleton (w,vlcloc@vclexp@vclrmem@vclwmem,es),None)

(* Amo, similar to exchange *)
    let amo : A.V.op_t -> 'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t = fun op rloc rexpr rmem wmem ->
      fun eiid ->
        let eiid,locm = rloc eiid in
        let eiid,expm = rexpr eiid in
        let (loc,vlcloc,esloc) =  Evt.as_singleton_nospecul locm
        and (v,vclexp,esexp) = Evt.as_singleton_nospecul expm in
        let eiid,rmemm = rmem loc eiid in
        let r = V.fresh_var () in
        let eiid,wmemm = wmem loc r eiid in
        let w,vclrmem,esrmem =  Evt.as_singleton_nospecul rmemm
        and (),vclwmem,eswmem = Evt.as_singleton_nospecul wmemm in
        let vlop = VC.Assign (r,VC.Binop (op,w,v)) in
        let es = E.amo esexp esloc esrmem eswmem in
        eiid,
        (Evt.singleton (w,vlop::vlcloc@vclexp@vclrmem@vclwmem,es), None)

(* Linux (successful) compexchange *)
    let linux_cmpexch_ok :
        'loc t -> 'v t -> 'v t -> ('loc -> 'v t) ->
          ('loc -> 'v -> unit t) -> ('v -> 'v -> unit t) -> 'v t =
            fun rloc rold rnew rmem wmem req eiid ->
              let eiid,locm = rloc eiid in (* read location *)
              let eiid,oldm = rold eiid in (* read old value *)
              let eiid,newm = rnew eiid in (* read new value *)
              let (loc,vlcloc,esloc) =  Evt.as_singleton_nospecul locm
              and (oldv,vlcold,esold) = Evt.as_singleton_nospecul oldm
              and (newv,vlcnew,esnew) = Evt.as_singleton_nospecul newm in
              let eiid,rmemm = rmem loc eiid in
              let eiid,wmemm = wmem loc newv eiid in
              let w,vclrmem,esrmem =  Evt.as_singleton_nospecul rmemm
              and (),vclwmem,eswmem = Evt.as_singleton_nospecul wmemm in
              let es = E.linux_cmpexch_ok esloc esold esnew esrmem eswmem in
              let eiid,eqm = req oldv w eiid in
              let (),vcleq,eseq =  Evt.as_singleton_nospecul eqm in
              assert (E.is_empty_event_structure eseq) ;
              eiid,
              (Evt.singleton
                 (w,vcleq@vlcloc@vlcold@vlcnew@vclrmem@vclwmem,es),
               None)


    let linux_cmpexch_no :
        'loc t -> 'v t -> ('loc -> 'v t) ->
          ('v -> 'v -> unit t) -> 'v t =
            fun rloc rold rmem rneq eiid ->
              let eiid,locm = rloc eiid in (* read location *)
              let eiid,oldm = rold eiid in (* read old value *)
              let (loc,vlcloc,esloc) =  Evt.as_singleton_nospecul locm
              and (oldv,vlcold,esold) = Evt.as_singleton_nospecul oldm in
              let eiid,rmemm = rmem loc eiid in
              let w,vclrmem,esrmem =  Evt.as_singleton_nospecul rmemm in
              let es = E.linux_cmpexch_no esloc esold esrmem in
              let eiid,eqm = rneq oldv w eiid in
              let (),vcleq,_ =  Evt.as_singleton_nospecul eqm in
              eiid,
              (Evt.singleton
                 (w,vcleq@vlcloc@vlcold@vclrmem,es),
               None)


(**************)
(* Add unless *)
(**************)

(* Success *)
    let linux_add_unless_ok rloc ra ru rmem wmem neq add ropt eiid =
      let eiid,locm = rloc eiid in (* read location *)
      let eiid,am = ra eiid in     (* read added value *)
      let eiid,um = ru eiid in     (* limit *)
      let vloc,clloc,esloc =  Evt.as_singleton_nospecul locm
      and va,cla,esa = Evt.as_singleton_nospecul am
      and vu,clu,esu = Evt.as_singleton_nospecul um in
      let eiid,rmem = rmem vloc eiid in
      let vv,clrmem,esrmem = Evt.as_singleton_nospecul rmem in
      let eiid,addm = add vv va eiid in
      let vadd,cladd,esadd = Evt.as_singleton_nospecul addm in
      assert (E.is_empty_event_structure esadd) ;
      let eiid,wmem = wmem vloc vadd eiid in
      let _,clwmem,eswmem = Evt.as_singleton_nospecul wmem in
      let eiid,eqm = neq vv vu eiid in
      let (),cleq,eseq = Evt.as_singleton_nospecul eqm in
      assert (E.is_empty_event_structure eseq) ;
      let es = E.linux_add_unless_ok esloc esa esu esrmem eswmem (Misc.is_some ropt) in
      let r = match ropt with Some r -> r | None -> vv in
      eiid,
      (Evt.singleton
         (r,cleq@cladd@clwmem@clrmem@clloc@cla@clu,es),
       None)

(* Failure *)
    let linux_add_unless_no rloc ru rmem eq ropt eiid =
      let eiid,locm = rloc eiid in (* read location *)
      let eiid,um = ru eiid in     (* limit *)
      let vloc,clloc,esloc =  Evt.as_singleton_nospecul locm
      and vu,clu,esu = Evt.as_singleton_nospecul um in
      let eiid,rmem = rmem vloc eiid in
      let vv,clrmem,esrmem = Evt.as_singleton_nospecul rmem in
      let eiid,eqm = eq vv vu eiid in
      let (),cleq,eseq = Evt.as_singleton_nospecul eqm in
      assert (E.is_empty_event_structure eseq) ;
      let es = E.linux_add_unless_no esloc esu esrmem (Misc.is_some ropt) in
      let r = match ropt with Some r -> r | None -> vv in
      eiid, (Evt.singleton (r,cleq@clrmem@clloc@clu,es), None)

(* Store conditional, tricky dependencies *)
    let riscv_sc success read_res read_data read_addr
        cancel_res write_result write_mem eiid =
      let eiid,read_res =  read_res eiid in
      let eiid,read_data = read_data eiid in
      let eiid,(read_addr,spec) = read_addr eiid in
      assert (spec=None) ;
      let resa,cl_resa,es_resa =  Evt.as_singleton_nospecul read_res
      and data,cl_data,es_data =  Evt.as_singleton_nospecul read_data in
      let eiid,acts =
        Evt.fold
          (fun (addr,cl_addr,es_addr) (eiid,acts) ->
            let eiid,cancel_res = cancel_res eiid in
            let eiid,write_result = write_result eiid in
            let eiid,write_mem = write_mem addr resa data eiid in
            let (),cl_wres,es_wres = Evt.as_singleton_nospecul cancel_res
            and (),cl_wresult,es_wresult =
              Evt.as_singleton_nospecul write_result
            and r,cl_wmem,es_wmem =  Evt.as_singleton_nospecul write_mem in
            let es =
              E.riscv_sc success
                es_resa es_data es_addr es_wres es_wresult es_wmem in
            eiid,
            Evt.add
              (r,cl_resa@cl_data@cl_addr@cl_wres@cl_wresult@cl_wmem,es)
              acts)
          read_addr (eiid,Evt.empty) in
      eiid,(acts,None)

(* AArch64 failed cas *)
    let do_aarch64_cas_no
        (is_physical:bool)
        (add_ctrl:bool)
        (read_rn:'loc t) (read_rs:'v t)
        (write_rs:'v-> unit t)
        (read_mem: 'loc -> 'v t)
        (branch: 'loc -> unit t)
        (rne: 'v -> 'v -> unit t)
        eiid =
      let eiid,read_rn = read_rn eiid in
      let eiid,read_rs = read_rs eiid in
      let cv,cl_cv,es_rs = Evt.as_singleton_nospecul read_rs in
      let acts_rn,spec = read_rn in
      assert (Misc.is_none spec) ;
      let eiid,acts =
        Evt.fold
          (fun  (a,cl_a,es_rn) (eiid,acts) ->
            let eiid,read_mem = read_mem a eiid in
            let ov,cl_rm,es_rm = Evt.as_singleton_nospecul read_mem in
            let eiid,write_rs = write_rs ov eiid in
            let (),cl_wrs,es_wrs = Evt.as_singleton_nospecul write_rs in
            let eiid,branch = branch a eiid in
            let (),cl_br,es_br =  Evt.as_singleton_nospecul branch in
            let eiid,nem = rne ov cv eiid in
            let (),cl_ne,eseq =  Evt.as_singleton_nospecul nem in
            assert (E.is_empty_event_structure eseq) ;
            let es =
              E.aarch64_cas_no is_physical add_ctrl es_rn es_rs es_wrs es_rm es_br in
            let cls = cl_a@cl_cv@cl_rm@cl_wrs@cl_ne@cl_br  in
            eiid,Evt.add ((),cls,es) acts)
          acts_rn (eiid,Evt.empty) in
      eiid,(acts, None)

(* AArch64 successful cas *)
    let do_aarch64_cas_ok
        (is_physical:bool) (prov_data: [`DataFromRRs | `DataFromRx])
        (read_rn:'loc t) (read_rs:'v t) (read_rt: 'v t)
        (write_rs:'v-> unit t)
        (read_mem: 'loc -> 'v t) (write_mem: 'loc -> 'v -> unit t)
        (branch: 'loc -> unit t)
        (req: 'v -> 'v -> unit t)
        eiid =
      let eiid,read_rn = read_rn eiid in
      let eiid,read_rs = read_rs eiid in
      let eiid,read_rt = read_rt eiid in
      let cv,cl_cv,es_rs = Evt.as_singleton_nospecul read_rs
      and nv,cl_nv,es_rt = Evt.as_singleton_nospecul read_rt in
      let acts_rn,spec = read_rn in
      assert (spec=None) ;
      let eiid,acts =
        Evt.fold
          (fun (a,cl_a,es_rn) (eiid,acts) ->
            let eiid,read_mem = read_mem a eiid in
            let eiid,write_mem = write_mem a nv eiid in
            let ov,cl_rm,es_rm = Evt.as_singleton_nospecul read_mem
            and (),cl_wm,es_wm= Evt.as_singleton_nospecul write_mem in
            let eiid,write_rs = write_rs ov eiid in
            let (),cl_wrs,es_wrs = Evt.as_singleton_nospecul write_rs in
            let eiid,branch = branch a eiid in
            let (),cl_br,es_br =  Evt.as_singleton_nospecul branch in
            let eiid,eqm = req ov cv eiid in
            let (),cl_eq,eseq =  Evt.as_singleton_nospecul eqm in
            assert (E.is_empty_event_structure eseq) ;
            let es =
              E.aarch64_cas_ok is_physical prov_data es_rn es_rs es_rt es_wrs es_rm es_wm es_br in
            let cls = cl_a@cl_cv@cl_nv@cl_rm@cl_wm@cl_wrs@cl_br@cl_eq  in
            eiid,Evt.add ((),cls,es) acts)
          acts_rn (eiid,Evt.empty) in
      eiid,(acts, None)

(* Temporary morello variation of CAS *)
    let aarch64_cas_ok_morello
        (read_rn:'loc t) (read_rt: 'v t)
        (read_mem: 'v t) (write_mem: 'loc -> 'v -> unit t)
        eiid =
      let eiid,read_rn = read_rn eiid in
      let eiid,read_rt = read_rt eiid in
      let a,cl_a,es_rn = Evt.as_singleton_nospecul read_rn
      and nv,cl_nv,es_rt = Evt.as_singleton_nospecul read_rt in
      let eiid,read_mem = read_mem eiid in
      let eiid,write_mem = write_mem a nv eiid in
      let _,cl_rm,es_rm = Evt.as_singleton_nospecul read_mem
      and (),cl_wm,es_wm= Evt.as_singleton_nospecul write_mem in
      let es =
        E.aarch64_cas_ok_morello es_rn es_rt es_rm es_wm in
      let cls = cl_a@cl_nv@cl_rm@cl_wm in
      eiid,(Evt.singleton ((),cls,es), None)

    let has_no_spec (x,y) = assert(y=None); x

(* Simple alternative *)
    let altT    : 'a t -> 'a t -> 'a t =
      fun m1 m2 eiid ->
        let (eiid, act1) = m1 eiid in
        let act1 = has_no_spec act1 in
        let (eiid, act2) = m2 eiid in
        let act2 = has_no_spec act2 in
        let un =  Evt.union act1 act2 in
        (eiid, (un,None))

    let aarch64_or_riscv_store_conditional
          must_fail read_res read_data read_addr
          cancel_res write_result write_mem =
      let m_fail = riscv_sc false
           read_res read_data read_addr cancel_res
           (write_result V.one)
           (fun _a _resa _v -> unitT ()) in
      if must_fail then m_fail
      else
        altT m_fail
          (riscv_sc true
             read_res read_data read_addr cancel_res
             (write_result V.zero)
             write_mem)

    let aarch64_cas_ok (is_physical: bool) (read_rn: 'loc t) (read_rs: 'v t)
        (read_rt: 'v t) (write_rs: 'v -> unit t) (read_mem: 'loc -> 'v t)
        (write_mem: 'loc -> 'v -> unit t) (branch: 'loc -> unit t) (req: 'v -> 'v -> unit t) =
      let do_ prov_data =
        do_aarch64_cas_ok is_physical prov_data read_rn read_rs read_rt
          write_rs read_mem write_mem branch req
      in
      altT (do_ `DataFromRRs) (do_ `DataFromRx)

    let aarch64_cas_no (is_physical:bool) (read_rn:'loc t) (read_rs:'v t)
        (write_rs:'v-> unit t) (read_mem: 'loc -> 'v t) (branch: 'loc -> unit t)
        (rne: 'v -> 'v -> unit t) =
      let do_ add_ctrl =
        do_aarch64_cas_no is_physical add_ctrl read_rn read_rs write_rs
          read_mem branch rne
      in
      altT (do_ true) (do_ false)

    (* RISCV store conditional may always succeed? *)
    let riscv_store_conditional = aarch64_or_riscv_store_conditional false

    let aarch64_store_conditional = aarch64_or_riscv_store_conditional

(* stu combinator *)
    let stu : 'a t -> 'b t -> ('a -> unit t) -> (('a * 'b) -> unit t) -> unit t
        = fun rD rEA wEA wM  ->
          fun eiid ->
            let eiid,rd = rD eiid in
            let eiid,rea = rEA eiid in
            let (vrd,vclrd,esrd) = Evt.as_singleton_nospecul rd
            and (vrea,vclrea,esrea) = Evt.as_singleton_nospecul rea in
            let eiid,wea = wEA vrea eiid in
            let eiid,wm = wM (vrd,vrea) eiid in
            let (_vwea,vclwea,eswea) = Evt.as_singleton_nospecul wea
            and (_vwm,vclwm,eswm) = Evt.as_singleton_nospecul wm in
            let es = E.stu esrd esrea eswea eswm in
            eiid,(Evt.singleton ((),vclrd@vclrea@vclwea@vclwm,es), None)

    let lift_combi_opt f c (v1,vcl1,es1) (v2,vcl2,es2) =
      match c es1 es2 with
      | None -> None
      | Some es -> Some (f v1 v2,vcl2@vcl1,es)

    let lift_combi f c elt1 elt2 acc =
      match lift_combi_opt f c elt1 elt2 with
      | None -> acc
      | Some s3 -> Evt.add s3 acc

    let fold2_ess f ess1 ess2 =
      Evt.fold
        (fun es1 k ->
          Evt.fold (fun es2 k -> f es1 es2 k) ess2 k)
        ess1 Evt.empty

    let fold3_ess f essa essb essc =
      let essbc = Evt.combine essb essc in
      Evt.fold
        (fun esa k ->
          List.fold_left
            (fun k (esb,esc) -> f esa esb esc k)
            k essbc)
        essa Evt.empty

    let lift_combis f c ess1 ess2 = fold2_ess (lift_combi f c) ess1 ess2

    let do_speculate (v,vcl,es) = (v,vcl,E.do_speculate es)
    let do_speculates ess = Evt.map do_speculate ess

    (* Combine the results *)
    let combi f c s1 s2 =
      fun eiid ->
        let (eiid,(s1act,spec1)) = s1 eiid in
        let (eiid,(s2act,spec2)) = s2 eiid in
        let s3act = lift_combis f c s1act s2act in
        let spec3 = match spec1,spec2 with
        | None, None -> None
        | Some elt1, Some elt2 -> Some (lift_combis f c elt1 elt2)
        | None, Some elt2 ->
            let elt1 = do_speculates s1act in
            Some (lift_combis f c elt1 elt2)
        | Some elt1, None ->
            let elt2 = do_speculates s2act in
            Some (lift_combis f c elt1 elt2) in
        (eiid,(s3act,spec3))

    let (>>|) : 'a t -> 'b t -> ('a * 'b)  t
        = fun s1 s2 -> combi Misc.pair (fun es1 es2 -> es1 =|= es2) s1 s2

    let para_atomic s1 s2 = combi Misc.pair E.para_atomic s1 s2

    let para_input_right s1 s2 = combi Misc.pair E.para_input_right s1 s2

    let (>>::) : 'a t -> 'a list t -> 'a list  t
        = fun s1 s2 -> combi (Misc.cons) (fun es1 es2 -> es1 =|= es2) s1 s2

(* Parallel composition no result *)
    let (|||) : unit t -> unit t -> unit t
      = fun  s1 s2 -> combi (fun _ _ -> ()) (=|=) s1 s2

(* Sequence memory events *)
    let seq_mem : 'a t -> 'b t -> ('a * 'b) t
      = fun  s1 s2 -> combi Misc.pair E.seq_mem s1 s2

    let seq_mem_list : 'a t -> 'a list t -> 'a list t
      = fun  s1 s2 -> combi Misc.cons E.seq_mem s1 s2

(* Force monad value *)
    let forceT (v : 'a) : 'b t -> 'a t =
      let f (_, vcl, es) = (v, vcl, es) in
      map_elt f

    let (>>!) s v = forceT v s

    let discardT : 'a t -> unit t = fun s eiid -> forceT () s eiid

(* Add a value *)
    let addT (v1: 'a) : 'b t -> ('a * 'b) t =
      let f (v2, vcl, es) = ((v1, v2), vcl, es) in
      map_elt f

(* Assert a value *)
    let assertT (v: A.V.v) : 'a t -> 'a t =
      let f (r, cs, es) = (r, VC.Assign (v, VC.Atom V.one) :: cs, es) in
      map_elt f

(* Choosing dependant upon flag, notice that, once determined v is either one or zero *)
    let choiceT =
      fun v l r eiid ->
        if V.is_var_determined v then begin
          if V.is_zero v  then r eiid else l eiid
        end else
          let (eiid, (lact,lspec)) = l eiid in
          assert (lspec = None);
          let (eiid, (ract,rspec)) = r eiid in
          assert (rspec = None);
          let fl = (fun (r,cs,es) ->
            (r,(VC.Assign (v,VC.Atom V.one)) :: cs,es)) in
          let fr = (fun (r,cs,es) ->
            (r,(VC.Assign (v,VC.Atom V.zero)) :: cs, es)) in
          let un =
            Evt.union
              (Evt.map fl lact)
              (Evt.map fr ract) in
          (eiid, (un, None))

(* Extract speculaive behaviout, base case is to speculate active branch *)
    let as_speculated (act,spec) = match spec with
    | Some spec -> spec
    | None -> do_speculates act

(* Combine active branch and speculated one -> active *)
    let combi_acts ok no fcs =
      let acts,_ = ok
      and specs =  as_speculated no in
      Evt.fold
        (fun (v,cl_act,es_act) acc ->
          let cl_act = fcs cl_act in
          Evt.fold
            (fun (_v_spec,cl_spec,es_spec) acc ->
              Evt.add
                (v,cl_act@cl_spec,Misc.as_some (es_act =|= es_spec))
                acc)
            specs acc)
        acts Evt.empty

(* Simple union of speculated structure(s) *)
    let combi_spec p1 p2 =
      let es1 = as_speculated p1
      and es2 = as_speculated p2 in
      let specs =
        lift_combis
          (fun v _ -> v) (* Values are assume to be the same, unchecked *)
          (=|=)
          es1 es2 in
      Some specs

    let speculT  : V.v -> 'a code -> 'a code -> 'a code =
      fun v l r eiid ->
        let eiid,pl = l eiid in
        let eiid,pr = r eiid in
        if V.is_var_determined v then begin
          if V.is_zero v then
            eiid,(combi_acts pr pl (fun cs -> cs),combi_spec pl pr)
          else
            eiid,(combi_acts pl pr (fun cs -> cs),combi_spec pr pl)
        end else
          let act =
            Evt.union
              (combi_acts pr pl (fun cs -> VC.Assign (v,VC.Atom V.zero)::cs))
              (combi_acts pl pr (fun cs -> VC.Assign (v,VC.Atom V.one)::cs))
          and spec = combi_spec pr pl in
          eiid,(act,spec)

    let condJumpT = if do_deps then speculT else choiceT

    let indirectJumpT v lbls g =
      assert (not do_deps) ;
      (* One *)
      let do_one (p,lbl) eiid =
        let eiid,(act,_) = g lbl eiid in
        let f (r,cs,es) =
          let cs =
            VC.Assign (v,VC.Atom (V.Val (Constant.Label (p,lbl))))::cs in
          r,cs,es in
        eiid,(Evt.map f act,None) in
      (* Rec *)
      let rec do_rec lbls eiid =
        match lbls with
        | [] -> assert false (* Caught earlier, in ArchSem module *)
        | [lbl] -> do_one lbl eiid
        | lbl::lbls ->
           let eiid,(act,_) = do_one lbl eiid in
           let eiid,(acts,_) = do_rec lbls eiid in
           eiid,(Evt.union act acts,None) in
        do_rec (Label.Full.Set.elements lbls)

    let speculPredT v pod l r =
      fun eiid ->
        let eiid,pod = pod eiid in
        let eiid,l = l eiid in
        let eiid,r = r eiid in
        let (),podcl,podact = Evt.as_singleton_nospecul pod
        and lv,lcl,lact = Evt.as_singleton_nospecul l
        and rv,rcl,ract = Evt.as_singleton_nospecul r in
        let lspec = E.do_speculate lact
        and rspec = E.do_speculate ract in
        let cl = podcl@lcl@rcl in
        let lact =
          Misc.as_some (podact =*$= Misc.as_some (lact =|= rspec))
        and ract =
          Misc.as_some (podact =*$= Misc.as_some (ract =|= lspec))
        and spec =
          Misc.as_some
            (E.do_speculate podact =*$=  Misc.as_some (lspec =|= rspec)) in
        let act =
          Evt.union
            (Evt.singleton (rv, VC.Assign (v,VC.Atom V.zero)::cl,ract))
            (Evt.singleton (lv, VC.Assign (v,VC.Atom V.one)::cl,lact))
        and spec = Evt.singleton (rv,cl,spec) in
        eiid,(act,Some spec)

    let condPredT  v pod m1 m2 =
      if do_deps then speculPredT v pod m1 m2
      else pod >>= fun () -> choiceT v m1 m2

    let discard_false sact =
      List.fold_right
        (fun (b,vcl,evt) k -> if b then ((),vcl,evt)::k else k)
        sact []

    let discard_false_opt = function
      | None -> None
      | Some sact -> match discard_false sact with
        | [] -> None
        | sact -> Some sact

    let (|*|) : bool code -> unit code -> unit code
        = fun s1 s2 ->
          fun (poi,eiid) ->
            let ((_,eiid), (s1act,spec1)) = s1 (poi,eiid) in
            let ((_,eiid), (s2act,spec2)) = s2 (poi,eiid) in
            let s1lst = Evt.elements s1act in
            let s2lst = Evt.elements s2act in
            let s3act =
              List.fold_left
                (fun acc (va,vcla,evta) ->
                  if va then
                    List.fold_left
                      (fun acc (_,vclb,evtb) ->
                        match evta +|+ evtb with
                        | Some evtc -> Evt.add ((), vcla@vclb, evtc) acc
                        | None      -> acc)
                      acc s2lst
                  else acc)
                Evt.empty s1lst in
            let spec3 = None in
            let pair = begin
              if Evt.is_empty s2act then
                (discard_false s1act,discard_false_opt spec1)
              else if Evt.is_empty s1act then (s2act,spec2)
              else (s3act,spec3)
            end
            in
            ((poi,eiid),pair)

    let cseq : 'a t -> ('a -> 'b t) -> 'b t = fun s f ->  data_comp (+|+) s f

    let para_bind_output_right : 'a t -> ('a -> 'b t) -> 'b t =
      fun s f -> data_comp E.para_output_right s f

    let asl_seq : 'a t -> ('a -> 'b t) -> 'b t =
      fun s f -> data_comp E.para_po_seq_output_right s f

    type poi = int

(************************************************)
(* Combining instruction and continuation code. *)
(* Notice that result is v2                     *)
(************************************************)


(* ordinary combination, not much to say *)

    let other_combi ok (_,vcl1,es1) (v2,vcl2,es2) k =
      if ok v2 then
        let es = E.inst_code_comp es1 es2 in
        Evt.add (v2,vcl1@vcl2,es) k
      else k

    let other_combi_spec ok (_,vcl1,es1) (v2,vcl2,es2) (_,vcl3,es3) k =
      if ok v2 then
        let es = E.inst_code_comp_spec es1 es2 es3 in
        Evt.add (v2,vcl1@vcl2@vcl3,es) k
      else k


(* Ordinary instr + code compostion. Notice: no causality from s to f v1 *)

    let comb_instr_code
        : ('b -> bool) -> (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
            =
          fun ok s f (poi,eiid) ->
            let ({id=eiid;_},(acts1,spec1)) = s poi {id=eiid; sub=0;} in
            assert (spec1 = None) ;
            let poi,acts =
              Evt.fold
                (fun ((poi1,v1), vcl1, es1) ((po,eiid),acts) ->
                  let (po2,eiid),(acts2,spec2) = f v1 (poi1,eiid) in
                  assert (spec2 = None) ;
                  let acts =
                    Evt.fold
                      (fun (v2,vcl2,es2) acts ->
                        if ok v2 then
                          let es = E.inst_code_comp es1 es2 in
                          Evt.add (v2,vcl2@vcl1,es) acts
                        else acts)
                      acts2 acts in
                  (max po2 po,eiid),acts)
                acts1 ((0,eiid),Evt.empty) in
            (poi,(acts,None))

(* Idem, speculation is possible and handled if present *)

    let not_speculated es = E.EventSet.is_empty es.E.speculated

    let comb_instr_code_deps
        : ('b -> bool) -> (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
            =
          fun ok s f -> fun (poi,eiid) ->
            let ({id=eiid;_}, (sact,spec)) = s poi {id=eiid;sub=0} in
            (* We check that all semantics for "s" (instruction)
               1. Yield the same value,
               2. Have the same status w.r.t. speculation.
               So as to apply f (code continuation) only once (or twice) *)
            let ((poi,v1),_,es1) =
              Evt.wrap_check
                (fun (v1,_,es1) (v2,_,es2) ->
                  v1 = v2 && not_speculated es1 = not_speculated es2) sact in
            if not_speculated es1 then
              let poi,(b_setact,bspec) = f v1 (poi,eiid) in
              let k = fold2_ess (other_combi ok) sact b_setact in
              let spec = match spec,bspec with
              | None, None -> None
              | None, Some spec2 ->
                  let spec1 = do_speculates sact in
                  Some (fold2_ess (other_combi ok) spec1 spec2)
              | Some spec1,None ->
                  let spec2 = do_speculates b_setact in
                  Some (fold2_ess (other_combi ok) spec1 spec2)
              | Some spec1,Some spec2 ->
                  Some (fold2_ess (other_combi ok) spec1 spec2) in
              (poi,(k,spec))
            else
              let poi,(b_setact,bspec) = f v1 (poi,eiid) in
              let poi,(c_setact,cspec) = f v1 poi in
              let k =
                fold3_ess (other_combi_spec ok) sact b_setact c_setact in
              let spec = match spec,bspec,cspec with
              | Some spec1,None,None ->
                  let spec2 = do_speculates b_setact
                  and spec3 = do_speculates c_setact in
                  Some (fold3_ess (other_combi_spec ok) spec1 spec2 spec3)
              | Some spec1,Some spec2,Some spec3 ->
                  Some (fold3_ess (other_combi_spec ok) spec1 spec2 spec3)
              | _ ->
                  Warn.fatal "Inconsistent speculation in (<<<)" in
              poi,(k,spec)

(* Actual instr + code combination depends upon deps mode *)
    let add_instr ok s f =
      if do_deps then comb_instr_code_deps ok s f
      else comb_instr_code ok s f

    let (>>>) s f = add_instr (fun _ -> true) s f

(* For combining conditions and branches of an if, as above + instruction dependencies *)
    let (>>>>) s f = fun eiid ->
      let (eiid,(sact,spec)) = s eiid in
      assert(spec = None);
      let eiid,bfinal=
        Evt.fold (fun (v1, vcl1, es1) (eiid1,acc) ->
          let b_set = f v1 in
          let (eiid_b,(b_setact,bspec)) = b_set eiid1 in
          assert (bspec = None);
          Evt.fold (fun (v2,vcl2,es2) (eiid2,acc) ->
            let es = E.cond_comp es1 es2 in
            eiid2,Evt.add (v2,vcl2@vcl1,es) acc)
            b_setact (eiid_b,acc))
          sact (eiid,Evt.empty)
      in eiid,(bfinal,None)

    (* Build event structure from event set *)

    let do_trivial es = E.from_events es

    (* Build event structure from action and instruction instance *)

    let (++) f g  = fun eiid ->
      let eiid,x = f eiid in
      g x eiid
    and (--) f g  = fun eiid ->
      let eiid,x = f eiid in
      eiid,g x

    let do_make_one_event iiid a eiid =
      bump_eid eiid,
      { E.eiid = eiid.id; E.subid=eiid.sub;
        E.iiid = iiid;
        E.action = a; }

    let make_one_event ii = do_make_one_event (E.IdSome ii)
    and make_one_init_event = do_make_one_event E.IdInit

    let do_make_one_event_structure a iiid =
      do_make_one_event iiid a --
      fun evt -> do_trivial (E.EventSet.singleton evt)

    let make_one_event_structure a ii =
      do_make_one_event_structure a (E.IdSome ii)


    (* Add some fields to structure *)

    let add_data_ports st = { st with E.data_ports = st.E.events; }
    let add_success_ports st = { st with E.success_ports = st.E.events; }

    let as_data_port : 'a t -> 'a t =
      let f (a, cs, es) = (a, cs, add_data_ports es) in
      fun m -> map_elt f m

    let do_make_one_event_structure_data is_data =
      if is_data then
        fun a iiid -> do_make_one_event_structure a iiid -- add_data_ports
      else
        do_make_one_event_structure

    (* Add return value and equation to monad *)
    let make_one_monad v eqs st = fun eiid ->
      eiid,(Evt.singleton (v,eqs,st),None)

    (* A few monad constructors from action and instruction instance *)
    let mk_singleton_es a ii =
      make_one_event_structure a ii ++ make_one_monad () []

    (* Some specific, exported basic monad building functions *)
    let mk_singleton_es_success a ii =
      make_one_event_structure a ii -- add_success_ports ++
      make_one_monad () []

    let mk_singleton_es_eq a eqs ii =
      make_one_event_structure a ii ++
      make_one_monad () eqs

    let restrict cs = make_one_monad () cs E.empty_event_structure

    (******************************************************)
    (* Some basic event structures, read, write, fence... *)
    (******************************************************)

    (* Fresh variables are generated by reading *)
    let do_read_loc is_data mk_action loc iiid = fun eiid ->
      (* It is important to call V.fresh_var
         for every _complete_ call of read_loc *)
      let v = match iiid,loc with
          | E.IdSome {A.env={A.regs=env;_}; _},A.Location_reg (_,r) ->
             begin match A.look_reg r env with
             | Some v -> v
             | None -> V.fresh_var ()
             end
          | _ -> V.fresh_var () in
      let m =
        do_make_one_event_structure_data is_data (mk_action loc v) iiid ++
        make_one_monad v [] in
      m eiid

    let read_loc is_data mk_action loc ii =
      do_read_loc is_data mk_action loc (E.IdSome ii)

    (* Writing is as simple as emiting one event *)

    let do_write_loc mk_action loc iiid =
      do_make_one_event_structure (mk_action loc) iiid ++ make_one_monad () []

    let write_loc mk_action loc ii =
      do_write_loc mk_action loc (E.IdSome ii)

    (* Fence events have no 'maximal' or output events,
       which makes them transparent to iicoo_data composition *)
    let mk_fence a ii =
      make_one_event_structure a ii --
      (fun st -> { st with E.output = Some E.EventSet.empty;}) ++
      make_one_monad () []

    let fetch op arg mk_action ii = fun eiid ->
      let v = V.fresh_var ()
      and vstored = V.fresh_var () in
      let m =
        let eqs = [VC.Assign (vstored,VC.Binop (op,v,arg))]
        and a = mk_action v vstored in
        make_one_event_structure a ii ++ make_one_monad v eqs in
      m eiid

    (**********************)
    (* Morello extensions *)
    (**********************)

    let add_atomic_tag_read m a f ii = fun eiid ->
      let (eiid,(sact,sspec)) = m eiid in
      assert(sspec = None);
      let (v,eqs,st) = Evt.as_singleton sact in
      let a_tag = V.fresh_var () in
      let eqs = VC.Assign (a_tag,VC.Unop (Op.CapaTagLoc,a))::eqs in
      let vs_tag = V.fresh_var () in
      let v_tag = V.fresh_var () in
      let eqs = eqs@[VC.Assign (v_tag,VC.Binop (Op.CapaSetTag,v,vs_tag))] in
      let eiid,es = bump_eid eiid, E.EventSet.add
        { E.eiid = eiid.id; E.subid=eiid.sub; E.iiid = E.IdSome ii;
          E.action = f (A.Location_global a_tag) vs_tag;} st.E.events in
      let e_full_action = if E.EventSet.is_empty st.E.events then f (A.Location_global a) v
        else (E.EventSet.max_elt st.E.events).E.action in
      let e_full = if E.EventSet.is_empty st.E.mem_accesses then
        { E.eiid=eiid.id; E.subid=eiid.sub; E.iiid = E.IdSome ii;
          E.action = e_full_action; } else
        E.EventSet.max_elt st.E.mem_accesses in
      let st = { st with
        E.events = es;
        E.sca = E.EventSetSet.singleton es;
        E.mem_accesses = E.EventSet.singleton e_full;
        E.aligned = [e_full,es]; } in
      bump_eid eiid,(Evt.singleton (v_tag,eqs,st),None)

    let add_atomic_tag_write m a v f ii = fun eiid ->
      let (eiid,(sact,sspec)) = m eiid in
      assert(sspec = None);
      let ((),eqs,st) = Evt.as_singleton sact in
      let a_tag = V.fresh_var () in
      let eqs = VC.Assign (a_tag,VC.Unop (Op.CapaTagLoc,a))::eqs in
      let eiid,es = bump_eid eiid, E.EventSet.add
        { E.eiid = eiid.id; E.subid=eiid.sub; E.iiid = E.IdSome ii;
          E.action = f (A.Location_global a_tag) v;} st.E.events in
      let e_full_action = if E.EventSet.is_empty st.E.events then f (A.Location_global a) v
        else (E.EventSet.max_elt st.E.events).E.action in
      let e_full = if E.EventSet.is_empty st.E.mem_accesses then
        { E.eiid=eiid.id; E.subid=eiid.sub; E.iiid = E.IdSome ii;
          E.action = e_full_action; } else
        E.EventSet.max_elt st.E.mem_accesses in
      let st = { st with
        E.events = es;
        E.sca = E.EventSetSet.singleton es;
        E.mem_accesses = E.EventSet.singleton e_full;
        E.aligned = [e_full,es]; } in
      bump_eid eiid,(Evt.singleton ((),eqs,st),None)

(**************)
(* Mixed size *)
(**************)

    module Mixed(SZ:ByteSize.S) = struct

      let memtag = C.variant Variant.MemTag
      let morello = C.variant Variant.Morello
      let kvm = C.variant Variant.VMSA

      module AM = A.Mixed(SZ)

      module Scalar = V.Cst.Scalar
      let def_size v= match v with
        | V.Val (Constant.Instruction _) ->
            MachSize.Word (* TODO: arch dependennt *)
        | _ -> Scalar.machsize

      let extract_byte v = VC.Unop (Op.AndK AM.mask,v)

      let extract_step v =
        let d = extract_byte v
        and w = VC.Unop (Op.LogicalRightShift AM.nshift,v) in
        d,w

(* Translate to list of bytes, least significant first *)
      let explode sz v =
        let rec do_rec k v =
          if k <= 1 then [v],[]
          else
            let d,w = extract_step v in
            let vw = V.fresh_var () in
            let ds,eqs = do_rec (k-1) vw in
            let vd =  V.fresh_var () in
            vd::ds,
            VC.Assign (vw,w)::VC.Assign (vd,d)::eqs in
        do_rec (AM.nsz sz) v

(* Translate from list of bytes  least significant first *)
      let rec recompose ds = match ds with
      | [] -> assert false
      | [d] -> d,[]
      | d::ds ->
          let w,eqs = recompose ds in
          let vw = V.fresh_var ()
          and x =  V.fresh_var () in
          vw,VC.Assign (x,VC.Unop (Op.LeftShift AM.nshift,w))::VC.Assign (vw,VC.Binop (Op.Or,x,d))::eqs

(* Bytes addresses, little endian *)

      let byte_eas sz a =
        let kmax = AM.nsz sz in
        let rec do_rec k =
          if k >= kmax then [],[]
          else
            let xa = V.fresh_var() in
            let xas,eqs = do_rec (k+1) in
            xa::xas,VC.Assign (xa,VC.Unop (Op.AddK (k*AM.byte_sz),a))::eqs in
        let xas,eqs = do_rec 1 in
        let xas = a::xas in
        let open Endian in
        match AM.endian with
        | Little -> xas,eqs
        | Big -> List.rev xas,eqs

(* Build mixed size event structure *)
      let make_mixed es e =
        { E.empty_event_structure with
          E.events = es ;
          E.sca = E.EventSetSet.singleton es;
          E.mem_accesses = E.EventSet.singleton e;
          aligned = [e,es]; }

      let add_evt es e = E.EventSet.add e es

      let fold_pp m k xs = fun eiid ->
        List.fold_left (fun (eiid,k) x -> m k x eiid) (eiid,k) xs

      let read_mixed is_data sz mk_act a ii =
        fun eiid ->
          let eas,a_eqs = byte_eas sz a in
          let eavs = List.map (fun ea -> ea,V.fresh_var ()) eas in
          let vs = List.map snd eavs in
          let v,v_eqs = recompose vs in
          let eiid,es =
            fold_pp
              (fun es (ea,v) ->
                make_one_event ii
                  (mk_act SZ.byte (A.Location_global ea) v) --
                add_evt es)
              E.EventSet.empty eavs
            eiid in
          let eiid,e_full =
            make_one_event ii (mk_act sz (A.Location_global a) v) eiid in
          let st = make_mixed es e_full in
          let st =
            if is_data then { st with E.data_ports = es } else st in
          make_one_monad v (a_eqs@v_eqs) st eiid

      let fold_pp2 m k xs ys = fun eiid ->
        List.fold_left2 (fun (eiid,k) x y -> m k x y eiid) (eiid,k) xs ys

      let write_mixed sz mk_act a v ii =
        fun eiid ->
          let eas,a_eqs = byte_eas sz a
          and vs,v_eqs = explode sz v in
          let eiid,es =
            fold_pp2
              (fun es ea v ->
                make_one_event ii
                  (mk_act SZ.byte (A.Location_global ea) v) --
                add_evt es)
              E.EventSet.empty eas vs eiid in
          let eiid,e_full =
            make_one_event ii
              (mk_act sz (A.Location_global a) v) eiid in
          let st = make_mixed es e_full in
          make_one_monad () (a_eqs@v_eqs) st eiid

      let is_tagloc a =
        let open Constant in
        match a with
        | V.Val (Symbolic (TagAddr _)) -> true
        | _ -> false

      let is_pteloc a =
        let open Constant in
        match a with
        | V.Val (Symbolic (System (PTE,_))) -> true
        | _ -> false

      let is_instrloc a =
        match a with
        | V.Val (Constant.Label _) -> true
        | _ -> false

(*
 * Add init writes for tag addresses.
 * A symbolic location has its own tag address, hence
 * for a given x, tag(x) is initialised, except for arrays
 * that may span over several granules.
 * The task of initialising the appropriate tag addresses is
 * facilitated by array init being implemented as the explicit
 * initialisations of its elements.
 * For instance for int t[5], we have int-sized initial writes for
 * t+0, t+4, t+8, t+12, t+16, yielding the two tag addresses
 * tag(t) and t(t+16). Namely, with a granule size of 4 int's
 * the first four items have the same tag address tag(x).
 *)
      let add_inittags env =
        let glob,tag =
          List.fold_left
            (fun (glob,tag as p) (loc,v0) -> match loc with
            | A.Location_global a ->
                if is_tagloc a then begin
                  if dbg then
                    Printf.eprintf
                      "Explicit initialisation of tag %s to value %s\n"
                    (V.pp_v a) (V.pp_v v0) ;
                  glob,a::tag
                end else a::glob,tag
            | A.Location_reg _ -> p)
            ([],[]) env in
        let tag_set = V.ValueSet.of_list tag in
        let glob_set = V.ValueSet.of_list glob in
        let glob_set =
          V.ValueSet.filter
            (fun a ->  not (is_pteloc a || is_instrloc a))
            glob_set in
        let s = V.ValueSet.map (fun a -> V.op1 Op.TagLoc a) glob_set in
        let env =
          V.ValueSet.fold
            (fun atag env ->
              if V.ValueSet.mem atag tag_set then env
              else begin
                if dbg then
                  eprintf "Tag %s defaulting\n" (V.pp_v atag) ;
                (A.Location_global atag,V.Val (Constant.default_tag))::env
             end)
            s env in
        env

      let morello_init_tag s v eiid =
        assert morello ;
        let open Constant in
        bump_eid eiid,
        { E.eiid = eiid.id; E.subid=eiid.sub; E.iiid = E.IdInit;
          E.action =
            E.Act.mk_init_write
              (A.of_symbolic_data
                 {default_symbolic_data with name=Misc.add_ctag s})
              (def_size v) v; }

      let debug_env env =
        String.concat ", "
          (List.map
             (fun (loc,v) -> sprintf "%s -> %s" (A.pp_location loc) (V.pp_v v))
             env)

      let val_of_pteval p = V.Val (Constant.PteVal p)

      let default_pteval s = val_of_pteval (V.Cst.PteVal.default s)
      and pteval_of_pte s = val_of_pteval (V.Cst.PteVal.of_pte s)

      let expand_pteval loc v =
        let open Constant in
        match v with
        | V.Val (Symbolic (Physical (s,_))) -> default_pteval s
        | V.Val (PteVal _) -> v
        | _ ->
            Warn.user_error
              "Cannot initialize %s with %s"
              (A.pp_location loc ) (V.pp C.hexa v)

      let pte_loc s =
        let open Constant in
        A.Location_global (V.Val (Symbolic (System (PTE,s))))

      let pte2_loc s =
        let open Constant in
        A.Location_global (V.Val (Symbolic (System (PTE2,s))))

      let phy_loc s o =
        let open Constant in
        A.Location_global (V.Val (Symbolic (Physical (s,o))))

      let extract_virtual_pte env =
        let open Constant in
        List.fold_right
          (fun (loc,v as bd) (env,(virt,pte as maps)) ->
            match loc with
            | A.Location_global
              (V.Val
                 (Symbolic (Virtual {name=s; tag=None; offset=o;_}))) ->
               (phy_loc s o,v)::env,
               (StringSet.add s virt,pte)
            | A.Location_global (V.Val (Symbolic (System (PTE,s)))) ->
                let v = expand_pteval loc v in
                (loc,v)::env,(virt,StringSet.add s pte)
            | A.Location_global (V.Val (Symbolic (TagAddr (VIR,s,o)))) ->
               let loc =
                  A.Location_global (V.Val (Symbolic (TagAddr (PHY,s,o)))) in
               (loc,v)::env,maps
            | A.Location_global (V.Val (Symbolic (Physical _|Virtual _))) ->
                Warn.user_error "herd cannot handle initialisation of '%s'"
                  (A.pp_location loc)
            | _ -> bd::env,maps)
          env ([],(StringSet.empty,StringSet.empty))


      let add_initpte =
        let open Constant in
        fun env ->
          (* Collect virtual initialisations and explicit pte initialisations *)
          let env,(virt,pte) = extract_virtual_pte env in
          (* Add default initialisation of pte, when appropriate *)
          let env =
            StringSet.fold
              (fun s env ->
                if StringSet.mem s pte then env
                else (pte_loc s,default_pteval s)::env)
              virt env in
          let env =
            if C.variant Variant.PTE2 then
              List.fold_right
                (fun (loc,_ as bd) env -> match loc with
                | A.Location_global (V.Val (Symbolic (System (PTE,s))))
                  ->
                  bd::(pte2_loc s,pteval_of_pte s)::env
               | _ -> bd::env)
                env []
            else env in
          env

      let debug_add_initpte env =
        let r = add_initpte env in
        eprintf "Complete pte initialisation:\n[%s] -> [%s]\n" (debug_env env) (debug_env r) ;
        r

      let initwrites_non_mixed madd env size_env other_es =
        if dbg then
          Printf.eprintf "Initial env for init_writes: {%s}\n" (debug_env env) ;
        let env =
          if kvm then (if dbg then debug_add_initpte else add_initpte) env
          else env in
        fun eiid ->
          let eiid,es =
            List.fold_left
              (fun (eiid,es) (loc,v) ->
                let sz =
                  match A.symbolic_data loc with
                  | Some  {Constant.name=s; _}
                        when not (Misc.check_atag s) ->
(* Notice that size does not depend upon offset.
   That is, all addresses with the same base
   share the same size *)
                      A.look_size size_env s
                  | _ -> def_size v in
                let eiid,ew =
                  let v = V.map_scalar (V.Cst.Scalar.mask sz) v in
                  make_one_init_event
                    (E.Act.mk_init_write loc sz v) eiid in
                match A.symbolic_data loc with
                | Some {Constant.name=s; offset=0;_} ->
                    let eiid,ews =
                      if morello then
                        let eiid,em =
                          morello_init_tag
                            s (V.op1 Op.CapaGetTag v) eiid in
                        eiid,(em::[ew])
                      else eiid,[ew] in
                    (eiid,ews@es)
                | _ -> (eiid,ew::es))
              (eiid,[]) env in
          let es = E.EventSet.of_list (es @ other_es) in
          if dbg then
            begin
              eprintf "Init writes %a\n" E.debug_events es
            end ;
          madd (make_one_monad () [] (do_trivial es)) eiid

      let debug_env env =
        String.concat "; "
          (List.map
             (fun (loc,v) -> A.pp_location loc ^ " -> " ^ V.pp_v v)
             env)

      let initwrites_mixed env size_env other_es =
        if dbg then begin
            eprintf "Env is: [%s]\n" (debug_env env)
          end ;
        fun eiid ->
        try
          let eiid,es,sca =
            List.fold_left
              (fun (eiid,es,sca) (loc,v) ->
                let open Constant in
                match loc with
                | A.Location_global
                  (V.Val
                     (Symbolic
                        (Virtual
                           {name=s;offset=_;_})) as a)
                      when not (Misc.check_atag s) ->
                    (* Suffix encoding of tag addresses, sufficient for now *)
                    let sz = A.look_size size_env s in
                    let ds = AM.explode sz v
                    and eas = AM.byte_eas sz a in
                    let eiid,ews =
                      List.fold_left2
                        (fun (eiid,ews) a d ->
                          let eiid,ew =
                            make_one_init_event
                              (E.Act.mk_init_write
                                 (A.Location_global a) SZ.byte d)
                              eiid in
                          eiid,ew::ews)
                        (eiid,[]) eas ds in
                    let eiid,ews =
                      if morello then
                        let eiid,em =
                          morello_init_tag
                            s (V.op1 Op.CapaGetTag v)
                            eiid in
                        eiid,em::ews
                      else eiid,ews in
                    eiid,ews@es,
                    E.EventSetSet.add (E.EventSet.of_list ews) sca
                | _ ->
                    let eiid,ew =
                      make_one_init_event
                        (E.Act.mk_init_write loc (def_size v) v) eiid in
                    eiid,ew::es,
                    E.EventSetSet.add (E.EventSet.singleton ew) sca)
              (eiid,[],E.EventSetSet.empty) env in
          let es = E.EventSet.of_list (es @ other_es) in
          if dbg then begin
              eprintf "Init writes %a\n" E.debug_events es
            end ;
          let st = do_trivial es in
          let st = { st with E.sca; } in
          make_one_monad () [] st eiid
        with
        | V.Undetermined -> assert false

      let do_initwrites madd env =
        if dbg then
          Printf.eprintf
            "Env before additions: %s\n" (debug_env env) ;
        let env = if memtag then add_inittags env else env in
        (if A.is_mixed then initwrites_mixed
        else initwrites_non_mixed madd) env

      let t2code : 'a t -> 'a code
          = fun m -> fun (poi,eiid) ->
            let eiid,r = m {id=eiid;sub=0;} in
            ((poi,eiid.id),r)

      let initwrites madd env size_env =
        t2code (do_initwrites madd env size_env [])

    end

    (***************************************************)
    (* Operations, no events generated, only equations *)
    (***************************************************)

    let delay_op mk_c =
      let v = V.fresh_var () in
      make_one_monad v [VC.Assign (v, mk_c ())] E.empty_event_structure

    let any_op mk_v mk_c =
      try
        let v = mk_v () in
        make_one_monad v [] E.empty_event_structure
      with
      | V.Undetermined ->
         (* Not ready yet add equation *)
         delay_op mk_c
      | exn ->
         if C.debug.Debug_herd.exc then raise exn
         (* Delay failure *)
         else delay_op mk_c

    let op1 op v1 =
      any_op
        (fun () -> V.op1 op v1)
        (fun () -> VC.Unop (op,v1))

    and op op v1 v2 =
      any_op
        (fun () -> V.op op v1 v2)
        (fun () -> VC.Binop (op,v1,v2))

    and op3 op v1 v2 v3 =
      any_op
        (fun () -> V.op3 op v1 v2 v3)
        (fun () -> VC.Terop (op,v1,v2,v3))

    let add =  op Op.Add


(* Add an inequality constraint *)

    let assign v1 v2 =
      make_one_monad () [(VC.Assign (v1,VC.Atom v2))]
        E.empty_event_structure

    let neqT : V.v -> V.v -> unit t
        = fun v1 v2 ->
          op Op.Eq v1 v2 >>= fun v -> assign v V.zero

    let eqT : V.v -> V.v -> unit t = assign

    let cutoffT msg ii v =
      forceT v (mk_singleton_es (E.Act.cutoff msg) ii)

    type evt_struct = E.event_structure
    type output = VC.cnstrnts * evt_struct

    let get_output et k =
      let (_,(es,_)) = et (0,0) in
      List.fold_left
        (fun k (_,vcl,evts) -> (vcl,evts)::k)
        k (Evt.elements es)

    let force_once (m : 'a t) : 'a t =
      let res = ref None in
      let new_m eiid =
        match !res with
        | None ->
            let eiid, v = m eiid in
            let _evts, evts_specul = v in
            let () =
              if Option.is_none evts_specul then ()
              else Warn.warn_always "Speculated stored events. Results unknown."
            in
            let () = res := Some v in
            (eiid, v)
        | Some v -> (eiid, v)
      in
      new_m

    let debugT (s : string) (m : 'a t) : 'a t
      = fun eiid ->
        let eiid,(evts,specs) = m eiid in
        List.iter (fun (_,_,es) -> eprintf "%s%a" s E.debug_event_structure es) (Evt.elements evts) ;
        eiid,(evts,specs)
  end
