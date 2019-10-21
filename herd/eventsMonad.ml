(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A monad for event structures *)

module type Config = sig
  val hexa : bool
  val debug : Debug_herd.t
  val variant : Variant.t -> bool
  val byte : MachSize.sz
end

module Make (C:Config) (A:Arch_herd.S) (E:Event.S with module A = A and module Act.A = A) :
    (Monad.S with module A = A and module E = E
and type evt_struct = E.event_structure) =
  struct

    module A = A
    module E = E
    module V = A.V
    module VC =
      Valconstraint.Make
        (struct
          let hexa = C.hexa let debug = C.debug.Debug_herd.solver
        end)
        (A)


(* LM Use lists for polymorphism.
   It is assumed that list elts are pairwise distinct.
 *)
    module Evt = struct
      type 'a t =  ('a * VC.cnstrnts * E.event_structure) list

      let empty = []

      let singleton x = [x]

      let is_empty = function
        | [] -> true
        | _::_ -> false

      let as_singleton = function
        | [x] -> x
        | _ -> assert false

      let add x s = x::s

      let map = List.map

      let fold f xs y0 =
        List.fold_left (fun x y -> f y x) y0 xs

      let filter = List.filter

      let union = (@)

      let elements (x:'a t) = x
    end

    type 'a t = int -> int * ('a Evt.t) (* Threading through eiid *)

    let zeroT : 'a t
        = (fun eiid_next -> (eiid_next, Evt.empty))

    let unitT (v : 'a) : 'a t =
      fun eiid_next ->
        eiid_next,Evt.singleton (v, [], E.empty_event_structure)

    let (=**=) = E.(=**=)
    let (=*$=) = E.(=*$=)
    let (=$$=) = E.(=$$=)
    let (=|=) = E.(=|=)
    let (+|+) = E.(+|+)

(* Bind the result *)
    let data_comp comp_str s f =
      (fun eiid ->
        let (eiid_next, sact) = s eiid in
        Evt.fold (fun (v1, vcl1, es1) (eiid1,acc) ->
          let b_set = f v1 in
          let (eiid_b,b_setact) = b_set eiid1 in
          Evt.fold (fun (v2,vcl2,es2) (eiid2,acc_inner) ->
            match comp_str es1 es2 with
            | None -> (eiid2, acc_inner)
            | Some es -> (eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
                   )
            b_setact (eiid_b,acc)
                 )
          sact (eiid_next,Evt.empty))

    let (>>=) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (=*$=) s f

    let (>>==) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f -> data_comp (=$$=) s f

(* Bind the result *)
    let (>>*=) : 'a t -> ('a -> 'b t) -> ('b) t
        = fun s f ->
          (fun eiid ->
            let (eiid_next, sact) = s eiid in
            Evt.fold (fun (v1, vcl1, es1) (eiid1,acc) ->
              let b_set = f v1 in
              let (eiid_b,b_setact) = b_set eiid1 in
              Evt.fold (fun (v2,vcl2,es2) (eiid2,acc_inner) ->
                match es1 =**= es2 with
                | None -> (eiid2, acc_inner)
                | Some es -> (eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
                       )
                b_setact (eiid_b,acc)
                     )
              sact (eiid_next,Evt.empty))

(* Exchange combination *)
    let exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'b t) ->  ('b * 'b) t
        = fun rx ry wx wy ->
          fun eiid ->
            let eiid,rxact = rx eiid in
            let eiid,ryact = ry eiid in
            let (vrx,vclrx,esrx) = Evt.as_singleton rxact
            and (vry,vclry,esry) = Evt.as_singleton ryact in
            let eiid,wxact = wx vry eiid in
            let eiid,wyact = wy vrx eiid in
            let vwx,vclwx,eswx = Evt.as_singleton wxact
            and vwy,vclwy,eswy = Evt.as_singleton wyact in
            let es =
              E.exch esrx esry eswx eswy in
            eiid,Evt.singleton((vwx,vwy),vclrx@vclry@vclwx@vclwy,es)
(* linux exchange *)
    let linux_exch : 'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t = fun rloc rexpr rmem wmem ->
      fun eiid ->
        let eiid,locm = rloc eiid in
        let eiid,expm = rexpr eiid in
        let (loc,vlcloc,esloc) =  Evt.as_singleton locm
        and (v,vclexp,esexp) = Evt.as_singleton expm in
        let eiid,rmemm = rmem loc eiid in
        let eiid,wmemm = wmem loc v eiid in
        let w,vclrmem,esrmem =  Evt.as_singleton rmemm
        and (),vclwmem,eswmem = Evt.as_singleton wmemm in
        let es = E.linux_exch esexp esloc esrmem eswmem in
        eiid,
        Evt.singleton (w,vlcloc@vclexp@vclrmem@vclwmem,es)

(* Amo, similar to exchange *)
    let amo : Op.op -> 'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t = fun op rloc rexpr rmem wmem ->
      fun eiid ->
        let eiid,locm = rloc eiid in
        let eiid,expm = rexpr eiid in
        let (loc,vlcloc,esloc) =  Evt.as_singleton locm
        and (v,vclexp,esexp) = Evt.as_singleton expm in
        let eiid,rmemm = rmem loc eiid in
        let r = V.fresh_var () in
        let eiid,wmemm = wmem loc r eiid in
        let w,vclrmem,esrmem =  Evt.as_singleton rmemm
        and (),vclwmem,eswmem = Evt.as_singleton wmemm in
        let vlop = VC.Assign (r,VC.Binop (op,w,v)) in
        let es = E.amo esexp esloc esrmem eswmem in
        eiid,
        Evt.singleton (w,vlop::vlcloc@vclexp@vclrmem@vclwmem,es)

(* Linux (successful) compexchange *)
    let linux_cmpexch_ok :
        'loc t -> 'v t -> 'v t -> ('loc -> 'v t) ->
          ('loc -> 'v -> unit t) -> ('v -> 'v -> unit t) -> 'v t =
            fun rloc rold rnew rmem wmem req eiid ->
              let eiid,locm = rloc eiid in (* read location *)
              let eiid,oldm = rold eiid in (* read old value *)
              let eiid,newm = rnew eiid in (* read new value *)
              let (loc,vlcloc,esloc) =  Evt.as_singleton locm
              and (oldv,vlcold,esold) = Evt.as_singleton oldm
              and (newv,vlcnew,esnew) = Evt.as_singleton newm in
              let eiid,rmemm = rmem loc eiid in
              let eiid,wmemm = wmem loc newv eiid in
              let w,vclrmem,esrmem =  Evt.as_singleton rmemm
              and (),vclwmem,eswmem = Evt.as_singleton wmemm in
              let es = E.linux_cmpexch_ok esloc esold esnew esrmem eswmem in
              let eiid,eqm = req oldv w eiid in
              let (),vcleq,eseq =  Evt.as_singleton eqm in
              assert (E.is_empty_event_structure eseq) ;
              eiid,
              Evt.singleton
                (w,vcleq@vlcloc@vlcold@vlcnew@vclrmem@vclwmem,es)


    let linux_cmpexch_no :
        'loc t -> 'v t -> ('loc -> 'v t) ->
          ('v -> 'v -> unit t) -> 'v t =
            fun rloc rold rmem rneq eiid ->
              let eiid,locm = rloc eiid in (* read location *)
              let eiid,oldm = rold eiid in (* read old value *)
              let (loc,vlcloc,esloc) =  Evt.as_singleton locm
              and (oldv,vlcold,esold) = Evt.as_singleton oldm in
              let eiid,rmemm = rmem loc eiid in
              let w,vclrmem,esrmem =  Evt.as_singleton rmemm in
              let es = E.linux_cmpexch_no esloc esold esrmem in
              let eiid,eqm = rneq oldv w eiid in
              let (),vcleq,_ =  Evt.as_singleton eqm in
              eiid,
              Evt.singleton
                (w,vcleq@vlcloc@vlcold@vclrmem,es)


(**************)
(* Add unless *)
(**************)

(* Success *)
    let linux_add_unless_ok rloc ra ru rmem wmem neq add ropt eiid =
      let eiid,locm = rloc eiid in (* read location *)
      let eiid,am = ra eiid in     (* read added value *)
      let eiid,um = ru eiid in     (* limit *)
      let vloc,clloc,esloc =  Evt.as_singleton locm
      and va,cla,esa = Evt.as_singleton am
      and vu,clu,esu = Evt.as_singleton um in
      let eiid,rmem = rmem vloc eiid in
      let vv,clrmem,esrmem = Evt.as_singleton rmem in
      let eiid,addm = add vv va eiid in
      let vadd,cladd,esadd = Evt.as_singleton addm in
      assert (E.is_empty_event_structure esadd) ;
      let eiid,wmem = wmem vloc vadd eiid in
      let _,clwmem,eswmem = Evt.as_singleton wmem in
      let eiid,eqm = neq vv vu eiid in
      let (),cleq,eseq = Evt.as_singleton eqm in
      assert (E.is_empty_event_structure eseq) ;
      let es = E.linux_add_unless_ok esloc esa esu esrmem eswmem (Misc.is_some ropt) in
      let r = match ropt with Some r -> r | None -> vv in
      eiid,
      Evt.singleton
        (r,cleq@cladd@clwmem@clrmem@clloc@cla@clu,es)

(* Failure *)
    let linux_add_unless_no rloc ru rmem eq ropt eiid =
      let eiid,locm = rloc eiid in (* read location *)
      let eiid,um = ru eiid in     (* limit *)
      let vloc,clloc,esloc =  Evt.as_singleton locm
      and vu,clu,esu = Evt.as_singleton um in
      let eiid,rmem = rmem vloc eiid in
      let vv,clrmem,esrmem = Evt.as_singleton rmem in
      let eiid,eqm = eq vv vu eiid in
      let (),cleq,eseq = Evt.as_singleton eqm in
      assert (E.is_empty_event_structure eseq) ;
      let es = E.linux_add_unless_no esloc esu esrmem (Misc.is_some ropt) in
      let r = match ropt with Some r -> r | None -> vv in
      eiid, Evt.singleton (r,cleq@clrmem@clloc@clu,es)

(* Store conditional, tricky dependencies *)
    let riscv_sc success read_res read_data read_addr
        cancel_res write_result write_mem eiid =
      let eiid,read_res =  read_res eiid in
      let eiid,read_data = read_data eiid in
      let eiid,read_addr = read_addr eiid in
      let resa,cl_resa,es_resa =  Evt.as_singleton read_res
      and data,cl_data,es_data =  Evt.as_singleton read_data
      and addr,cl_addr,es_addr =  Evt.as_singleton read_addr in
      let eiid,cancel_res = cancel_res eiid in
      let eiid,write_result = write_result eiid in
      let eiid,write_mem = write_mem addr resa data eiid in
      let (),cl_wres,es_wres = Evt.as_singleton cancel_res
      and (),cl_wresult,es_wresult =  Evt.as_singleton write_result
      and (),cl_wmem,es_wmem =  Evt.as_singleton write_mem in
      let es =
        E.riscv_sc success
          es_resa es_data es_addr es_wres es_wresult es_wmem in
      eiid,
      Evt.singleton ((),cl_resa@cl_data@cl_addr@cl_wres@cl_wresult@cl_wmem,es)

(* AArch64 successful cas *)
    let aarch64_cas_ok
        (read_rn:'loc t) (read_rs:'v t) (read_rt: 'v t)
        (write_rs:'v-> unit t)
        (read_mem: 'loc -> 'v t) (write_mem: 'loc -> 'v -> unit t)
        (req: 'v -> 'v -> unit t)
        eiid =
      let eiid,read_rn = read_rn eiid in
      let eiid,read_rs = read_rs eiid in
      let eiid,read_rt = read_rt eiid in
      let a,cl_a,es_rn = Evt.as_singleton read_rn
      and cv,cl_cv,es_rs = Evt.as_singleton read_rs
      and nv,cl_nv,es_rt = Evt.as_singleton read_rt in
      let eiid,read_mem = read_mem a eiid in
      let eiid,write_mem = write_mem a nv eiid in
      let ov,cl_rm,es_rm = Evt.as_singleton read_mem
      and (),cl_wm,es_wm= Evt.as_singleton write_mem in
      let eiid,write_rs = write_rs ov eiid in
      let (),cl_wrs,es_wrs = Evt.as_singleton write_rs in
      let eiid,eqm = req ov cv eiid in
      let (),cl_eq,eseq =  Evt.as_singleton eqm in
      assert (E.is_empty_event_structure eseq) ;
      let es =
        E.aarch64_cas_ok es_rn es_rs es_rt es_wrs es_rm es_wm in
      let cls = cl_a@cl_cv@cl_nv@cl_rm@cl_wm@cl_wrs@cl_eq  in
      eiid,Evt.singleton ((),cls,es)

(* Simple alternative *)
    let altT : 'a t -> 'a t -> 'a t =
      fun m1 m2 eiid ->
        let (eiid, act1) = m1 eiid in
        let (eiid, act2) = m2 eiid in
        let un =  Evt.union act1 act2 in
(*
  (Evt.map (fun (r,cs,es) -> (r,cs,es))  act1)
  (Evt.map (fun (r,cs,es) -> (r,cs, es)) act2)
 *)
        (eiid, un)

    let riscv_store_conditional read_res read_data read_addr
        cancel_res write_result write_mem =
      altT
        (riscv_sc false
           read_res read_data read_addr cancel_res
           (write_result A.V.one)
           (fun _a _resa _v -> unitT ()))
        (riscv_sc true
           read_res read_data read_addr cancel_res
           (write_result A.V.zero)
           write_mem)

(* stu combinator *)
    let stu : 'a t -> 'b t -> ('a -> unit t) -> (('a * 'b) -> unit t) -> unit t
        = fun rD rEA wEA wM  ->
          fun eiid ->
            let eiid,rd = rD eiid in
            let eiid,rea = rEA eiid in
            let (vrd,vclrd,esrd) = Evt.as_singleton rd
            and (vrea,vclrea,esrea) = Evt.as_singleton rea in
            let eiid,wea = wEA vrea eiid in
            let eiid,wm = wM (vrd,vrea) eiid in
            let (_vwea,vclwea,eswea) = Evt.as_singleton wea
            and (_vwm,vclwm,eswm) = Evt.as_singleton wm in
            let es = E.stu esrd esrea eswea eswm in
            eiid,Evt.singleton ((),vclrd@vclrea@vclwea@vclwm,es)

(* Combine the results *)
    let (>>|) : 'a t -> 'b t -> ('a * 'b)  t
        = fun s1 s2 ->
          (fun eiid ->
            let (eiid_next, s1act) = s1 eiid in
            Evt.fold (fun (v1,vcl1,es1) (eiid1,acc) ->
              let (eiid2,s2act) = s2 eiid1 in
              Evt.fold (fun (v2,vcl2,es2) (eiid3,acc_inner) ->
                match es1 =|= es2 with
                | None -> (eiid3,acc_inner)
                | Some es -> (eiid3,Evt.add ((v1,v2),vcl2@vcl1,es) acc_inner))
                s2act (eiid2, acc))
              s1act (eiid_next,Evt.empty))

(* Combine the results *)
    let (>>::) : 'a t -> 'a list t -> 'a list  t
        = fun s1 s2 ->
          (fun eiid ->
            let (eiid_next, s1act) = s1 eiid in
            Evt.fold (fun (v1,vcl1,es1) (eiid1,acc) ->
              let (eiid2,s2act) = s2 eiid1 in
              Evt.fold (fun (v2,vcl2,es2) (eiid3,acc_inner) ->
                match es1 =|= es2 with
                | None -> (eiid3,acc_inner)
                | Some es -> (eiid3,Evt.add (v1 :: v2,vcl2@vcl1,es) acc_inner))
                s2act (eiid2, acc))
              s1act (eiid_next,Evt.empty))

(* Force monad value *)
    let forceT : 'a -> 'b t -> 'a t =
      fun v s eiid ->
        let (eiid,sact) = s eiid in
        (eiid,Evt.map
           (fun (_,vcl,es) -> (v,vcl,es))
           sact)

    let (>>!) s v = forceT v s
(*      let (>>!!) s f a = forceT (f a) s *)

    let discardT : 'a t -> unit t =
      fun s eiid -> forceT () s eiid


(* Add a value *)
    let addT : 'a -> 'b t -> ('a * 'b) t
        = fun v s eiid ->
          let (eiid1,sact) = s eiid in
          (eiid1,Evt.map
             (fun (vin, vcl, es) -> ((v, vin), vcl, es))
             sact)

(* Filter by values *)
    let filterT : V.v -> V.v t -> V.v t
        = fun v s eiid ->
          let (eiid1, sact) = s eiid in
(* In concrete value world, the next line filters out only the value exactly equal as specified.
   In symbolic value world, we have a choice. The inner check can always return true, and checking
   can be postponed, OR, we can do a tiny bit of constraint solving now, and throw out the
   ones we know are impossible already *)
          let poss_with_old_cnstrnts =
            Evt.filter (fun (vin,_,_) -> V.equalityPossible vin v) sact in
          let poss_with_upd_cnstrnts =
            Evt.map (fun (vin,vcl,es) -> (vin, (VC.Assign (vin, (VC.Atom v))) :: vcl, es)) poss_with_old_cnstrnts in
          (eiid1, poss_with_upd_cnstrnts)


(* Choosing dependant upon flag,
   notice that, once determined v is either one or zero *)
    let choiceT : V.v -> 'a t -> 'a t -> 'a t =
      fun v l r eiid ->
        if V.is_var_determined v then
          if V.is_zero v  then r eiid else l eiid
        else
          let (eiid, lact) = l eiid in
          let (eiid, ract) = r eiid in
          let un =
            Evt.union
              (Evt.map (fun (r,cs,es) ->
                (r,(VC.Assign (v,VC.Atom V.one)) :: cs,es))
                 lact)
              (Evt.map
                 (fun (r,cs,es) ->
                   (r,(VC.Assign (v,VC.Atom V.zero)) :: cs, es))
                 ract) in
          (eiid, un)

    let (|*|) : unit t -> unit t -> unit t
        = fun s1 s2 ->
          (fun eiid ->
            let (eiid_next, s1act) = s1 eiid in
            let (eiid_final, s2act) = s2 eiid_next in
            let s1lst = Evt.elements s1act in
            let s2lst = Evt.elements s2act in
            (eiid_final,
             if Evt.is_empty s2act then s1act
             else if Evt.is_empty s1act then s2act
             else
               List.fold_left
                 (fun acc (_,vcla,evta) ->
                   List.fold_left
                     (fun acc_inner (_,vclb,evtb) ->
                       match evta +|+ evtb with
                       | Some evtc -> Evt.add ((), vcla@vclb, evtc) acc_inner
                       | None      -> acc_inner
                     )
                     acc s2lst
                 )
                 Evt.empty s1lst))

(* For combining instruction + next instructions.
   Notice: no causality from s to f v1 *)
    let (>>>) s f = fun eiid ->
      let (eiid_next, sact) = s eiid in
      Evt.fold
        (fun (v1, vcl1, es1) (eiid1,acc) ->
          let b_set = f v1 in
          let eiid_b,b_setact = b_set eiid1 in
          Evt.fold
            (fun (v2,vcl2,es2) (eiid2,acc_inner) ->
              match es1 +|+ es2 with
              | None -> eiid2, acc_inner
              | Some es ->
                  eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
            b_setact (eiid_b,acc))
        sact (eiid_next,Evt.empty)

(* For combining conditions and branches of an if,
   as above + instruction dependencies *)
    let (>>>>) s f = fun eiid ->
      let (eiid_next, sact) = s eiid in
      Evt.fold
        (fun (v1, vcl1, es1) (eiid1,acc) ->
          let b_set = f v1 in
          let eiid_b,b_setact = b_set eiid1 in
          Evt.fold
            (fun (v2,vcl2,es2) (eiid2,acc_inner) ->
              let es = E.cond_comp es1 es2 in
              eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
            b_setact (eiid_b,acc))
        sact (eiid_next,Evt.empty)



(* trivial event_structure with just one event
   and no relation *)

    let do_trivial es =
      { E.empty_event_structure with E.events = es ; }


    let trivial_event_structure is_data e =
      let es = E.EventSet.singleton e in
      let st = do_trivial es in
      if is_data then
        { st with E.data_ports = st.E.events; }
      else st

    let read_loc is_data mk_action loc ii =
      fun eiid ->
        V.fold_over_vals
          (fun v (eiid1,acc_inner) ->
            (eiid1+1,
             Evt.add
               (v, [],
                trivial_event_structure is_data
                  {E.eiid = eiid1 ;
                   E.iiid = Some ii;
                   E.action = mk_action loc v })
               acc_inner)) (eiid,Evt.empty)

    let mk_singleton_es a ii =
      fun eiid ->
        (eiid+1,
         Evt.singleton
           ((), [],
            trivial_event_structure false
              {E.eiid = eiid ;
               E.iiid = Some ii;
               E.action = a }))

    let mk_singleton_es_success =
      fun a ii ->
        fun eiid ->
          (eiid+1,
           Evt.singleton
             ((), [],
              let str =
                trivial_event_structure false
                  {E.eiid = eiid ;
                   E.iiid = Some ii;
                   E.action = a } in
              { str with E.success_ports=str.E.events; }))

    let mk_fence a ii =
      fun eiid ->
        (eiid+1,
         Evt.singleton
           ((), [],
            let es =
              trivial_event_structure false
                {E.eiid = eiid ;
                 E.iiid = Some ii;
                 E.action = a } in
            { es with E.output = Some E.EventSet.empty; }))
(*
  let mk_singleton_es_eq a x rr y ii =
  fun eiid ->
  (eiid+1,
  Evt.singleton
  ((), [VC.Assign (x, VC.Atom y); VC.Assign (rr,VC.Atom V.one)],
  trivial_event_structure
  {E.eiid = eiid ;
  E.iiid = Some ii;
  E.action = a }))
 *)
    let mk_singleton_es_eq a eqs ii =
      fun eiid ->
        (eiid+1,
         Evt.singleton
           ((), eqs,
            trivial_event_structure false
              {E.eiid = eiid ;
               E.iiid = Some ii;
               E.action = a }))


    let any_op mk_v mk_c =
      (fun eiid_next ->
        eiid_next,
        begin try
          let v = mk_v () in
          Evt.singleton
            (v, [], E.empty_event_structure)
        with V.Undetermined ->
          let v = V.fresh_var () in
          Evt.singleton
            (v, [VC.Assign (v, mk_c ())], E.empty_event_structure)
        end)

    let op1 op v1 =
      any_op
        (fun () -> V.op1 op v1)
        (fun () -> VC.Unop (op,v1))

    and op op v1 v2 =
      any_op
        (fun () -> V.op op v1 v2)
        (fun () ->
          match op with
          | Op.Xor when V.compare v1 v2 = 0 -> VC.Atom V.zero
          | _ -> VC.Binop (op,v1,v2))

    and op3 op v1 v2 v3 =
      any_op
        (fun () -> V.op3 op v1 v2 v3)
        (fun () -> VC.Terop (op,v1,v2,v3))

    let add =  op Op.Add

    let assign v1 v2 =
      fun eiid ->
        eiid,
        Evt.singleton
          ((), [(VC.Assign (v1,VC.Atom v2))],
           E.empty_event_structure)


(**************)
(* Mixed size *)
(**************)

    module Scalar = V.Cst.Scalar
    let def_size = Scalar.machsize

    let extract_byte v = VC.Unop (Op.AndK A.mask,v)

    let extract_step v =
      let d = extract_byte v
      and w = VC.Unop (Op.LogicalRightShift A.nshift,v) in
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
      do_rec (A.nsz sz) v

(* Translate from list of bytes  least significant first *)
    let rec recompose ds = match ds with
    | [] -> assert false
    | [d] -> d,[]
    | d::ds ->
        let w,eqs = recompose ds in
        let vw = V.fresh_var ()
        and x =  V.fresh_var () in
        vw,VC.Assign (x,VC.Unop (Op.LeftShift A.nshift,w))::VC.Assign (vw,VC.Binop (Op.Or,x,d))::eqs

(* Bytes addresses, little endian *)

    let byte_eas sz a =
      let kmax = A.nsz sz in
      let rec do_rec k =
        if k >= kmax then [],[]
        else
          let xa = V.fresh_var() in
          let xas,eqs = do_rec (k+1) in
          xa::xas,VC.Assign (xa,VC.Unop (Op.AddK (k*A.byte_sz),a))::eqs in
      let xas,eqs = do_rec 1 in
      let xas = a::xas in
      let open Endian in
      match A.endian with
      | Little -> xas,eqs
      | Big -> List.rev xas,eqs

    let read_mixed is_data sz mk_act a ii =
      fun eiid ->
        let eas,a_eqs = byte_eas sz a in
        let eavs = List.map (fun ea -> ea,V.fresh_var ()) eas in
        let vs = List.map snd eavs in
        let v,v_eqs = recompose vs in
        let eiid,es =
          List.fold_left
            (fun (eiid,es) (ea,v) ->
              eiid+1,
              E.EventSet.add
                {E.eiid = eiid;
                 E.iiid = Some ii;
                 E.action = mk_act A.byte (A.Location_global ea) v;} es)
            (eiid,E.EventSet.empty) eavs  in
        let e_full =
          { E.eiid=eiid; E.iiid = Some ii;
            E.action = mk_act sz (A.Location_global a) v; } in
        let st =
          { E.empty_event_structure with
            E.events = es;
            E.data_ports = if is_data then es else E.EventSet.empty;
            E.sca = E.EventSetSet.singleton es;
            E.mem_accesses = E.EventSet.singleton e_full;} in
        eiid+1,Evt.singleton (v,a_eqs@v_eqs,st)

    let write_mixed sz mk_act a v ii =
      fun eiid ->
        let eas,a_eqs = byte_eas sz a
        and vs,v_eqs = explode sz v in
        let eiid,es =
          List.fold_left2
            (fun (eiid,es) ea v ->
              eiid+1,
              E.EventSet.add
                {E.eiid = eiid;
                 E.iiid = Some ii;
                 E.action = mk_act A.byte (A.Location_global ea) v;} es)
            (eiid,E.EventSet.empty) eas vs in
         let st =
          { E.empty_event_structure with
            E.events = es;
            E.sca = E.EventSetSet.singleton es;} in
        eiid,
         Evt.singleton ((),a_eqs@v_eqs,st)

(* Memory tagging *)

   let last_byte = (8/A.byte_sz)-1 (* dernier octet, 7 pour A.byte = Byte *)

   let write_PA_tag mk_act a v ii =
     fun eiid ->
        let a7 = V.fresh_var() in (* Pour l'addresse du dernier byte de a *)
        let a7_eq = VC.Assign (a7,VC.Unop (Op.AddK (last_byte *A.byte_sz),a)) in
          let eas,a_eqs = byte_eas A.byte a7
          and vs,v_eqs = explode A.byte v in
          let eiid,es =
            List.fold_left2
              (fun (eiid,es) ea v ->
                eiid+1,
                E.EventSet.add
                  {E.eiid = eiid;
                   E.iiid = Some ii;
                   E.action = mk_act A.byte (A.Location_global ea) v;} es)
              (eiid,E.EventSet.empty) eas vs in
           let st =
            { E.empty_event_structure with
              E.events = es;
              E.sca = E.EventSetSet.singleton es;} in
          eiid,
          Evt.singleton ((),a7_eq::a_eqs@v_eqs,st)

   let get_alloc_tag a = op1 Op.TagLoc a

    let get_alloc_tag_val mk_act a ii =
      read_mixed false A.byte mk_act a ii

   let set_tag mk_act a v ii =
       write_mixed A.byte mk_act a v ii

(* Add an inequality constraint *)
    let neqT : V.v -> V.v -> unit t
        = fun v1 v2 ->
          op Op.Eq v1 v2 >>= fun v -> assign v V.zero

    let eqT : V.v -> V.v -> unit t = assign


    let fetch op arg mk_action ii =
      fun eiid ->
        V.fold_over_vals
          (fun v (eiid1,acc_inner) ->
            let vstored = V.fresh_var () in
            (eiid1+1,
             Evt.add
               (v, [VC.Assign (vstored,VC.Binop (op,v,arg))],
                trivial_event_structure false
                  {E.eiid = eiid1 ;
                   E.iiid = Some ii;
                   E.action = mk_action v vstored})
               acc_inner)) (eiid,Evt.empty)

    let tooFar _msg = zeroT
(*
  fun eiid ->
  eiid,
  Evt.singleton
  ((), [VC.Unroll msg],E.empty_event_structure)
 *)

    type evt_struct = E.event_structure
    type output = VC.cnstrnts * evt_struct

    let initwrites_non_mixed env _ =
      fun eiid ->
        let eiid,es =
          List.fold_left
            (fun (eiid,es) (loc,v) ->
              let ew =
                {E.eiid = eiid ;
                 E.iiid = None ;
                 E.action = E.Act.mk_init_write loc def_size v ;} in
              (eiid+1,ew::es))
            (eiid,[]) env in
        let es = E.EventSet.of_list es in
(*        Printf.eprintf "Init writes %a\n" E.debug_events es; *)
        eiid,
        Evt.singleton ((),[],do_trivial es)

    let initwrites_mixed env size_env =
      fun eiid ->
        try
          let eiid,es,sca =
          List.fold_left
            (fun (eiid,es,sca) (loc,v) ->
              match loc with
              | A.Location_global (A.V.Val (Constant.Symbolic ((s,_),0)) as a) ->
                  let sz = A.look_size size_env s in
                  let ds = A.explode sz v
                  and eas = A.byte_eas sz a in
                  let eiid,ews =
                    List.fold_left2
                      (fun (eiid,ews) a d ->
                        let ew =
                          { E.eiid = eiid ;
                            E.iiid = None ;
                            E.action =
                            E.Act.mk_init_write (A.Location_global a) A.byte d ;} in
                        eiid+1,ew::ews)
                      (eiid,[]) eas ds in
                  eiid,ews@es, E.EventSetSet.add (E.EventSet.of_list ews) sca
              | _ ->
                  let ew =
                    {E.eiid = eiid ;
                     E.iiid = None ;
                     E.action = E.Act.mk_init_write loc def_size v ;} in
                  (eiid+1,ew::es,
                   E.EventSetSet.add (E.EventSet.singleton ew) sca))
            (eiid,[],E.EventSetSet.empty) env in
        let es = E.EventSet.of_list es in
(*        Printf.eprintf "Init writes %a\n" E.debug_events es; *)

        let st = do_trivial es in
        let st = { st with E.sca; } in
        eiid,
        Evt.singleton ((),[],st)
        with
        | V.Undetermined -> assert false

    let mixed = C.variant Variant.Mixed

    let initwrites =  if mixed then initwrites_mixed else initwrites_non_mixed

    let get_output =
      fun et ->
        let (_,es) = et 0 in
        List.map (fun (_,vcl,evts) -> (vcl,evts)) (Evt.elements es)
  end
