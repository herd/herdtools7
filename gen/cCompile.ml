(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* 'C' compiler *)

(* Compared with hardware archs, XXXCompile and top are merged... *)

open Printf
open Code

module type Config = sig
  include Top.Config
  include Cycle.Config
  val list_edges : bool
  val typ : TypBase.t
  val cpp : bool
  val docheck : bool
end

module Make(O:Config) : Builder.S
    = struct

      let () =
       if O.optcond then
          Warn.warn_always "optimised conditions are not supported by C arch"

      module O = struct
        include O
        let optcond = false
      end

      module A = struct
        include CArch
        let deftype = O.typ
      end
      module E = Edge.Make(A)
      let () =
        if O.list_edges then begin
          eprintf "Edges:" ;
          let es = E.fold_edges (fun e k -> E.pp_edge e::k) [] in
          let es = List.sort String.compare es in
          List.iter (eprintf " %s") es ;        
          eprintf "\n%!" ;
          exit 0
        end

      module R = Relax.Make(A)(E)
      module C = Cycle.Make(O)(E)

      module AR = struct
        module A = A
        module E = E
        module R = R
        module C = C
      end

      module U = TopUtils.Make(O)(AR)
      module F = Final.Make(O)(AR)

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

(* Typing *)
      let type_event env e =
        let loc = e.C.loc in
        let ty = match e.C.atom with
        | Some _ -> A.Atomic A.deftype
        | None  -> A.Plain A.deftype in
        try
          let ty0 = StringMap.find loc env in
          if ty0 <> ty then
            Warn.fatal "Type mismatch on location %s" loc ;
          env
        with
        | Not_found -> StringMap.add loc ty env

      let type_cycle n =
        let rec do_rec env m =
          let env = type_event env m.C.evt in
          if m.C.next == n then env
          else do_rec env m.C.next in
        do_rec StringMap.empty n

(* Basics *)
      let st0 = 0

      let reset_alloc,alloc_reg =
        if O.cpp then
          let c = ref 0 in
          (fun () -> c := 0),
          (fun _p st ->
            let c0 = !c in
            incr c ;
            { A.id = c0; },st+1)
        else
          (fun () -> ()),(fun _p st -> { A.id=st;},st+1)

      let compile_store e = 
        let v = e.C.v in
        let loc = e.C.loc in
        match e.C.atom with
        | None ->
            A.Store (loc,A.Const v)
        | Some a ->
            if A.applies_atom a Code.W then
              A.AtomicStore(a,loc,A.Const v)
            else Warn.fatal "wrong memory order for store"

      let load_checked_not = None

      let load_from mo loc = match mo with
      | None ->
          if O.cpp then A.Load loc
          else A.Deref (A.Load loc)
      | Some mo ->
          if A.applies_atom mo Code.R then
            A.AtomicLoad (mo,loc)
          else Warn.fatal "wrong memory order for load"

      let excl_from omo loc v = match omo with
      | None -> Warn.fatal "Non atomic RMW"
      | Some mo ->
          if A.applies_atom_rmw omo omo then
            A.AtomicExcl (mo,loc,v)
          else
            Warn.fatal "wrong memory order for atomic exchange"

      let _load_from_event e = load_from e.C.atom (A.Loc e.C.loc)

      let compile_load st p mo loc =
        let r,st = alloc_reg p st in
        let i =  A.Decl (A.Plain A.deftype,r,Some (load_from mo loc)) in
        r,i,st

      let compile_excl st p mo loc v =
        let r,st = alloc_reg p st in
        let i =  A.Decl (A.Plain A.deftype,r,Some (excl_from mo loc v)) in
        r,i,st

      let assertval mo loc v =
        if O.cpp then
          A.AssertVal(load_from mo loc,v)
        else load_from mo loc

      let assert_excl v1 mo loc v2 =
         if O.cpp then
          A.AssertVal(excl_from mo loc v2 ,v1)
        else excl_from mo loc v2


      let compile_load_assertvalue v st p mo loc =
        let r,st = alloc_reg p st in
        let i = 
          A.Decl
            (A.Plain A.deftype,r,
             Some (assertval mo loc v)) in
        r,i,st

      let compile_excl_assertvalue v1 st p mo loc v2 =
        let r,st = alloc_reg p st in
        let i =
          A.Decl
            (A.Plain A.deftype,r,
             Some (assert_excl v1 mo loc v2)) in
        r,i,st


      let do_breakcond cond p r v =
        A.If ((v,cond,A.Load (A.Reg (p,r))),A.Break,None)

      let breakcond cond p r v =
        do_breakcond cond p r (A.Const v)

      let compile_load_not_zero st p mo x =
        let r,st = alloc_reg p st in
        let decls = A.Decl (A.Plain A.deftype,r,None)
        and body =
          A.Seq
            (A.SetReg (r,load_from mo x),breakcond A.Eq p r 0) in
        r,A.Seq (decls,A.Loop body),st

      let compile_load_one st p mo x =
        let r,st = alloc_reg p st in
        let decls = A.Decl (A.Plain A.deftype,r,None)
        and body =
          A.Seq
            (A.SetReg (r,load_from mo x),
             breakcond A.Eq p r 1) in
        r,A.Seq (decls,A.Loop body),st


      let do_compile_load_not st p mo x e =
        let idx,st = alloc_reg p st in
        let r,st = alloc_reg p st in
        let decls =
          A.Seq
            (A.Decl (A.Plain TypBase.Int,idx,Some (A.Const 200)),
             A.Decl (A.Plain A.deftype,r,None))
        and body =
          A.seqs
            [A.SetReg (r,load_from mo x) ;
             do_breakcond A.Ne p r e ;
             A.Decr idx ;
             breakcond A.Eq p idx 0;] in
        r,A.Seq (decls,A.Loop body),st
          
      let compile_load_not_value st p mo x v =
        do_compile_load_not st p mo x (A.Const v)

      let compile_load_not_eq st p mo x r =
        do_compile_load_not st p mo x (A.Load (A.Reg (p,r)))


      let compile_access st p n =
        let e = n.C.evt in
        if e.C.rmw then match  e.C.dir with
        | R ->
            let v = n.C.next.C.evt.C.v in
            let loc = A.Loc e.C.loc in
            let mo = e.C.atom in
            let r,i,st =
              compile_excl_assertvalue e.C.v st p mo loc v in
            Some r,i,st
        | W -> None,A.Nop,st
        else match e.C.dir with
        | R ->
            let loc = A.Loc e.C.loc in
            let mo = e.C.atom in
            let r,i,st =
              (if U.do_poll n then compile_load_one
              else compile_load_assertvalue e.C.v)
                st p mo loc in
            Some r,i,st
        | W ->
            let i = compile_store e in
            None,i,st


(* Lift definitions *)
      module RegSet =
        MySet.Make
          (struct
            type t = A.arch_reg
            let compare = Pervasives.compare
          end)

      let insert_now d i =
        List.fold_right
          (fun (t,r) k ->
            A.seqs [A.Decl (t,r,Some (A.Const (-1)));k])
          d i

      let rec lift_rec top xs i =
        let open A in
        match i with
        | Decl (t,r,None) ->
            if not top && RegSet.mem r xs then [t,r],Nop
            else [],i
        | Decl (t,r,Some e) ->
            if not top && RegSet.mem r xs then
              [t,r],SetReg (r,e)
            else
              [],i
        | Seq (i1,i2) ->
            let d1,i1 = lift_rec top xs i1
            and d2,i2 = lift_rec top xs i2 in
            d1@d2,seqs [i1;i2]
        | Loop i ->
            let d,i = lift_rec false xs i in
            if top then
              [],insert_now d (Loop i)
            else
              d,Loop i
        | If (ce,itrue,ifalse) ->
            let dtrue,itrue = lift_rec false xs itrue
            and dfalse,ifalse = match ifalse with
            | None -> [],None
            | Some ifalse ->
                let dfalse,ifalse = lift_rec false xs ifalse in
                dfalse,Some ifalse in
            let d = dtrue@dfalse in
            if top then
              [],insert_now d (If (ce,itrue,ifalse))
            else
              d,If (ce,itrue,ifalse)
        | Store _|AtomicStore _|SetReg _
        | Fence _|Break|Decr _|Nop -> [],i

      let lift_top xs i =
        let d,i = lift_rec true xs i in
        match d with
        | [] -> i
        | _ -> assert false

      let lift_defs xs i =
        if O.cpp then i
        else
          let regs =
            List.fold_right
              (fun loc k -> match loc with
              | A.Loc _ -> k
              | A.Reg (_,r) -> r::k)
              xs [] in
          let xs = RegSet.of_list regs in
          lift_top xs i

(*************)
(* Observers *)
(*************)

      let add_fence fenced ins = match fenced,ins with
      | (false,_)|(_,A.Nop) -> ins
      | _ -> A.Seq (A.Fence A.strong,ins)

      let rec straight_observer_std fenced st p mo x = function
        | [] -> A.Nop,[]
        | v::vs ->
            let r,c,st =
              compile_load_assertvalue 
                (IntSet.choose v) st p mo x  in
            let cs,fs = straight_observer_std fenced st p  mo x vs in
            A.seq c (add_fence fenced cs),F.add_final_v p r v fs

      let rec straight_observer_check fenced st p mo x = function
        | [] -> assert false (* A.Nop,[] *)
        | [_] as vs -> straight_observer_std fenced st p mo x vs
        | v::vs ->
            let v0 = IntSet.choose v in
            if O.cpp then
              let ce = A.Const v0,A.Eq,assertval mo x v0 in
              let cs,fs = straight_observer_check fenced st p  mo x vs in
              A.If (ce,add_fence fenced cs,load_checked_not),fs
            else
              let r,i,st = compile_load st p mo x in
              let ce = A.Const v0,A.Eq,A.Load (A.Reg (p,r)) in
              let cs,fs = straight_observer_check fenced st p  mo x vs in
              A.Seq (i,A.If (ce,add_fence fenced cs,load_checked_not)),
              F.add_final_v p r v fs

      let  straight_observer_check_lift fenced st p mo x i =
        let i,f = straight_observer_check fenced st p mo x i in
        let xs = List.map fst f in
        lift_defs xs i,f

      let straight_observer =
        (if O.docheck then straight_observer_check_lift
        else straight_observer_std) false

      let fenced_observer =
        (if O.docheck then straight_observer_check_lift
        else straight_observer_std) true

      let loop_observer st p mo x = function
        | []|[_] -> A.Nop,[]    
        | v::vs ->
            let r,c,st = compile_load_not_zero st p mo x in
            let rec do_loop st prev_r = function
              | [] ->  assert false
              | [v] ->
                  let r,c,_st = compile_load_not_eq st p mo x prev_r in
                  c,F.add_final_v p r v []
              | v::vs ->
                  let r,c,st = compile_load_not_eq st p mo x prev_r in
                  let cs,fs = do_loop st r vs in
                  A.seq c cs,F.add_final_v p r v fs in
            let cs,fs = do_loop st r vs in
            A.seq c cs,F.add_final_v p r v fs

      let rec split_last = function
        | [] -> assert false
        | [v] -> [],v 
        | v::vs ->
            let vs,w = split_last vs in
            v::vs,w

      let rec do_opt_coherence k obs = function
        | [] -> [k]
        | (v,vobs)::co ->
            let i = IntSet.inter obs vobs in
            if IntSet.is_empty i then begin
              k::
              do_opt_coherence (IntSet.singleton v) vobs co
            end else
              do_opt_coherence (IntSet.add v k) vobs co

      let opt_coherence = function
        | [] -> assert false
        | (v,obs)::co ->
            do_opt_coherence (IntSet.singleton v) obs co

      let min_set =
        if O.coherence_decreasing then IntSet.max_elt
        else IntSet.min_elt

      let max_set =
        if O.coherence_decreasing then IntSet.min_elt
        else IntSet.max_elt

      let min_max xs =
        let ps = List.map (fun x -> min_set x, max_set x) xs in
        match ps with
        | []|[_] -> []
        | (_,x)::rem ->
            let rec remove_last = function
              | [] -> assert false
              | [x,_] -> [x]
              | (x,y)::rem ->
                  if x=y then x::remove_last rem
                  else x::y::remove_last rem in
            List.map IntSet.singleton (x::remove_last rem)

              

      exception NoObserver

      let build_observer st p mo x vs =
        let vs,f =
          if O.optcoherence && O.obs_type <> Config.Loop then
            let vs = opt_coherence vs in
            if O.verbose > 1 then begin
              eprintf "OPT:" ;
              List.iter
                (fun vs ->
                  eprintf " {%s}" (IntSet.pp_str "," (sprintf "%i") vs))
                vs ;
              eprintf "\n%!"
            end ;
            match vs with
            | []|[_] -> raise NoObserver
            | _ ->
                if
                  List.for_all
                    (fun x ->
                      match IntSet.as_singleton x with
                      | Some _ -> true | None -> false)
                    vs then
                  let ws,w = split_last vs in
                  (match ws with [_] -> [] | _ -> ws),[A.Loc x, w]
                else
                  min_max vs,[]
          else
            let vs = List.map (fun (v,_obs) -> IntSet.singleton v) vs in
            vs,[] in
        let cs,fs =
          let open Config in
          let x = A.Loc x in
          match O.obs_type with
          | Straight ->  straight_observer st p  mo x vs
          | Config.Fenced -> fenced_observer st p  mo x vs
          | Loop -> loop_observer st p mo x vs in
        cs,fs@f

      let rec build_observers p mo x arg =
        let open Config in
        match arg,O.do_observers with
        | [],_ -> [],[]
        | []::vss,_
        | [_]::vss,(Avoid|Accept) ->
            build_observers p mo x vss
        | vs::vss,_ ->
            try 
              let c,f = build_observer st0 p mo x vs in
              let is_nop = A.is_nop c in
              begin match is_nop,O.do_observers with
              | false,Avoid  -> Warn.fatal "Observer"
              | _,_ -> ()
              end ;
              if is_nop then
                let cs,fs = build_observers p mo x vss in
                cs,f@fs
              else
                let cs,fs = build_observers (p+1) mo x vss in
                c::cs,f@fs
            with NoObserver -> build_observers p mo x vss
                
      let rec check_rec env p =
        let add_look_loc loc v k =
          if O.optcond then k else (A.Loc loc,IntSet.singleton v)::k in
        let open Config in
        function
          | [] -> [],[]
          | (x,vs)::xvs ->
              let mo =
                try match StringMap.find x env with
                | A.Plain _ -> None
                | A.Atomic _ -> Some MemOrder.Rlx
                with
                | Not_found -> assert false in
              let c,f = match O.cond with
              | Observe ->
                  let vs = List.flatten vs in
                  begin match vs with
                  | [] -> [],[]
                  | _::_ ->
                      let v,_ = Misc.last vs in
                      [],[A.Loc x,IntSet.singleton v]
                  end
              | Unicond -> assert false
              | Cycle -> begin
                  match vs with
                  | []|[[_]] -> [],[]
                  | [[_;(v,_)]] ->
                      begin match O.do_observers with
                      | Local -> [],add_look_loc x v []
                      | Avoid|Accept -> [],[A.Loc x,IntSet.singleton v]
                      | Enforce ->  
                          let c,f = build_observers p mo x vs in
                          c,add_look_loc x v f
                      end
                  | _ ->
                      let v =
                        let v,_ = Misc.last (List.flatten vs) in
                        v in
                      begin match O.do_observers with
                      | Local -> [],add_look_loc x v []
                      | _ ->
                          let c,f = build_observers p mo x vs in
                          c,add_look_loc x v f
                      end
              end in
              let cs,fs =
                check_rec env (p+List.length c) xvs in
              c@cs,f@fs

      let check_writes p cos = check_rec p cos

(* Local check of coherence *)

      let do_add_load st p f mo x v =
        let r,c,st = compile_load_assertvalue v st p mo x in
        c,F.add_final_v p r (IntSet.singleton v) f,st

      let do_add_loop st p f mo x v w =
        let r,c,st = compile_load_not_value st p mo x v in
        c,F.add_final_v p r (IntSet.singleton w) f,st


      let do_observe_local st p code f mo x prev_v v =
        let mo = match mo with
        | Some _ -> Some MemOrder.Rlx
        | None -> None in
        let open Config in
        match O.obs_type with
        | Straight ->
            let c,f,st = do_add_load st p f mo x v in
            A.Seq (code,c),f,st
        | Config.Fenced ->
            let c,f,st = do_add_load st p f mo x v in
            A.Seq (code,A.Seq (A.Fence A.strong,c)),f,st
        | Loop ->
            let c,f,st = do_add_loop st p f mo x prev_v v in
            A.Seq (code,c),f,st

      let add_co_local_check lsts ns st p code f =
        let lst = Misc.last ns in
        if U.check_here lst then
          let elst = lst.C.evt in
          let x = elst.C.loc and v = lst.C.next.C.evt.C.v
          and prev_v = elst.C.v in
          let all_lst =
            try StringMap.find x lsts
            with Not_found -> C.evt_null in
          if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
          then
            code,(A.Loc x,IntSet.singleton v)::f,st
          else
            let mo = match elst.C.atom with
            | Some _ -> Some MemOrder.Rlx
            | None   -> None in
            do_observe_local st p code f mo (A.Loc x) prev_v v
        else
          code,f,st

(* Detours *)

      let do_observe_local_before st p code f mo x prev_v v =
        if O.optcoherence && v = 0 then
          code,[],st
        else
          let x = A.Loc x in
          let open Config in
          match O.obs_type with
          | Straight|Config.Fenced ->
              let c,f,st = do_add_load st p f mo x v in
              A.Seq (code,c),f,st
          | Loop ->
              let c,f,st = do_add_loop st p f mo x prev_v v in
              A.Seq (code,c),f,st
                

      let build_detour lsts st p n =
        let open Config in
        let c0,f,st = match O.do_observers with
        | Local ->
            let e = n.C.evt in
            let mo = e.C.atom in
            begin match n.C.edge.E.edge with
            | E.DetourWs (Dir W) ->          
                do_observe_local_before st p A.Nop [] mo n.C.evt.C.loc
                  n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
            | E.DetourWs (Dir R) ->          
                do_observe_local_before st p A.Nop [] mo n.C.evt.C.loc
                  n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
            | _ -> A.Nop,[],st 
            end
        | _ -> A.Nop,[],st in

        let _,c,st = compile_access st p n in
        let c = A.seq c0 c in
        match O.do_observers with
        | Local ->
            let c,f,_st = add_co_local_check lsts [n] st p c f in
            c,f
        | _ -> c,f

      let rec build_detours lsts p ns = match ns with
      | [] -> [],[]
      | n::ns ->
          let c,f = build_detour lsts st0 p n in
          let cs,fs = build_detours lsts (p+1) ns in
          c::cs,f@fs


(* zyva *)
      let rec compile_stores ns k = match ns with
      | [] -> k
      | n::ns ->
          let sto = n.C.store in
          if sto == C.nil then
            compile_stores ns k
          else
            let i = compile_store sto.C.evt in
            let k = compile_stores ns k in
            A.Seq (i,k)

      let add_fence n is = match n.C.edge.E.edge with
      | E.Fenced (fe,_,_,_) -> A.Seq (A.Fence fe,is)
      | _ -> is


      let do_observe_local st p (m,f) mo x pv v =
        let mo = match mo with
        | Some _ -> Some MemOrder.Rlx
        | None -> None in
        let open Config in
        match O.obs_type with
        | Straight ->
            let c,f,st = do_add_load st p f mo x v in
            c,(m,f),st
        | Config.Fenced ->
            let c,f,st = do_add_load st p f mo x v in
            A.Seq (A.Fence A.strong,c),(m,f),st
        | Loop ->
            let c,f,st = do_add_loop st p f mo x pv v in
            c,(m,f),st

      let observe_local st p f n =
        let open Config in
        match O.do_observers with
        | Local when U.check_here n ->
            let e = n.C.evt in
            do_observe_local st p f e.C.atom (A.Loc e.C.loc) e.C.v
              n.C.next.C.evt.C.v
        | _ -> A.Nop,f,st
              
      let observe_local_check st _p f n =
        let open Config in
        match O.do_observers with
        | Local when U.check_here n ->
            Warn.fatal "Local observers not complete in mode -check true"
        | _ -> (fun is -> is),f,st

      let rec compile_proc_std loc_writes st p ns = match ns with
      | [] ->
          let f = C.EventMap.empty,[] in
          A.Nop,f,st
      | n::ns ->
          let o,i,st = compile_access st p n in
          let is,fs,st = compile_proc_std loc_writes st p ns in
          let obs,fs,st = observe_local st p fs n in
          A.seqs [i;obs;add_fence n is],
          (if not (U.do_poll n) then
            F.add_final p o n fs
          else fs),
          st

      let rec do_compile_proc_check loc_writes st p ns = match ns with
      | [] -> assert false (* A.Nop,(C.EventMap.empty,[]),st *)
      | [_] -> compile_proc_std loc_writes st p ns
      | n::ns ->
          let e = n.C.evt in
          let o,fi,st =
            if e.C.rmw then match  e.C.dir with
            | R ->
                let vw = n.C.next.C.evt.C.v
                and mo = e.C.atom
                and loc = A.Loc e.C.loc
                and v = e.C.v in
                if O.cpp then
                  let ce = A.Const v,A.Eq,assert_excl vw mo loc v in
                  None,
                  (fun ins -> A.If (ce,add_fence n ins,load_checked_not)),
                  st
                else
                  let r,i,st = compile_excl st p mo loc vw in
                  let ce = A.Const v,A.Eq,A.Load (A.Reg (p,r)) in
                  Some r,
                  (fun ins ->
                    A.Seq
                      (i,A.If (ce,add_fence n ins,load_checked_not))),
                  st
            | W -> None,add_fence n,st
            else begin match e.C.dir with
            | R ->
                let v = e.C.v
                and mo = e.C.atom
                and loc = A.Loc e.C.loc in
                if O.cpp then
                  let ce = A.Const v,A.Eq,assertval mo loc v in
                  None,
                  (fun ins -> A.If (ce,add_fence n ins,load_checked_not)),
                  st
                else
                  let r,i,st = compile_load st p mo loc in
                  let ce = A.Const v,A.Eq,A.Load (A.Reg (p,r)) in
                  Some r,
                  (fun ins ->
                    A.Seq
                      (i,A.If (ce,add_fence n ins,load_checked_not))),
                  st
            | W ->
                None,
                (fun ins -> A.Seq (compile_store e,add_fence n ins)),
                st
            end in
          let is,fs,st = do_compile_proc_check loc_writes st p ns in
          let obs,fs,st = observe_local_check st p fs n in
          fi (obs is),
          (if true then F.add_final p o n fs
          else fs),
          st

      let compile_proc_check loc_writes st p ns =
        let i,(_,f as mf),st = do_compile_proc_check loc_writes st p ns in
        lift_defs (List.map fst f) i,mf,st


      let add_args env prog =
        List.map
          (fun i ->
            let locs = A.addrs_of i in
            let args =
              StringSet.fold
                (fun x k ->
                  let t =
                    try StringMap.find x env
                    with Not_found -> assert false in
                  (t,x)::k)
                locs [] in
            args,i)
          prog
          
          
      let compile_cycle ok n =
        reset_alloc () ;
        let env = type_cycle n in        
        let open Config in
        let splitted =  C.split_procs n in
        (* Split before, as  proc numbers added by side effet.. *)
        let cos0 = C.coherence n in    
        let lsts = U.last_map cos0 in
        let cos = U.compute_cos cos0 in
        if O.verbose > 1 then U.pp_coherence cos0 ;
        let loc_writes = U.comp_loc_writes n in

        let rec do_rec p = function
          | [] -> [],(C.EventMap.empty,[]),[]
          | n::ns ->
              let c,(m,f),_st =
                (if O.docheck then compile_proc_check else  compile_proc_std)
                  loc_writes st0 p n in
              let c = compile_stores n c in
              let ds = C.get_detours_from_list n in
              let cds,fds = build_detours lsts (p+1) ds in
              let cs,(ms,fs),ios = do_rec (p+1+List.length cds) ns in
              let io = U.io_of_thread n in
              let iod = List.map U.io_of_detour ds in
              c::(cds@cs),(C.union_map m ms,f@fds@fs),(io::iod)@ios in

        let obsc,f =
          match O.cond with
          | Unicond -> [],[]
          | Cycle|Observe -> check_writes env 0 cos in
        match splitted,O.cond with
        | [],_ -> Warn.fatal "No proc"
(*    | [_],Cycle -> Warn.fatal "One proc" *)
        | _,_ ->
            let c,(m,f),ios =
              if
                let len =  List.length splitted in
                O.nprocs <= 0 ||
                (if O.eprocs then len = O.nprocs else len <= O.nprocs)
              then
                let ess = List.map (List.map (fun n -> n.C.edge)) splitted in
                if ok ess then
                  let cs,(m,fs),ios = do_rec (List.length obsc) splitted in
                  obsc@cs,(m,f@fs),ios
                else Warn.fatal "Last minute check"
              else  Warn.fatal "Too many procs" in
            let f =
              match O.cond with
              | Unicond ->
                  let evts = 
                    List.map
                      (List.map (fun n -> n.C.evt))
                      splitted in
                  F.run evts m
              | Cycle -> F.check f
              | Observe -> F.observe f in
            (add_args env c,f),
            (U.compile_prefetch_ios (List.length obsc) ios,
             U.compile_coms splitted),
            env


(********)
(* Dump *)
(********)

      type args = (A.typ * Code.loc) list

      let dump_args args =
        let pp =
          List.map
            (fun (t,loc) -> match t with
            | A.Plain _ ->
                sprintf "volatile %s* %s" (A.dump_typ t) loc
            | A.Atomic _ ->
                sprintf "%s* %s" (A.dump_typ t) loc)
            args in
        String.concat "," pp


      let dump_mem_order = MemOrder.pp_mem_order

      let dump_loc_exp = function
        | A.Loc loc -> loc
        | A.Reg (_,r) -> A.dump_reg r

      let dump_cond c = match c with A.Ne -> "!=" | A.Eq -> "=="

      let rec dump_exp e =
        let open A in
        match e with
        | Load loc -> dump_loc_exp loc
        | AtomicLoad (MemOrder.SC,loc) ->
            sprintf "atomic_load(%s)" (dump_loc_exp loc)
        | AtomicLoad (mo,loc) ->
            sprintf "atomic_load_explicit(%s,%s)"
              (dump_loc_exp loc) (dump_mem_order mo)
        | AtomicExcl (MemOrder.SC,loc,v) ->
            sprintf "atomic_exchange(%s,%i)" (dump_loc_exp loc) v
        | AtomicExcl (mo,loc,v) ->
            sprintf "atomic_exchange_explicit(%s,%i,%s)"
              (dump_loc_exp loc) v (dump_mem_order mo)
        | Deref (Load _ as e) -> sprintf "*%s" (dump_exp e)
        | Deref e -> sprintf "*(%s)" (dump_exp e)
        | Const v -> sprintf "%i" v
        | AssertVal (e,_) -> dump_exp e

      let dump_condexp (e1,c,e2) =
        sprintf "%s %s %s"
          (dump_exp e1) (dump_cond c) (dump_exp e2)

      let fx chan indent fmt =
        output_string chan indent ;
        kfprintf
          (fun chan -> output_string chan "\n")
          chan fmt

      let indent1 = "  "
      let indent2 = indent1 ^ indent1

      let rec dump_ins chan i ins =
        let open A in
        match ins with
        | Seq (i1,i2) ->
            dump_ins chan i i1 ;
            dump_ins chan i i2
        | Decl (t,r,None) ->
            fx chan i "%s %s;" (A.dump_typ t) (A.dump_reg r)
        | Decl (t,r,Some e) ->
            fx chan i "%s %s = %s;"
              (A.dump_typ t) (A.dump_reg r) (dump_exp e)
        | Store (loc,e) ->
            fx chan i "*%s = %s;" loc (dump_exp e)
        | AtomicStore (MemOrder.SC,loc,e) ->
            fx chan i "atomic_store(%s,%s);"
              loc (dump_exp e)
        | SetReg (r,e) ->
            fx chan i "%s = %s;" (A.dump_reg r) (dump_exp e)
        | AtomicStore (mo,loc,e) ->
            fx chan i "atomic_store_explicit(%s,%s,%s);"
              loc (dump_exp e) (dump_mem_order mo)
        | Fence mo ->
            fx chan i "atomic_thread_fence(%s);" (dump_mem_order mo)
        | Loop body ->
            fx chan i "for (;;) {" ;
            dump_ins chan (i ^ indent1) body ;
            fx chan i "}"
        | If (ce,itrue,ifalse) ->
            fx chan i "if (%s) {" (dump_condexp ce) ;
            dump_ins chan (i ^ indent1) itrue ;
            begin match ifalse with
            | None -> ()
            | Some ins ->
                fx chan i "} else {" ;
                dump_ins chan (i ^ indent1) ins ;
            end ;
            fx chan i "}"
        | Break ->
            fx chan i "break;"
        | A.Decr r ->
            fx chan i "%s--;" (A.dump_reg r)
        | Nop -> ()


      let dump_proc_code chan p (a,i) =
        fprintf chan "P%i (%s) {\n" p (dump_args a) ;
        dump_ins chan indent1 i ; 
        fprintf chan "}\n" ;
        ()

      type prog = (args * A.ins) list

      let dump_code chan prog =
        Misc.iteri
          (fun p code ->
            dump_proc_code chan p code ;
            output_string chan "\n")
          prog

(* Empty init *)
      let dump_init chan =  fprintf chan "\n{}\n\n"

(********)
(* Test *)
(********)

      type edge = E.edge
      type node = C.node
      let ppo _f k = k

      type check = edge list list -> bool

      type test =
          {
           name : string ;
           com : string ;
           info : Code.info ;
           edges : edge list ;
           prog : prog ;
           final : F.final ;
           types : A.typ StringMap.t;
         }

      let extract_edges t = t.edges

      let dump_c_test_channel chan t =
        fprintf chan "C %s\n" t.name ;
        if t.com <>  "" then fprintf chan "\"%s\"\n" t.com ;
        List.iter
          (fun (k,v) -> fprintf chan "%s=%s\n" k v)
          t.info ;
        Hint.dump O.hout t.name t.info ;
        (* Empty init *)
        dump_init chan ;
        dump_code chan t.prog ;
        F.dump_final chan t.final ;
        ()

(*
      let dump_c_test ({ name = name; _ } as t) =
        let fname = name ^ ".litmus" in
        Misc.output_protect
          (fun chan -> dump_c_test_channel chan t)
          fname
*)

(************************)
(* C++ a la cppmem dump *)
(************************)
      let rec dump_exp e =
        let open A in
        match e with
        | Load loc -> dump_loc_exp loc
        | AtomicLoad (mo,loc) ->
            sprintf "%s.load(%s)"
              (dump_loc_exp loc) (dump_mem_order mo)
        | AtomicExcl (mo,loc,v) ->
            sprintf "%s.exchange(%i,%s)"
              (dump_loc_exp loc) v (dump_mem_order mo)
        | Deref (Load _ as e) -> sprintf "*%s" (dump_exp e)
        | Deref e -> sprintf "*(%s)" (dump_exp e)
        | Const v -> sprintf "%i" v
        | AssertVal (AtomicLoad _|Load _ as e,v) ->
            sprintf "%s.readsvalue(%i)" (dump_exp e) v
        | AssertVal _ ->
            Warn.fatal "Cannot compile to C++ (expr)"

      let dump_cond_arg e = match e with
      | A.Const _|A.Load _ -> dump_exp e
      | _ -> sprintf "(%s)" (dump_exp e)

      let dump_condexp (e1,c,e2) =
        sprintf "%s %s %s"
          (dump_cond_arg e1)
          (dump_cond c)
          (dump_cond_arg e2)

      let rec dump_ins chan i ins =
        let open A in
        match ins with
        | Seq (i1,i2) ->
            dump_ins chan i i1 ;
            dump_ins chan i i2
        | Decl (_,_,None) -> ()
        | Decl (_t,r,Some e) ->
            fx chan i "%s = %s;" (A.dump_reg r) (dump_exp e)
        | Store (loc,e) ->
            fx chan i "%s = %s;" loc (dump_exp e)
        | SetReg (r,e) ->
            fx chan i "%s = %s;" (A.dump_reg r) (dump_exp e)
        | AtomicStore (mo,loc,e) ->
            fx chan i "%s.store(%s,%s);"
              loc (dump_exp e) (dump_mem_order mo)
        | If (ce,itrue,ifalse) ->
            fx chan i "if (%s) {" (dump_condexp ce) ;
            dump_ins chan (i ^ indent1) itrue ;
            begin match ifalse with
            | None -> ()
            | Some ifalse ->
                fx chan i "} else {" ;
                dump_ins chan (i ^ indent1) ifalse ;
            end ;
            fx chan i "}"
        | Fence _ 
        | Loop _ 
        | Break 
        | A.Decr _ ->
            Warn.fatal "Cannot compile to C++"
        | Nop -> ()


      let dump_prog chan =
        let rec do_rec pre = function
          | (_,i)::rem ->
              fx chan indent1 "%s {" pre ;
              dump_ins chan indent2 i ;
              fx chan indent1 "}" ;
              do_rec "|||" rem
          | [] ->
              fx chan indent1 "%s" "}}}" in
        do_rec "{{{"
          
      let dump_cpp_test_channel chan t =
        fprintf chan "// CPP %s\n" t.name ;
        if t.com <>  "" then fprintf chan "// \"%s\"\n" t.com ;
        fprintf chan "int main() {\n" ;
        StringMap.iter
          (fun loc t ->
            fprintf chan "  %s %s = 0;\n" (A.dump_typ t) loc)
          t.types ;
        dump_prog chan t.prog ;
        fprintf chan "  return 0;\n" ;
        fprintf chan "}\n" ;
        ()
          
(*
      let dump_cpp_test  ({ name = name; _ } as t) =
        let fname = name ^ ".c" in
        Misc.output_protect
          (fun chan -> dump_cpp_test_channel chan t)
          fname
*)
      let dump_test_channel =
        if O.cpp then dump_cpp_test_channel
        else dump_c_test_channel

(*
      let dump_test =
        if O.cpp then dump_cpp_test
        else dump_c_test
*)

      let test_of_cycle name ?com ?(info=[]) ?(check=(fun _ -> true)) es c =
        let com = match com with None -> E.pp_edges es | Some com -> com in
        let (prog,final),(prf,coms),env = compile_cycle check c in
        let coms = String.concat " " coms in
        let info = info@["Prefetch",prf ; "Com",coms; "Orig",com; ] in
        { name=name ; info=info; com=com ;  edges = es ;
          prog=prog ; final=final ; types=env;}
          
      let make_test name ?com ?info ?check es =
        try
          if O.verbose > 1 then eprintf "**Test %s**\n" name ;
          if O.verbose > 2 then eprintf "**Cycle %s**\n" (E.pp_edges es) ;
          let es,c = C.make es in
          test_of_cycle name ?com ?info ?check es c
        with
        | Misc.Fatal msg ->
            Warn.fatal "Test %s [%s] failed:\n%s" name (E.pp_edges es) msg
              


    end
