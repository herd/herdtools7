(****************************************************************************)
(*                           The diy toolsuite                              *)
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

open Code
open Printf

module type Config = sig
  val verbose : int
  val generator : string
  val debug : Debug_gen.t
  val hout : Hint.out
  val cond : Config.cond
  val coherence_decreasing : bool
  val neg : bool
  val nprocs : int
  val eprocs : bool
  val do_observers : Config.do_observers
  val obs_type : Config.obs_type
  val optcond : bool
  val overload : int option
  val poll : bool
  val optcoherence : bool
  val docheck : bool
  val typ : TypBase.t
  val hexa : bool
  val variant : Variant_gen.t -> bool
  val mtags : bool
end

module Make (O:Config) (Comp:XXXCompile_gen.S) : Builder.S
= struct

(* Config *)

  module A = Comp.A
  module E = Comp.E
  type check = Comp.check

  module R = Comp.R
  module C = Comp.C

  let ppo = Comp.ppo

  open E
  type edge = E.edge
  type node = C.node

  module F = Final.Make(O)(Comp)

  let add_init_check chk p o init =
    match chk,o with
    | true,Some r -> (A.Reg (p,r),"-1")::init
    | _,_ -> init

  type test =
      {
       name : string ;
       com : string ;
       info : Code.info ;
       edges : edge list ;
       init : A.init ;
       prog : A.pseudo list list ;
       scopes : BellInfo.scopes option ;
       final : F.final ;
       env : TypBase.t A.LocMap.t
     }

  let get_nprocs t = List.length t.prog
  let get_name t = t.name
  let set_name t n = { t with name=n; }
  let set_scope t sc =
    let pp = BellInfo.pp_scopes sc in
    { t with scopes = Some sc; info = ("Scopes",pp)::t.info; }

  let do_add_info key v k = match v with "" -> k | _ -> (key,v)::k
  let add_info t k i =  { t with info = do_add_info k i t.info; }
  let extract_edges {edges=es; _} = es

(* Utilities *)

module U = TopUtils.Make(O)(Comp)

(***************)
(* Compilation *)
(***************)

  let rec emit_overload st p init ov loc =
    if ov <= 0 then init,[],st
    else
      let loc_ov = sprintf "%s%i" loc ov in
      let _,init,i,st = Comp.emit_load st p init loc_ov in
      let init,is,st = emit_overload st p init (ov-1) loc in
      init,i@is,st

  let insert_overload n = match n.C.edge.E.edge with
  | Po (_,Dir R,Dir (W|R)) -> true
  | _ -> false

  type prev_load =
    | No       (* Non-existent or irrelevant *)
    | Yes of E.dp * A.arch_reg * Code.v

(* Catch exchanges at the very last moment... *)
  let as_rmw n = match n.C.edge.E.edge with
  | Rmw rmw -> rmw
  | _ -> assert false

  let call_emit_access st p init n =
    let e = n.C.evt in
    if e.C.rmw then match e.C.dir with
    | Some R ->
        Comp.emit_rmw
          (as_rmw n) st p init e n.C.next.C.evt
    | Some W|None ->
        None,init,[],st
    | Some J -> assert false
    else if
      match e.C.atom with
      | None -> true
      | Some a ->
          begin match e.C.dir with
          | None -> false
          | Some d ->  A.applies_atom a d
          end
    then
      Comp.emit_access st p init e
    else
      Warn.fatal "atomicity mismatch on edge %s, annotation '%s' on %s"
        (E.pp_edge n.C.edge)
        (E.pp_atom_option e.C.atom) (Misc.app_opt_def "_" pp_dir e.C.dir)

  let call_emit_access_dep st p init n dp r1 v1 =
    let e = n.C.evt in
     if e.C.rmw then match e.C.dir with
     | Some R ->
         Comp.emit_rmw_dep (as_rmw n) st p init e n.C.next.C.evt dp r1
     | Some W|None -> None,init,[],st
     | Some J -> assert false
     else
       Comp.emit_access_dep st p init e dp r1 v1

(* Encodes load of first non-initial value in chain,
   can poll on value in place of checking it *)

  let emit_access ro_prev st p init n =
    let init,ip,st  = match O.overload,n.C.evt.C.loc with
    | Some ov,Data loc  when insert_overload n ->
        emit_overload st p init ov loc
    | _ -> init,[],st in
    let o,init,i,st = match ro_prev,n.C.evt.C.loc with
    | No,Data loc ->
        if U.do_poll n then
          let r,init,i,st =
            Comp.emit_load_one st p init loc in
          Some r,init,i,st
        else
          call_emit_access st p init n
    | No,Code _ -> call_emit_access st p init n
    | (Yes (dp,r1,v1),_)
      -> call_emit_access_dep st p init n dp r1 v1 in
    o,init,ip@i,st

let edge_to_prev_load o n = match o with
| None -> No
| Some r ->
    begin match n.C.edge.E.edge with
    | Dp (dp,_,_) -> Yes (dp,r,n.C.evt.C.v)
    | _ -> No
    end

let get_fence n =
  match n.C.edge.E.edge with
  | E.Fenced (fe,_,_,_) ->  Some fe
  | _ -> None


  let no_check_load init st = init,Misc.identity,st

  let rec collect_inserts = function
    | [] -> [],[]
    | n::ns as all ->  match  n.C.edge.E.edge with
      | E.Insert f ->
          let fs,ns = collect_inserts ns in
          f::fs,ns
      | _ -> [],all

  let rec compile_proc pref chk loc_writes st p ro_prev init ns = match ns with
  | [] -> init,pref [],(C.EventMap.empty,[]),st
  | n::ns ->
      if O.verbose > 1 then eprintf "COMPILE PROC: <%s>\n" (C.str_node n);
      begin match  n.C.edge.E.edge with
      | E.Node _ ->
          let fs,ns =  collect_inserts ns in
          compile_proc
            (fun is ->
              pref
                (List.fold_right
                   (fun f is -> Comp.emit_fence p init n f@is)
                   fs is))
            chk loc_writes st p ro_prev init ns
      | E.Insert f ->
          let init,is,finals,st =
            compile_proc pref chk loc_writes st p ro_prev init ns in
          init,Comp.emit_fence p init n f@is,finals,st
      | _ ->
          let o,init,i,st = emit_access ro_prev st p init n in
          let nchk,add_check =
            match O.docheck,n.C.evt.C.dir,o,ns with
            | true,Some R,Some r,_::_ ->
                true,Comp.check_load p r n.C.evt
            | _ -> chk,no_check_load in
          let init,mk_c,st = add_check init st in
          let init,is,finals,st =
            compile_proc pref nchk loc_writes
              st p (edge_to_prev_load o n)
              init ns in
          let init,cf,st =
            match get_fence n with
            | Some fe -> Comp.full_emit_fence st p init n fe
            | None -> init,[],st in
          add_init_check chk p o init,
          i@
          mk_c (cf@is),
          (match n.C.evt.C.loc with
          | Data loc ->
              if StringSet.mem loc loc_writes  && not (U.do_poll n) then
                F.add_final p o n finals
              else finals
          | Code _ ->
              begin match o with
              | None   -> finals (* Code write *)
              | Some r -> (* fetch! *)
                  let m,fenv =  finals in
                  m,F.add_final_v p r (IntSet.singleton (U.fetch_val n))
                    fenv
              end),
          st
      end

(*************)
(* Observers *)
(*************)

let last_observation st p i x v =
  let r,i,c,_st = Comp.emit_load st p i x in
  i,c,F.add_final_v p r v []

let rec straight_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = Comp.emit_load st p i x in
      let i,cs,fs = straight_observer st p i x vs in
      i,c@cs,F.add_final_v p r v fs

let rec fenced_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = Comp.emit_load st p i x in
      let f = Comp.emit_fence p i C.nil Comp.stronger_fence in
      let i,cs,fs = fenced_observer st p i x vs in
      i,c@f@cs,F.add_final_v p r v fs


let loop_observer st p i x = function
  | []|[_] -> i,[],[]
  | v::vs ->
      let r,i,c,st = Comp.emit_load_not_zero st p i x in
      let rec do_loop st i prev_r = function
        | [] ->  assert false
        | [v] ->
            let r,i,c,_st = Comp.emit_load_not_eq st p i x prev_r in
            i,c,F.add_final_v p r v []
        | v::vs ->
            let r,i,c,st = Comp.emit_load_not_eq st p i x prev_r in
            let i,cs,fs = do_loop st i r vs in
            i,c@cs,F.add_final_v p r v fs in
      let i,cs,fs = do_loop st i r vs in
      i,c@cs,F.add_final_v p r v fs

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

  let build_observer st p i x vs =
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
                (fun is ->
                  match IntSet.as_singleton is with
                  | Some _ -> true | None -> false)
                vs then
              let ws,w = split_last vs in
              (match ws with [_] -> [] | _ -> ws),[A.Loc x, w]
            else
              min_max vs,[]
      else
        let vs = List.map (fun (v,_obs) -> IntSet.singleton v) vs in
        vs,[] in
    let i,cs,fs =
      let open Config in
      match O.obs_type with
      | Straight ->  straight_observer st p i x vs
      | Config.Fenced -> fenced_observer st p i x vs
      | Loop -> loop_observer st p i x vs in
    i,cs,fs@f

  let rec build_observers p i x arg =
    let open Config in
    match arg,O.do_observers with
    | [],_ -> i,[],[]
    | []::vss,_
    | [_]::vss,(Avoid|Accept) ->
        build_observers p i x vss
    | vs::vss,_ ->
        let i0 = i in
        try
          let i,c,f = build_observer A.st0 p i x vs in
          begin match c,O.do_observers with
          | _::_,Avoid  -> Warn.fatal "Observer"
          | _,_ -> ()
          end ;
          match c with
          | [] ->
              let i,cs,fs = build_observers p i0 x vss in
              i,cs,f@fs
          | _ ->
              let i,cs,fs = build_observers (p+1) i x vss in
              i,c::cs,f@fs
        with NoObserver -> build_observers p i x vss

  let rec check_rec ats p i =
    let add_look_loc loc v k =
        if (not (StringSet.mem loc ats) && O.optcond) then k
        else (A.Loc loc,IntSet.singleton v)::k in
    let open Config in
    function
      | [] -> i,[],[]
      | (x,vs)::xvs ->
          let i,c,f = match O.cond with
          | Observe ->
              let vs = List.flatten vs in
              begin match vs with
              | [] -> i,[],[]
              | _::_ ->
                  let v,_ = Misc.last vs in
                  i,[],[A.Loc x,IntSet.singleton v]
              end
          | Unicond -> assert false
          | Cycle -> begin
              match vs with
              | [] -> i,[],[]
              | [[(v,_)]] -> i,[],add_look_loc x v []
              | [[_;(v,_)]] ->
                  begin match O.do_observers with
                  | Local -> i,[],add_look_loc x v []
                  | Avoid|Accept|Three|Four|Infinity
                    -> i,[],[A.Loc x,IntSet.singleton v]
                  | Enforce ->
                      let i,c,f = build_observers p i x vs in
                      i,c,add_look_loc x v f
                  end
              | _ ->
                  let vs_flat = List.flatten vs in
                  let v,_ = Misc.last vs_flat in
              begin match O.do_observers with
              | Local -> i,[],add_look_loc x v []
              | Three ->
                  begin match vs_flat with
                  | _x1::_x2::_x3::_x4::_ ->
                      Warn.fatal "More than three writes"
                  | _ -> i,[],[A.Loc x,IntSet.singleton v]
                  end
              |Four ->
                  begin match vs_flat with
                  | _x1::_x2::_x3::_x4::_x5::_ ->
                      Warn.fatal "More than four writes"
                  | _ -> i,[],[A.Loc x,IntSet.singleton v]
                  end
              | Infinity ->
                  i,[],[A.Loc x,IntSet.singleton v]
              | _ ->
                  let i,c,f = build_observers p i x vs in
                  i,c,add_look_loc x v f
              end
          end in
          let i,cs,fs =
            check_rec ats (p+List.length c) i xvs in
          i,c@cs,f@fs

  let check_writes atoms p i cos = check_rec atoms p i cos


(* Local check of coherence *)

  let do_add_load st p i f x v =
    let r,i,c,st = Comp.emit_load st p i x in
    i,c,F.add_final_v p r (IntSet.singleton v) f,st

  let do_add_loop st p i f x v w =
    let r,i,c,st = Comp.emit_load_not_value st p i x v in
    i,c,F.add_final_v p r (IntSet.singleton w) f,st




  let do_observe_local st p i code f x prev_v v =
    let open Config in
    match O.obs_type with
    | Straight ->
        let i,c,f,st = do_add_load st p i f x v in
        i,code@c,f,st
    | Config.Fenced ->
        let i,c,f,st = do_add_load st p i f x v in
        let c = Comp.emit_fence p i C.nil Comp.stronger_fence@c in
        i,code@c,f,st
    | Loop ->
        let i,c,f,st = do_add_loop st p i f x prev_v v in
        i,code@c,f,st

  let add_co_local_check lsts ns st p i code f =
    let lst = Misc.last ns in
    if U.check_here lst then
      match lst.C.evt.C.loc with
      | Data x ->
          let v = lst.C.next.C.evt.C.v
          and prev_v = lst.C.evt.C.v in
          let all_lst =
            try StringMap.find x lsts
            with Not_found -> C.evt_null in
          if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
          then
            i,code,(A.Loc x,IntSet.singleton v)::f,st
          else
            do_observe_local st p i code f x prev_v v
      | Code _ -> i,code,f,st
    else i,code,f,st

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)
  let list_of_init_ok_locs p lab =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = (A.Loc (as_data (Code.myok p (n-1))))::k
               in do_rec (i-1) k'
      in
      do_rec lab []

  let gather_final_oks p lab =
    let oks = list_of_init_ok_locs p lab in
    let rec do_rec oks k =
      match oks with
      | [] -> k
      | ok::oks -> let k' = (ok,IntSet.singleton 1)::k
                   in do_rec oks k'
    in do_rec oks []

 let compile_cycle ok n =
    let open Config in
    Label.reset () ;
    let splitted =  C.split_procs n in
    (* Split before, as  proc numbers added by side effet.. *)
    let cos0 = C.coherence n in
    let lsts = U.last_map cos0 in
    let cos = U.compute_cos cos0 in
    if O.verbose > 1 then U.pp_coherence cos0 ;
    let loc_writes = U.comp_loc_writes n in

    let rec do_rec p i = function
      | [] -> List.rev i,[],(C.EventMap.empty,[]),[]
      | n::ns ->
          let i,c,(m,f),st =
            compile_proc Misc.identity false loc_writes A.st0 p No i n in
          let xenv = Comp.get_xstore_results c in
          let f =
            List.fold_left
              (fun f (r,v) -> F.add_final_v p r (IntSet.singleton v) f)
              f xenv in
          let i,c,f,st =
            match O.cond with
            | Unicond -> i,c,f,st
            | Cycle|Observe ->
                match O.do_observers with
                | Local -> add_co_local_check lsts n st p i c f
                | Avoid|Accept|Enforce|Three|Four|Infinity -> i,c,f,st in
          let i,c,st = Comp.postlude st p i c in
          let foks = gather_final_oks p (A.current_label st) in
          let i,cs,(ms,fs),ios = do_rec (p+1) i ns in
          let io = U.io_of_thread n in
          i,c::cs,(C.union_map m ms,f@foks@fs),io::ios in
    let i,obsc,f =
      match O.cond with
      | Unicond -> [],[],[]
      | Cycle|Observe ->
          let atoms = U.comp_atoms n in
          check_writes atoms 0  [] cos in
    match splitted,O.cond with
    | [],_ -> Warn.fatal "No proc"
(*    | [_],Cycle -> Warn.fatal "One proc" *)
    | _,_ ->
        let i,c,(m,f),ios =
          if
            let len =  List.length splitted in
            O.nprocs <= 0 ||
            (if O.eprocs then len = O.nprocs else len <= O.nprocs)
          then
            let ess = List.map (List.map (fun n -> n.C.edge)) splitted in
            if ok ess then
              let i,cs,(m,fs),ios = do_rec (List.length obsc) i splitted in
              if
                List.exists
                  (fun (_,loc) -> (loc:string) = Code.ok_str)
                  i
              then
                (A.Loc Code.ok_str,"1")::i,obsc@cs,
                (m,(A.Loc Code.ok_str,IntSet.singleton 1)::f@fs),ios
              else
                i,obsc@cs,(m,f@fs),ios
            else Warn.fatal "Last minute check"
          else  Warn.fatal "Too many procs" in
        let env =
          List.fold_left
            (fun m (loc,_) -> A.LocMap.add loc O.typ m)
            A.LocMap.empty f in
        let env =
          List.fold_left
            (fun m loc -> A.LocMap.add (A.Loc loc) O.typ m)
            env (C.get_globals n) in
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
        (i,c,f,env),
        (U.compile_prefetch_ios (List.length obsc) ios,
         U.compile_coms splitted)


(********)
(* Dump *)
(********)

let get_proc = function
  | A.Loc _ -> -1
  | A.Reg (p,_) -> p

(*
let pp_pointer t =
  let open TypBase in
  let open MachSize in
  match t with
  | Int|Std (Signed,Word) -> ""
  | _ -> sprintf "%s* " (TypBase.pp t)
*)

let dump_init chan inits env =
  fprintf chan "{" ;
  let pp =
    A.LocMap.fold
      (fun loc t k ->
      let open TypBase in
      let open MachSize in
      match t with
      | Int|Std (Signed,Word) -> k
      | _ -> sprintf "%s %s;" (TypBase.pp t) (A.pp_location loc)::k)
      env [] in
  begin match pp with
  | [] -> ()
  | _::_ ->
      fprintf chan "\n%s\n" (String.concat " " pp)
  end ;
  let rec p_rec q = function
    | [] -> fprintf chan "\n}\n"
    | (left,loc)::rem ->
        let p = get_proc left in
        if p <> q then fprintf chan "\n" else fprintf chan " " ;
        fprintf chan "%s=%s;" (A.pp_location left) loc ;
        if !Config.mtags then
          if not(List.exists (fun (a,b) -> b=loc) rem)
          then fprintf chan " %s.atag;" loc ;
        p_rec p rem in
  p_rec (-1) inits


let rec dump_pseudo = function
  | [] -> []
  | A.Instruction ins::rem -> A.dump_instruction ins::dump_pseudo rem
  | A.Label (lbl,ins)::rem ->
      sprintf "%s:" lbl::dump_pseudo (ins::rem)
  | A.Nop::rem -> dump_pseudo rem
  | A.Symbolic _::_ -> assert false (* no symbolic in diy *)
  | A.Macro (m,args)::rem ->
      sprintf "%s(%s)"
        m
        (String.concat ","
           (List.map A.pp_reg args))::
      dump_pseudo rem

let fmt_cols =
  let rec fmt_col p k = function
    | [] -> k
    | cs::prog ->
        (pp_proc p::dump_pseudo cs)::
        fmt_col (p+1) k prog in
  fmt_col 0 []

  let dump_code chan code =
    let pp = fmt_cols code in
    Misc.pp_prog chan pp

  let dump_test_channel chan t =
    fprintf chan "%s %s\n" (Archs.pp A.arch) t.name ;
    if t.com <>  "" then fprintf chan "\"%s\"\n" t.com ;
    List.iter
      (fun (k,v) -> fprintf chan "%s=%s\n" k v)
      t.info ;
    Hint.dump O.hout t.name t.info ;
    dump_init chan t.init t.env ;
    dump_code chan t.prog ;
    begin match t.scopes with
    | None -> ()
    | Some st ->
        fprintf chan "scopes: %s\n" (BellInfo.pp_scopes st)
    end ;
    F.dump_final chan t.final ;
    ()

  let num_labels =

    let rec num_ins p m = function
      | A.Label (lab,i) -> num_ins p (StringMap.add lab p m) i
      | _ -> m in

    let num_code p  = List.fold_left (num_ins p) in

    let rec num_rec p m = function
      | [] -> m
      | c::cs -> num_rec (p+1) (num_code p m c) cs in
    num_rec 0 StringMap.empty

let tr_labs m env =
  List.map
    (fun (loc,v) ->
      let v =
        try
          let p = StringMap.find v m in
          sprintf "%s:%s" (pp_proc p) v
        with Not_found -> v in
      (loc,v))
    env
let do_self =  O.variant Variant_gen.Self

let test_of_cycle name
  ?com ?(info=[]) ?(check=(fun _ -> true)) ?scope es c =
  let com = match com with None -> pp_edges es | Some com -> com in
  let (init,prog,final,env),(prf,coms) = compile_cycle check c in
  let m_labs = num_labels prog in
  let init = tr_labs m_labs init in
  let coms = String.concat " " coms in
  let info =
    let myinfo =
      (if do_self then fun k -> k else do_add_info "Prefetch" prf)
        (do_add_info "Com" coms (do_add_info "Orig" com [])) in
    let myinfo = match scope with
    | None -> myinfo
    | Some st -> ("Scopes",BellInfo.pp_scopes st)::myinfo in
    let myinfo = ("Generator",O.generator)::myinfo in
    info@myinfo in

  { name=name ; info=info; com=com ;  edges = es ;
    init=init ; prog=prog ; scopes = scope; final=final ; env=env; }

let make_test name ?com ?info ?check ?scope es =
  try
    if O.verbose > 1 then eprintf "**Test %s**\n" name ;
    if O.verbose > 2 then eprintf "**Cycle %s**\n" (pp_edges es) ;
    let es,c = C.make es in
    test_of_cycle name ?com ?info ?check ?scope es c
  with
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (pp_edges es) msg


end
