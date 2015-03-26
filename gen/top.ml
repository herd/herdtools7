(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code
open Printf

module type Config = sig
  val verbose : int
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
  val fno : Config.fno
  val poll : bool
  val optcoherence : bool
  val docheck : bool
end

module Make (O:Config) (Comp:XXXCompile.S) : Builder.S 
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
       final : F.final ;
     }

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
    | Yes of E.dp * A.arch_reg

(* Catch exchanges at the very last moment... *)

  let call_emit_access st p init n =
    let e = n.C.evt in
    if e.C.rmw then match e.C.dir with
    | R ->
        let r,init,cs,st = Comp.emit_exch st p init e n.C.next.C.evt in
        Some r,init,cs,st
    | W -> None,init,[],st

    else if 
      match e.C.atom with
      | None -> true
      | Some a ->  A.applies_atom a e.C.dir
    then      
      Comp.emit_access st p init e
    else
      Warn.user_error "atomicity mismatch on edge %s, annotation %s on %s"
        (E.pp_edge n.C.edge)
        (E.pp_atom_option e.C.atom) (pp_dir e.C.dir)

  let call_emit_access_dep st p init n dp r1 =
    let e = n.C.evt in
     if e.C.rmw then match e.C.dir with
     | R ->
         let r,init,cs,st =
           Comp.emit_exch_dep st p init e n.C.next.C.evt dp r1 in
         Some r,init,cs,st
     | W -> None,init,[],st
     else
       Comp.emit_access_dep st p init e dp r1

(* Encodes load of first non-initial value in chain,
   can poll on value in place of checking it *)

  let emit_access ro_prev st p init n =
    let init,ip,st = match O.overload with
    | Some ov  when insert_overload n ->
        emit_overload st p init ov n.C.evt.C.loc
    | _ -> init,[],st in
    let o,init,i,st = match ro_prev with
    | No ->
        let open Config in
        begin match
          O.fno,n.C.evt.C.dir,n.C.prev.C.prev.C.edge.E.edge,
          n.C.prev.C.edge.E.edge,
          n.C.edge.E.edge
        with
        | (FnoPrev|FnoBoth|FnoOnly),R,_,Rf _, E.Fenced _->
            let r,init,i,st = Comp.emit_fno st p init n.C.evt.C.loc in
            Some r,init,i,st
        | (FnoBoth|FnoOnly),R,Rf _, E.Fenced _,_->
            let r,init,i,st = Comp.emit_fno st p init n.C.evt.C.loc in
            Some r,init,i,st
        | _ ->
            if U.do_poll n then
              let r,init,i,st =
                Comp.emit_load_one st p init n.C.evt.C.loc in
              Some r,init,i,st
            else
              call_emit_access st p init n
        end
    | Yes (dp,r1) -> call_emit_access_dep st p init n dp r1 in
    o,init,ip@i,st

let edge_to_prev_load o e = match o with
| None -> No
| Some r ->
    begin match e.E.edge with
    | Dp (dp,_,_) -> Yes (dp,r)
    | _ -> No
    end

let get_fence n =
  let open Config in 
  match O.fno,n.C.edge.E.edge with
  | FnoOnly,E.Fenced (fe,_,_,_) ->
      begin match n.C.evt.C.dir,n.C.next.C.evt.C.dir with
      | R,R -> None
      | _,_ -> Some fe
      end
  | _, E.Fenced (fe,_,_,_) ->  Some fe
  | _,_ -> None
        
let rec compile_stores st p i ns k = match ns with
| [] -> i,k,st
| n::ns ->
    let sto = n.C.store in
    if sto == C.nil then
      compile_stores st p i ns k
    else
      let _,i,c,st = Comp.emit_access st p i sto.C.evt in
      let i,k,st = compile_stores st p i ns k in
      i,(c@k),st

let rec compile_proc chk loc_writes st p ro_prev init ns = match ns with
| [] -> init,[],(C.EventMap.empty,[]),st
| n::ns ->
    let open Config in
    begin match
      O.fno,
      n.C.evt.C.dir,n.C.prev.C.edge.E.edge,
      n.C.next.C.evt.C.dir,ns with
    | FnoAll,R,_,_,_ ->
        begin match  ro_prev with
        | No -> ()
        | Yes _ -> Warn.fatal "Dependency to fno"
        end ;
        let o,init,i,st = Comp.emit_fno st p init n.C.evt.C.loc in
        let init,is,finals,st =
          compile_proc chk loc_writes
            st p (edge_to_prev_load (Some o) n.C.edge)
            init ns in
        init,
        i@(match get_fence n with Some fe -> Comp.emit_fence fe::is  | _ -> is),
        (if StringSet.mem n.C.evt.C.loc loc_writes then
          F.add_final p (Some o) n finals
        else finals),
        st
    | (FnoRf,R,Rf _,d,m::ns)
    | (_,R,RfStar _,d,m::ns) ->
        let o1,init,i1,lab,st = Comp.emit_open_fno st p init n.C.evt.C.loc in
        let o2,init,i2,st =
          emit_access (edge_to_prev_load (Some o1) n.C.edge) st p init m in
        let init,i3,st = Comp.emit_close_fno st p init lab o1 n.C.evt.C.loc in
        let init,is,finals,st =
          compile_proc chk loc_writes
            st p (edge_to_prev_load o2 m.C.edge)
            init ns in
        init,
        i1@
        (match  get_fence n  with Some fe -> [Comp.emit_fence fe] | _ -> [])@
        (match d with R -> i2@i3@is | W -> i3@i2@is),
        F.add_final p (Some o1) n (F.add_final p o2 m finals),
        st
    | _ ->
        let o,init,i,st = emit_access ro_prev st p init n in
        let nchk,add_check =
          match O.docheck,n.C.evt.C.dir,o,ns with
          | true,R,Some r,_::_ ->
              true,Comp.check_load p r n.C.evt
          | _ -> chk,Misc.identity  in
        let init,is,finals,st =
          compile_proc nchk loc_writes
            st p (edge_to_prev_load o n.C.edge)
            init ns in
        add_init_check chk p o init,
        i@
        add_check
          (match get_fence n with Some fe -> Comp.emit_fence fe::is  | _ -> is),
        (if
          StringSet.mem n.C.evt.C.loc loc_writes && not (U.do_poll n)
        then
          F.add_final p o n finals
        else finals),
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
      let f = [Comp.emit_fence Comp.stronger_fence] in
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
            
  let rec check_rec atoms p i =
    let add_look_loc loc v k =
      if not (StringSet.mem loc atoms) && O.optcond then
        k else (A.Loc loc,IntSet.singleton v)::k in
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
                  | Avoid|Accept -> i,[],[A.Loc x,IntSet.singleton v]
              | Enforce ->  
                  let i,c,f = build_observers p i x vs in
                  i,c,add_look_loc x v f
                  end
              | _ ->
              let v =
                let v,_ = Misc.last (List.flatten vs) in
                v in
              begin match O.do_observers with
              | Local -> i,[],add_look_loc x v []
              | _ ->
                  let i,c,f = build_observers p i x vs in
                  i,c,add_look_loc x v f
              end
          end in
          let i,cs,fs =
            check_rec atoms (p+List.length c) i xvs in
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
        let c = Comp.emit_fence Comp.stronger_fence::c in
        i,code@c,f,st
    | Loop ->
        let i,c,f,st = do_add_loop st p i f x prev_v v in
        i,code@c,f,st

  let add_co_local_check lsts ns st p i code f =
    let lst = Misc.last ns in
    if U.check_here lst then
      let x = lst.C.evt.C.loc and v = lst.C.next.C.evt.C.v
      and prev_v = lst.C.evt.C.v in
      let all_lst =
        try StringMap.find x lsts
        with Not_found -> C.evt_null in
      if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
      then
        i,code,(A.Loc x,IntSet.singleton v)::f,st
      else
        do_observe_local st p i code f x prev_v v
    else i,code,f,st

(**********)
(* Detour *)
(**********)

  let do_observe_local_before st p i code f x prev_v v =
    if O.optcoherence && v = 0 then
      i,code,[],st
    else
      let open Config in
      match O.obs_type with
      | Straight|Config.Fenced ->
          let i,c,f,st = do_add_load st p i f x v in
          i,code@c,f,st
      | Loop ->
          let i,c,f,st = do_add_loop st p i f x prev_v v in
          i,code@c,f,st
            

  let build_detour lsts st p i n =
    let open Config in
    let i,c0,f,st = match O.do_observers with
    | Local -> begin match n.C.edge.E.edge with
      | DetourWs (Dir W) ->          
          do_observe_local_before st p i [] [] n.C.evt.C.loc
            n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
      | DetourWs (Dir R) ->          
          do_observe_local_before st p i [] [] n.C.evt.C.loc
            n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
      | _ -> i,[],[],st 
    end
    | _ -> i,[],[],st in

    let _,i,c,st = Comp.emit_access st p i n.C.evt in
    let c = c0@c in
    match O.do_observers with
    | Local ->
        let i,c,f,_st = add_co_local_check lsts [n] st p i c f in
        i,c,f
    | _ -> i,c,f

  let rec build_detours lsts p i ns = match ns with
  | [] -> i,[],[]
  | n::ns ->
      let i,c,f = build_detour lsts A.st0 p i n in
      let i,cs,fs = build_detours lsts (p+1) i ns in
      i,c::cs,f@fs

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

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
          let i,c,(m,f),st = compile_proc false loc_writes A.st0 p No i n in
          let i,c,st = compile_stores st p i n c in
          let i,c,f,st =
            match O.cond with
            | Unicond -> i,c,f,st                
            | Cycle|Observe ->
                match O.do_observers with
                | Local -> add_co_local_check lsts n st p i c f
                | Avoid|Accept|Enforce -> i,c,f,st in
          let i,c,_ = Comp.postlude st p i c in
          let ds = C.get_detours_from_list n in
          let i,cds,fds = build_detours lsts (p+1) i ds in
          let i,cs,(ms,fs),ios = do_rec (p+1+List.length cds) i ns in
          let io = U.io_of_thread n in
          let iod = List.map U.io_of_detour ds in
          i,c::(cds@cs),(C.union_map m ms,f@fds@fs),(io::iod)@ios in
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
                  (fun (_,loc) -> (loc:string) = Code.ok)
                  i
              then
                (A.Loc Code.ok,"1")::i,obsc@cs,
                (m,(A.Loc Code.ok,IntSet.singleton 1)::f@fs),ios
              else
                i,obsc@cs,(m,f@fs),ios
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
        (i,c,f),
        (U.compile_prefetch_ios (List.length obsc) ios,
         U.compile_coms splitted)


(********)
(* Dump *)
(********)

let get_proc = function
  | A.Loc _ -> -1
  | A.Reg (p,_) -> p

let dump_init chan inits =
  fprintf chan "{" ;
  let rec p_rec q = function
    | [] -> fprintf chan "\n}\n"
    | (left,loc)::rem ->
        let p = get_proc left in
        if p <> q then fprintf chan "\n" else fprintf chan " " ;
        fprintf chan "%s=%s;" (A.pp_location left) loc ;
        p_rec p rem in
  p_rec (-1) inits


let rec dump_pseudo = function
  | [] -> []
  | A.Instruction ins::rem -> A.dump_instruction ins::dump_pseudo rem
  | A.Label (lbl,ins)::rem ->
      sprintf "%s:" lbl::dump_pseudo (ins::rem)
  | A.Nop::rem -> dump_pseudo rem
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
        (sprintf "P%i" p::dump_pseudo cs)::
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
    dump_init chan t.init ;
    dump_code chan t.prog ;
    F.dump_final chan t.final ;
    ()


let _dump_test ({ name = name; _ } as t) =
  let fname = name ^ ".litmus" in
  Misc.output_protect
    (fun chan -> dump_test_channel chan t)
    fname


let test_of_cycle name ?com ?(info=[]) ?(check=(fun _ -> true)) es c =
  let com = match com with None -> pp_edges es | Some com -> com in
  let(init,prog,final),(prf,coms) = compile_cycle check c in
  let coms = String.concat " " coms in
  let info = info@["Prefetch",prf ; "Com",coms; "Orig",com; ] in
  { name=name ; info=info; com=com ;  edges = es ;
    init=init ; prog=prog ; final=final ; }
    
let make_test name ?com ?info ?check es =
  try
    if O.verbose > 1 then eprintf "**Test %s**\n" name ;
    if O.verbose > 2 then eprintf "**Cycle %s**\n" (pp_edges es) ;
    let es,c = C.make es in
    test_of_cycle name ?com ?info ?check es c
  with
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (pp_edges es) msg
  

end
