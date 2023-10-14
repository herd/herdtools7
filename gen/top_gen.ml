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
  val cycleonly: bool
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
    | true,Some r -> (A.Reg (p,r),Some (A.S "-1"))::init
    | _,_ -> init

  type typ = Typ of TypBase.t | Array of TypBase.t * int

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
       env : typ A.LocMap.t
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
    | Yes of E.dp * A.arch_reg * C.node

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
    | Some (J|D|I) -> assert false
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
      Warn.fatal "annotation mismatch on edge %s, annotation '%s' on %s"
        (E.pp_edge n.C.edge)
        (E.pp_atom_option e.C.atom) (Misc.app_opt_def "_" pp_dir e.C.dir)

  let call_emit_access_dep st p init n dp r1 n1 =
    let e = n.C.evt in
     if e.C.rmw then match e.C.dir with
     | Some R ->
         Comp.emit_rmw_dep (as_rmw n) st p init e n.C.next.C.evt dp r1 n1
     | Some W|None -> None,init,[],st
     | Some (J|D|I) -> assert false
     else
       Comp.emit_access_dep st p init e dp r1 n1

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
    | Yes (dp,r1,n1),_
      -> call_emit_access_dep st p init n dp r1 n1 in
    o,init,ip@i,st

let edge_to_prev_load o n = match o with
| None -> No
| Some r ->
    begin match n.C.edge.E.edge with
    | Dp (dp,_,_) -> Yes (dp,r,n)
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
                   (fun f is -> let _,cs,_ = Comp.emit_fence st p init n f
                   in cs@is)
                   fs is))
            chk loc_writes st p ro_prev init ns
      | E.Insert f ->
          let init,is,finals,st =
            compile_proc pref chk loc_writes st p ro_prev init ns in
          let init,cs,st = Comp.emit_fence st p init n f in
          init,cs@is,finals,st
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
            | Some fe -> Comp.emit_fence st p init n fe
            | None -> init,[],st in
          add_init_check chk p o init,
          i@
          mk_c (cf@is),
          (match n.C.evt.C.loc with
          | Data loc ->
              let call_add =
                StringSet.mem loc loc_writes  && not (U.do_poll n) in
              if call_add then
                F.add_final (A.get_friends st) p o n finals
              else begin
                finals
              end
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
  let r,i,c,_st = Comp.emit_obs Ord st p i x in
  i,c,F.add_final_v p r v []

let rec straight_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = Comp.emit_obs Ord st p i x in
      let i,cs,fs = straight_observer st p i x vs in
      i,c@cs,F.add_final_v p r v fs

let rec fenced_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = Comp.emit_obs Ord st p i x in
      let i,f,st = Comp.emit_fence st p i C.nil Comp.stronger_fence in
      let i,cs,fs = fenced_observer st p i x vs in
      i,c@f@cs,F.add_final_v p r v fs


let loop_observer st p i x = function
  | []|[_] -> i,[],[]
  | v::vs ->
      let r,i,c,st = Comp.emit_obs_not_zero st p i x in
      let rec do_loop st i prev_r = function
        | [] ->  assert false
        | [v] ->
            let r,i,c,_st = Comp.emit_obs_not_eq st p i x prev_r in
            i,c,F.add_final_v p r v []
        | v::vs ->
            let r,i,c,st = Comp.emit_obs_not_eq st p i x prev_r in
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

let min_set = IntSet.min_elt

let max_set = IntSet.max_elt

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
    i,cs,F.add_int_sets fs f

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

  let check_writes env_wide atoms =

    let call_build_observers p i x vs =
      if StringMap.mem x env_wide then
        Warn.user_error "No observers on wide accesses"
      else
        let vs =
          List.map
            (Misc.filter_map (fun (v,obs) ->
               if Array.length v > 0 then Some (v.(0),obs) else None ))
            vs in
        build_observers p i x vs in

    let cons_one x v fs =
      let loc = A.Loc x in
      if StringMap.mem x env_wide then
        F.cons_vec loc v fs
      else if Array.length v > 0 then
        (* For MTE locations, we may have only a tag write and not an *)
        (* Ord write. We therefore need a length check here. *)
        F.cons_int (A.Loc x) v.(0) fs
      else fs in

    let add_look_loc loc v k =
      if (not (StringSet.mem loc atoms) && O.optcond) then k
      else cons_one loc v k in

    let rec check_rec p i =

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
                  i,[],cons_one x v []
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
                    -> i,[],cons_one x v []
                  | Enforce ->
                      let i,c,f = call_build_observers p i x vs in
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
                      | _ -> i,[],cons_one x v []
                      end
                  |Four ->
                      begin match vs_flat with
                      | _x1::_x2::_x3::_x4::_x5::_ ->
                          Warn.fatal "More than four writes"
                      | _ -> i,[],cons_one x v []
                      end
                  | Infinity ->
                      i,[],cons_one x v []
                  | _ ->
                      let i,c,f = call_build_observers p i x vs in
                      i,c,add_look_loc x v f
                  end
          end in
          let i,cs,fs =
            check_rec (p+List.length c) i xvs in
          i,c@cs,f@fs in
    check_rec

  let compile_store st p init n =
    let ro,init,c,st = call_emit_access st p init n in
    assert (ro=None) ;
    init,c,st

  let rec compile_stores st p i ns c = match ns with
    | [] -> i,c,st
    | n::ns ->
       let sto = n.C.store in
       if sto == C.nil then
         compile_stores st p i ns c
       else
         let i,c0,st = compile_store st p i sto in
         let i,c,st = compile_stores st p i ns c in
         i,c0@c,st

      (* Local check of coherence *)

  let do_add_load bank st p i f x v =
    let r,i,c,st = Comp.emit_obs bank st p i x in
    let v =
      match bank with
      | Pair -> v+v
      | _ -> v in
    i,c,F.add_final_v p r (IntSet.singleton v) f,st

  let do_add_loop st p i f x v w =
    let r,i,c,st = Comp.emit_obs_not_value st p i x v in
    i,c,F.add_final_v p r (IntSet.singleton w) f,st

  let rec do_observe_local bank obs_type st p i code f x prev_v v =
    match obs_type with
    | Config.Straight ->
        let i,c,f,st = do_add_load bank st p i f x v in
        i,code@c,f,st
    |  Config.Fenced ->
        let i,c,f,st = do_add_load bank st p i f x v in
        let i,c',st = Comp.emit_fence st p i C.nil Comp.stronger_fence in
        let c = c'@c in
        i,code@c,f,st
    | Config.Loop ->
       begin match prev_v with
       | Some prev_v ->
          begin match bank with
          | Pair ->
             Warn.user_error "No loop observer for pairs"
          | _ -> ()
          end ;
          let i,c,f,st = do_add_loop st p i f x prev_v v in
          i,code@c,f,st
       | None ->
          do_observe_local bank Config.Fenced st p i code f x None v
       end

  let do_observe_local_simd st p i code f x bank nxt =
    let vs = nxt.C.vecreg in
    let r,i,c,st = Comp.emit_obs bank st p i x in
    let i,c,st =
      match O.obs_type with
      | Config.Straight -> i,c,st
      | Config.Fenced|Config.Loop -> (* No loop observed, too complex *)
         let i,c',st =
           Comp.emit_fence st p i C.nil Comp.stronger_fence in
         i,c'@c,st in
    let rs = r::A.get_friends st r in
    let f =
      List.fold_right2
        (fun r v -> F.add_final_loc p r (Code.add_vector O.hexa v))
        rs vs f in
    i,code@c,f,st

  let do_add_local_check_pte avoid st p i code f n x =
    if StringSet.mem (Misc.add_pte x) avoid then i,code,f,st
    else match U.find_next_pte_write n with
    | None -> assert false (* As U.check_here n returned true *)
    | Some nxt ->
        let v = nxt.C.evt.C.pte in
        let r,i,c,st = Comp.emit_obs Pte st p i x in
        i,code@c,F.add_final_pte p r v f,st

  let add_co_local_check_pte avoid ns st p i code f =
    let lst = Misc.last ns in
    if U.check_here lst then
      match lst.C.evt.C.loc,lst.C.evt.C.bank with
      | Data x,Pte ->
          do_add_local_check_pte avoid st p i code f lst x
      | _ -> i,code,f,st
    else
      i,code,f,st

  let add_co_local_check avoid_ptes lsts ns st p i code f =
    let lst = Misc.last ns in
    if U.check_here lst then
      match lst.C.evt.C.loc,lst.C.evt.C.bank with
      | Data x,(Ord|Pair) -> (* TODO check for -obs local mode and pairs *)
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | VecReg _ ->
            do_observe_local_simd st p i code f x bank nxt
         | _ ->
            let v = nxt.C.v
            and prev_v = lst.C.evt.C.v in
            let all_lst =
              try StringMap.find x lsts
              with Not_found -> C.evt_null in
            if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
            then
              i,code,F.cons_int_set (A.Loc x,IntSet.singleton v) f,st
            else
              let bank =
                match bank with
                | Pair -> Pair
                | _ -> Ord in
              do_observe_local  bank O.obs_type st p i code f x (Some prev_v) v
         end
      | Data x,Tag ->
          let v = lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs Tag st p i x in
          i,code@c,F.add_final_loc p r (Code.add_tag x v) f,st
      | Data x,CapaTag ->
          let v = lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs CapaTag st p i x in
          i,code@c,F.add_final_loc p r (Code.add_capability x v) f,st
      | Data x,CapaSeal ->
          let v = lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs CapaSeal st p i x in
          i,code@c,F.add_final_loc p r (Code.add_capability x v) f,st
      | Data x,Pte ->
          do_add_local_check_pte avoid_ptes st p i code f lst x
      | Data x,(VecReg _)->
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | Ord|Pair ->
            let v = nxt.C.v in
            do_observe_local bank O.obs_type st p i code f x None v
         | VecReg _ ->
            do_observe_local_simd st p i code f x bank nxt
         | _ -> Warn.user_error "Mixing SIMD and other variants"
         end
      | Code _,_ -> i,code,f,st
    else  i,code,f,st

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

  let gather_final_oks p st =
    let npairs = A.get_noks st in
    if npairs > 0 then
      [A.Loc (as_data (Code.myok_proc p)),IntSet.singleton npairs]
    else []

  let do_memtag = O.variant Variant_gen.MemTag
  let do_morello = O.variant Variant_gen.Morello
  let do_kvm = Variant_gen.is_kvm O.variant

  let compile_cycle ok initvals n =
    if O.verbose > 0 then begin
      Printf.eprintf "COMPILE CYCLE:\n%a" C.debug_cycle n
    end ;
    let open Config in
    Label.reset () ;
    let env_wide = C.get_wide n in
    let env_pair =
      if StringMap.is_empty env_wide then StringSet.empty
      else C.get_pair n in
    let splitted =  C.split_procs n in
    (* Split before, as  proc numbers added by side effet.. *)
    let cos0 = C.coherence n in
    let lsts = U.last_map cos0 in
    let cos = U.compute_cos cos0 in
    let last_ptes = if do_kvm then C.last_ptes n else [] in
    if O.verbose > 1 then
      Printf.eprintf "Last_Ptes: %s\n"
        (String.concat ","
           (List.map
              (fun (loc,v) ->
                Printf.sprintf "%s->%s" loc (C.PteVal.pp v)) last_ptes)) ;
    let no_local_ptes = StringSet.of_list (List.map fst last_ptes) in
    if O.verbose > 1 then U.pp_coherence cos0 ;
    let loc_writes = U.comp_loc_writes n in

    let rec do_rec p i = function
      | [] -> List.rev i,[],(C.EventMap.empty,[]),[],A.LocMap.empty
      | n::ns ->
          let i,c,(m,f),st =
            compile_proc Misc.identity false loc_writes A.st0 p No i n in
          let i,c,st = compile_stores st p i n c in
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
                | Local -> add_co_local_check no_local_ptes lsts n st p i c f
                | Avoid|Accept|Enforce|Three|Four|Infinity ->
                    add_co_local_check_pte no_local_ptes n st p i c f in
          let i,c,st = Comp.postlude st p i c in
          let env_p = A.get_env st in
          let foks = gather_final_oks p st in
          let i,cs,(ms,fs),ios,env = do_rec (p+1) i ns in
          let io = U.io_of_thread n in
          i,c::cs,
          (C.union_map m ms,F.add_int_sets (f@fs) foks),
          io::ios,
          A.LocMap.union_std
            (fun loc t1 t2 ->
               if TypBase.equal t1 t2 then Some t1
               else
                 Warn.fatal
                   "Location %s defined with contradictory types %s and %s"
                   (A.pp_location loc)
                   (TypBase.pp t1) (TypBase.pp t2))
          env_p env in
    let i,obsc,f =
      match O.cond with
      | Unicond -> [],[],[]
      | Cycle|Observe ->
          let atoms = U.comp_atoms n in
          check_writes env_wide atoms 0  [] cos in
    match splitted,O.cond with
    | [],_ -> Warn.fatal "No proc"
(*    | [_],Cycle -> Warn.fatal "One proc" *)
    | _,_ ->
        let i,c,(m,f),ios,env =
          if
            let len =  List.length splitted in
            O.nprocs <= 0 ||
            (if O.eprocs then len = O.nprocs else len <= O.nprocs)
          then
            let ess = List.map (List.map (fun n -> n.C.edge)) splitted in
            if ok ess then
              let i,cs,(m,fs),ios,env =
                do_rec (List.length obsc) i splitted in
              i,obsc@cs,(m,f@fs),ios,env
            else Warn.fatal "Last minute check"
          else  Warn.fatal "Too many procs" in
        let env = A.LocMap.map (fun t -> Typ t) env in
        let env =
          StringMap.fold
            (fun loc sz k ->
              let aloc = A.Loc loc in
              assert (not (A.LocMap.mem aloc k)) ;
              let ty =
                if StringSet.mem loc env_pair then O.typ
                else TypBase.Int in
              A.LocMap.add aloc (Array (ty,sz)) k)
          env_wide env in
        let env =
          let ptes = A.LocSet.of_list (F.extract_ptes f) in
          List.fold_left
            (fun m (loc,_) ->
              try
                (* Do not override previous typing bindings *)
                ignore (A.LocMap.find loc m); m
              with Not_found ->
                let t =
                  if A.LocSet.mem loc ptes then TypBase.pteval_t else O.typ in
                A.LocMap.add loc (Typ t) m)
            env f in
        let env =
          let globals = C.get_globals ~init:initvals n in
          let typ =
            if do_morello
            then TypBase.Std (TypBase.Unsigned,MachSize.S128)
            else O.typ in
        let typ = Typ typ in
          List.fold_left
            (fun m loc ->
              let loc = A.Loc loc in
              if A.LocMap.mem loc m then m
              else A.LocMap.add loc typ m)
            env globals in
        let flts =
          if O.variant Variant_gen.NoFault then []
          else if do_memtag then
            let tagchange =
              let ts =
                List.fold_left
                  (fun k ns ->
                    List.fold_left
                      (fun k n -> match n.C.evt.C.dir,n.C.evt.C.loc,n.C.evt.C.bank with
                      | Some W,Data x,Tag -> x::k
                      | _ -> k)
                      k ns) []
                  splitted in
              StringSet.of_list ts in
            let get_locs ns =
              let xs =
                List.fold_left
                  (fun k n -> match n.C.evt.C.loc,n.C.evt.C.bank with
                  | Data x,Ord when StringSet.mem x tagchange -> x::k
                  | _ -> k)
                  [] ns in
              StringSet.of_list xs in
            let flts = List.mapi (fun i ns -> i,get_locs ns) splitted in
            List.filter (fun (_,xs) -> not (StringSet.is_empty xs)) flts
          else if do_morello then
            let tagchange ns =
              let ts =
                List.fold_left
                  (fun k n -> match n.C.prev.C.edge.edge,n.C.evt.C.loc,n.C.evt.C.bank with
                  | Dp (dp,_,_),Data x,CapaTag when A.is_addr dp -> x::k
                  | Dp (dp,_,_),Data x,CapaSeal when A.is_addr dp -> x::k
                  | _ -> k)
                  [] ns in
              StringSet.of_list ts in
            let flts = List.mapi (fun i ns -> i,tagchange ns) splitted in
            List.filter (fun (_,xs) -> not (StringSet.is_empty xs)) flts
          else if do_kvm then
            let get_locs ns =
              let xs =
                List.fold_left
                  (fun k n ->
                    let e = n.C.evt in
                    match e.C.loc,e.C.bank with
                    | Data x,Ord -> x::k
                    | _ -> k)
                  [] ns in
              StringSet.of_list xs in
            let flts = List.mapi (fun i ns -> i,get_locs ns) splitted in
            List.filter (fun (_,xs) -> not (StringSet.is_empty xs)) flts
          else [] in
        let f =
          List.fold_left (fun f (x,p) -> F.cons_pteval (A.Loc x) p f) f last_ptes in
        let fc =
          match O.cond with
          | Unicond ->
              let evts =
                List.map
                  (List.map (fun n -> n.C.evt))
                  splitted in
              F.run evts m
          | Cycle -> F.check f
          | Observe -> F.observe f in
        let i = if do_kvm then A.complete_init O.hexa initvals i else i in
        (i,c,fc flts,env),
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
  let locs_init =
    List.fold_left
      (fun k (loc,_) -> A.LocSet.add loc k)
      A.LocSet.empty inits in
  fprintf chan "{" ;
  let pp =
    A.LocMap.fold
      (fun loc t k ->
        match t with
        | Array (ty,sz) ->
           sprintf "%s %s[%d];"
             (TypBase.pp ty) (A.pp_location loc) sz::k
        | Typ t ->
            if A.LocSet.mem loc locs_init then k
            else
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
        fprintf chan "%s%s%s;"
          (match loc with
           | Some _ ->
               begin
                 try
                   let t =
                     match A.LocMap.find left env with
                     | Typ t -> t
                     | _ -> raise Not_found in
                   TypBase.pp t ^ " "
                 with Not_found -> ""
               end
           | None -> "")
          (A.pp_location left)
          (match loc with
          | Some v -> sprintf "=%s" (A.pp_initval v)
          | None -> "") ;
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

  let dump_test_channel_full chan t =
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

  let dump_test_channel chan t =
    if O.cycleonly then
      if t.com <> "" then
        fprintf chan "%s: %s\n" t.name t.com
      else
       Warn.fatal "-cycleonly=true requested but no cycle generated"
    else
      dump_test_channel_full chan t

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
    (fun bd -> match bd with
      | (loc,Some (A.S v)) ->
          begin try
            let v =
              let p = StringMap.find v m in
              sprintf "%s:%s" (pp_proc p) v in
            loc,Some (A.S v)
          with Not_found -> bd
          end
      | (_,(Some (A.P _)|None)) as bd -> bd)
    env

let do_self =  O.variant Variant_gen.Self

let test_of_cycle name
  ?com ?(info=[]) ?(check=(fun _ -> true)) ?scope ?(init=[]) es c =
  let com = match com with None -> pp_edges es | Some com -> com in
  let (init,prog,final,env),(prf,coms) = compile_cycle check init c in
  let archinfo = Comp.get_archinfo c in
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
    info@myinfo@archinfo in

  { name=name ; info=info; com=com ;  edges = es ;
    init=init ; prog=prog ; scopes = scope; final=final ; env=env; }

let make_test name ?com ?info ?check ?scope es =
  try
    if O.verbose > 1 then eprintf "**Test %s**\n" name ;
    if O.verbose > 2 then eprintf "**Cycle %s**\n" (pp_edges es) ;
    let es,c,init = C.make es in
    test_of_cycle name ?com ?info ?check ?scope ~init es c
  with
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (pp_edges es) msg

end
