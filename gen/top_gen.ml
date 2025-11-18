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
  val metadata : bool
  val same_loc : bool
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
       env : typ A.LocMap.t ;
       obs : F.locations list
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

  let insert_overload n =
    let open E in
    match n.C.edge.E.edge with
    | Po (_,Dir R,Dir (W|R)) -> true
    | _ -> false

  type prev_load =
    | No       (* Non-existent or irrelevant *)
    | Yes of E.dp * A.arch_reg * C.node

(* Catch exchanges at the very last moment... *)
  let as_rmw n =
    let open E in
    match n.C.edge.E.edge with
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
        else call_emit_access st p init n
    | No,Code _ -> call_emit_access st p init n
    | Yes (dp,r1,n1),_ -> call_emit_access_dep st p init n dp r1 n1 in
    o,init,ip@i,st

let edge_to_prev_load o n = match o with
| None -> No
| Some r ->
    let open E in
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

  (* - `ro_prev` if there is a previous load
     - `st` is the machine state
     - `chk` if an initial is needed
     - `p` procedure (number)
     - `ro_prev` carried if the previous related load
     - `init` the inital values
     - `ns` input node list
  *)
  let rec compile_proc pref chk loc_writes st p ro_prev init ns = match ns with
  | [] -> init,pref [],(C.EventMap.empty,[]),st
  | n::ns ->
      if O.verbose > 1 then eprintf "COMPILE PROC: <%s>\n" (C.str_node n);
      begin match  n.C.edge.E.edge with
      (* There are following fences *)
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
      (* A single fence *)
      | E.Insert f ->
          let ro_prev,init,cs,st, n1 = match ro_prev with
          | No  -> let init, cs, st = Comp.emit_fence st p init n f in
              None, init, cs, st, n
          | Yes (dp,r1,n1) ->
            let ro_prev,init,cs,st =
              Comp.emit_fence_dp st p init n f dp r1 n1 in
              ro_prev,init,cs,st, n1 in
          let init,is,finals,st =
            compile_proc pref chk loc_writes st p (edge_to_prev_load ro_prev n1) init ns in
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
            else finals
          | Code _ ->
            begin match o with
            | None   -> finals (* Code write *)
            | Some r -> (* fetch! *)
              let m,fenv =  finals in
              m,F.add_final_v p r (IntSet.singleton @@ C.Value.to_int @@ U.fetch_val n)
                fenv
            end),
          st
      end
  (* END of compile_proc *)

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

  (* `env_wide` is a lookup table for the widths of locations and `atoms` is a set of all atom *)
  let check_writes env_wide atoms proc init cos =

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

    (* add the value `v` of `loc` into the accumulator `k` *)
    let add_look_loc loc v k =
      if (not (StringSet.mem loc atoms) && O.optcond) then k
      else cons_one loc v k in

    (* - `p`, process number
       - `i`, initial value accumulator
       - input `xvs`, type `U.cos` (defined in topUtil.ml)
         the final values of write events for all locations *)
    let check p i xvs =
      let open Config in
      (* The accumulator:
        - `p` procedure number.
        - `i` initial value, of type `init` (defined in archExtra_gen.ml).
          It remains unchanged in the default configuration.
          It is only updated via `call_build_observers`.
        - `cs` pseudo code. It is empty in the default configuration.
          It is only updated via `call_build_observers`.
        Element of `vxs` is `(x, vs)`.
        - `x` is the location represented by a string
        - `vs` the final value of the location `x` *)
      let _p,i,cs,fs = List.fold_left
        ( fun (p, i, cs, fs) (x, (vs : (C.Value.v array * IntSet.t) list list)) ->
        let vs = List.map ( List.map
            ( fun (v, vset) -> (Array.map C.Value.to_int v, vset) )
          ) vs in
        (* - `i`, new init value after this iteration,
           - `c`, new pseudo code to be added into `cs`,
           - `f`, new final value to be added into `fs` *)
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
          (* default configuration *)
          | Cycle -> begin
            (* If it is one location mode, `-oneloc`,
               we are more interested in checking the oldest value *)
            match vs with
            | [] -> i,[],[]
            (* the common case with one write event *)

            | [[(v,_)]] -> i,[],add_look_loc x v []
            | [[(_,_);(v,_)]] ->
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
                  if List.length vs_flat > 3 then
                    Warn.fatal "More than three writes"
                  else i,[],cons_one x v []
                | Four ->
                  if List.length vs_flat > 4 then
                    Warn.fatal "More than four writes"
                  else i,[],cons_one x v []
                | Infinity ->
                  i,[],cons_one x v []
                | _ ->
                  let i,c,f = call_build_observers p i x vs in
                  i,c,add_look_loc x v f
              end
        end in
        (* Update the procedure number, carry over the new `i`,
           and accumulating the new results of `c` and `f` *)
        (p+List.length c), i, cs@c, fs@f
      ) (p, i, [], []) xvs in
      i,cs,fs in
      (* END of check definition *)
    check proc init cos
(* END of check_writes *)

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
        (fun r v -> F.add_final_loc p r (v |> List.map C.Value.to_int |> Code.add_vector O.hexa))
        rs vs f in
    i,code@c,f,st

  let do_add_local_check_pte avoid st p i code f n x =
    if StringSet.mem (Misc.add_pte x) avoid then i,code,f,st
    else match U.find_next_pte_write n with
    | None -> assert false (* As U.check_here n returned true *)
    | Some nxt ->
        let v = C.Value.to_pte nxt.C.evt.C.v in
        let r,i,c,st = Comp.emit_obs Pte st p i x in
        i,code@c,F.add_final_pte p r v f,st

  let add_co_local_check avoid_ptes lsts ns st p i code f =
    let lst = Misc.last ns in
    if U.check_here lst then
      match lst.C.evt.C.loc,lst.C.evt.C.bank with
      | Data x,(Ord|Pair|Instr) -> (* TODO check for -obs local mode and pairs *)
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | VecReg _ ->
            do_observe_local_simd st p i code f x bank nxt
         | _ ->
            let v = C.Value.to_int nxt.C.v
            and prev_v = C.Value.to_int lst.C.evt.C.v in
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
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs Tag st p i x in
          i,code@c,F.add_final_loc p r (Code.add_tag x v) f,st
      | Data x,CapaTag ->
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs CapaTag st p i x in
          i,code@c,F.add_final_loc p r (Code.add_capability x v) f,st
      | Data x,CapaSeal ->
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,i,c,st = Comp.emit_obs CapaSeal st p i x in
          i,code@c,F.add_final_loc p r (Code.add_capability x v) f,st
      | Data x,Pte ->
          do_add_local_check_pte avoid_ptes st p i code f lst x
      | Data x,(VecReg _)->
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | Ord|Pair ->
            let v = C.Value.to_int nxt.C.v in
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
    let splitted = C.split_procs n in
    (* Split before, as  proc numbers added by side effet.. *)
    let cos0 = C.coherence n in
    let lsts = U.last_map cos0 in
    let cos = U.compute_cos cos0 in
    (* the post condition for checking PTE value *)
    let last_ptes = if do_kvm then C.last_ptes n else [] in
    if O.verbose > 1 then
      Printf.eprintf "Last_Ptes: %s\n"
        (String.concat ","
           (List.map
              (fun (loc,v) ->
                Printf.sprintf "%s->%s" loc (C.Value.pp_pte v)) last_ptes)) ;
    let no_local_ptes = StringSet.of_list (List.map fst last_ptes) in
    if O.verbose > 1 then U.pp_coherence cos0 ;
    let loc_writes = U.comp_loc_writes n in
    (* `do_rec` compile individual instructions *)
    let rec do_rec p i = function
      | [] -> List.rev i,[],(C.EventMap.empty,[]),[],A.LocMap.empty
      | n::ns ->
          let init_st = A.remove_reg_allocator A.st0 (A.used_register i) in
          let i,c,(m,f),st =
            compile_proc Misc.identity false loc_writes init_st p No i n in
          let i,c,st = compile_stores st p i n c in
          let xenv = Comp.get_xstore_results c in
          let f =
            List.fold_left
              (fun f (r,v) -> F.add_final_v p r (IntSet.singleton v) f)
              f xenv in
          let i,c,f,st =
            match O.cond,O.do_observers with
            | Unicond,_
            | _,(Avoid|Accept|Enforce|Three|Four|Infinity) -> i,c,f,st
            | (Cycle|Observe),Local ->
              add_co_local_check no_local_ptes lsts n st p i c f in
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
    (* end of `do_rec` *)
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
              (* - `i` is the initial value.
                 - `cs` pseudo code list
                 - `m` location to its last affect events
                 - `fs` final state
              *)
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
        (* Collect all the faults information to `flts` which contains two elements:
           - `pos_flts` includes all faults that eventually generate `Fault`
           - `neg_flts` includes all faults that eventually generate `~Fault`, (negation) *)
        let flts =
          (* `get_locs` filter the interesting faults from a node list `ns`.
             The behaviour based on different fault-related flags. *)
          let get_faults ns =
          (* TODO: the `if-else` pattern on flags is not a good idea as it may short circuit *)
           if O.variant Variant_gen.NoFault then
             F.FaultAtomSet.empty,F.FaultAtomSet.empty
           else if do_memtag || do_kvm || do_morello then
             List.fold_left
               (fun (pos_flts,neg_flts) n ->
                  let e = n.C.evt in
                  match e.C.check_fault,e.C.loc,e.C.bank with
                  | Some (lbl, do_fault),Data x,(Ord|CapaTag|CapaSeal) ->
                    let proc = n.C.evt.C.proc in
                    let flt = ((proc, Some lbl), Some (F.S x), None) in
                    (* Collect fault information based on `do_fault`,
                       add into either `pos_flts` for checking `Fault(...)`
                       or `neg_flts` for checking `~Fault(...)`. *)
                    if do_fault then F.FaultAtomSet.add flt pos_flts,neg_flts
                    else pos_flts,F.FaultAtomSet.add flt neg_flts
                  | _ -> (pos_flts,neg_flts)) (F.FaultAtomSet.empty,F.FaultAtomSet.empty) ns
           else (* no fault-related flag *)
             F.FaultAtomSet.empty,F.FaultAtomSet.empty
          in
          (* END of get_faults *)
          (* Extract faults from proc `i` and its node list `ns` *)
          get_faults (List.flatten splitted) in
        (* Add the all the pair in `last_ptes` into the post-condition `final_env` *)
        let f =
          List.fold_left (fun f (x,p) -> F.cons_pteval (A.Loc x) p f) f last_ptes in
        (* `fc` converts faults `flts` and final values of registers `final_env` to final *)
        let fc flts =
          match O.cond with
          | Unicond ->
              let evts =
                List.map
                  (List.map (fun n -> n.C.evt))
                  splitted in
              F.run evts m flts
          | Cycle -> F.check ~is_pos:(not O.neg) f flts
          | Observe -> F.exist_true in
        let obs =
          match O.cond with
          | Unicond | Cycle -> []
          | Observe -> F.observe f flts in
        let i = if do_kvm then A.complete_init O.hexa initvals i else i in
        (i,c,fc flts,env,obs),
        (U.compile_prefetch_ios (List.length obsc) ios,
         U.compile_coms splitted)
  (* END of compile_cycle *)


(********)
(* Dump *)
(********)

  module State = struct
  type state_atom = (A.location * (TestType.t * A.initval))
  type t = state_atom list

  let dump_state_atom state =
    let is_global = ( fun _ -> false ) in
    MiscParser.dump_state_atom
      is_global A.pp_location A.pp_initval state

    (* split the `state_atom list` to `state_atom list list`
       by grouping location for the same procedure *)
    let states_list states =
      List.fold_left
        ( fun (acc, prev_loc) (loc,v) ->
          let new_acc = match prev_loc,loc with
            | Some (A.Reg(p1,_)),A.Reg(p2,_) when p1 = p2 ->
              begin match acc with
                | [] -> assert false
                | hd :: tail -> ((loc,v) :: hd) :: tail
              end
            | _ -> [(loc,v)] :: acc in
          (new_acc, Some loc)
        ) ([],None) states
      |> fst

    let dump_state states = DumpUtils.dump_state dump_state_atom (states_list states)

    let typ_to_testtype = function
      | Typ TypBase.Int -> TestType.TyDef
      | Typ t -> TestType.Ty (TypBase.pp t)
      | Array (t,size) -> TestType.TyArray (TypBase.pp t,size)

    let init_type_env_to_states inits env =
      (* annotation the inits list with typing *)
      let type_inits = List.map ( fun (loc, init_opt) ->
          let typing = match loc with
            (* NOT add typing information of `pte` to loc *)
            | A.Loc l when Misc.is_pte l -> TestType.TyDef
            | _ -> A.LocMap.find_opt loc env
                    |> Option.map typ_to_testtype
                    |> Option.value ~default:TestType.TyDef in
          (loc,(typing,Option.value ~default:(A.S "0") init_opt))
          ) inits in
      (* Add those location exists in `env` but not in `inits`
         to the head of `type_inits` *)
      let locs = List.split inits |> fst |> A.LocSet.of_list in
      let extra_type_declaration =
        A.LocMap.filter ( fun k -> not @@ A.LocSet.mem k locs ) env
        (* to_list only exists after ocaml 5 *)
          |> A.LocMap.to_seq |> List.of_seq
          (* give default 0 value *)
          |> List.map ( fun (loc, t) -> (loc, (typ_to_testtype t, A.S "0")) ) in
      List.sort (fun (l, _) (r, _) -> A.location_compare r l) (type_inits @ extra_type_declaration)
          |> List.filter_map ( fun (loc, (typ, value)) ->
              match typ,value with
              (* fix the array value from `v` -> `{v, v, ..}` *)
              | TestType.TyArray (_,size), A.S v ->
                Some (loc, (typ, A.S ( sprintf "{%s}" ( List.init size ( fun _ -> v ) |> String.concat "," ) ) ) )
              (* fix the array only support basic value *)
              | TestType.TyArray (_), _ -> assert false
              (* remove if value and type is default *)
              | TestType.TyDef, A.S "0" -> None
              | _ -> Some (loc, (typ, value))
          )
  end

  module Dumper = SimpleDumper.Make
  (struct let compat = false end)
  (struct
    module A = A

    type v  = F.v
    let dump_v = F.dump_val

    type state = State.t
    let dump_state = State.dump_state

    type prop = F.prop
    let dump_prop = ConstrGen.prop_to_string F.pp_prop_atom
    let dump_constr = ConstrGen.constraints_to_string F.pp_prop_atom

    type location = A.location
    let dump_location = A.pp_location

    type fault_type = FaultType.No.t
    let dump_fault_type = FaultType.No.pp
  end)

  let add_proc_to_prog prog =
    List.mapi ( fun index code ->
      ((index, None, MiscParser.Main),code)
    ) prog

  let dump_test_channel_full chan t =
    let core_dumper_name = {
      Name.name = t.name;
      Name.file = "";
      Name.texname = "";
      Name.doc = t.com;
    } in
    let extra_data = match t.scopes with
      | None -> []
      | (Some  _ as scopes) ->
        [MiscParser.BellExtra ({
          BellInfo.regions = None;
          BellInfo.scopes = scopes;
          BellInfo.levels = None;
        })] in
    let core_dumper_t = {
      MiscParser.info = t.info ;
      MiscParser.init = State.init_type_env_to_states t.init t.env ;
      MiscParser.prog = add_proc_to_prog t.prog ;
      filter = None ;
      MiscParser.condition = t.final;
      MiscParser.locations = t.obs;
      MiscParser.extra_data = extra_data ;
    } in
    Dumper.dump_info chan core_dumper_name core_dumper_t

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

let tr_labs m init =
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
      | _ -> bd)
    init

let do_self =  O.variant Variant_gen.Self

let test_of_cycle name
  ?com ?(info=[]) ?(check=(fun _ -> true)) ?scope ?(init=[]) es c =
  let com = match com with None -> E.pp_edges es | Some com -> com in
  let (init,prog,final,env,obs),(prf,coms) = compile_cycle check init c in
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
    init=init ; prog=prog ; scopes = scope; final=final ; env=env; obs=obs}

let make_test name ?com ?info ?check ?scope es =
  try
    if O.verbose > 1 then eprintf "**Test %s**\n" name ;
    if O.verbose > 2 then eprintf "**Cycle %s**\n" (E.pp_edges es) ;
    let es,c,init = C.make es in
    test_of_cycle name ?com ?info ?check ?scope ~init es c
  with
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (E.pp_edges es) msg

end
