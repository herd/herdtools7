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
  val variant : Variant_gen.set
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

  let add_init_check check_init proc_num reg init =
    match check_init,reg with
    | true,Some r -> (A.Location.Location_reg (proc_num,r),Some (A.S "-1"))::init
    | _,_ -> init

  type typ = Typ of TypBase.t | Array of TypBase.t * int

(* Utilities *)

module U = TopUtils.Make(O)(Comp)

(***************)
(* Compilation *)
(***************)

  let rec emit_overload state proc_num init extra_loads loc =
    if extra_loads <= 0 then init,[],state
    else
      let overload_loc = sprintf "%s%i" loc extra_loads in
      let _,init,load_code,state = Comp.emit_load state proc_num init overload_loc in
      let init,following_code,state = emit_overload state proc_num init (extra_loads-1) loc in
      init,load_code@following_code,state

  let insert_overload n =
    let open E in
    match n.C.edge.E.edge with
    | Po (_,Dir R,Dir (W|R)) -> true
    | _ -> false

(* Catch exchanges at the very last moment... *)
  let as_rmw n =
    let open E in
    match n.C.edge.E.edge with
    | Rmw rmw -> rmw
    | _ -> assert false

  let call_emit_access state proc_num init n =
    let e = n.C.evt in
    if e.C.rmw then match e.C.dir with
    | Some R ->
        Comp.emit_rmw
          (as_rmw n) state proc_num init e n.C.next.C.evt
    | Some W|None ->
        None,init,[],state
    else if
      match e.C.atom with
      | None -> true
      | Some a ->
          begin match e.C.dir with
          | None -> false
          | Some d -> A.applies_atom a d
          end
    then
      let reg,init,code,state = Comp.emit_access state proc_num init e in
      let state = match reg,e.C.bank,n.C.next.C.evt.C.bank,O.typ with
        | Some r,Code.Ord,next_bank,TypBase.Int
          when next_bank = Code.Pair ||
               E.get_access_atom e.C.atom = Some (MachSize.Word,4) ->
            (* A word-sized ordinary read before a pair access already has the
               default `int` type; avoid emitting a redundant `uint32_t`. *)
            let loc = A.of_reg proc_num r in
            begin match A.LocMap.find_opt loc (A.get_env state) with
            | Some (TypBase.Std (TypBase.Unsigned,MachSize.Word)) ->
                A.add_type loc TypBase.Int state
            | _ -> state
            end
        | _ -> state in
      reg,init,code,state
    else
      Warn.fatal "annotation mismatch on edge %s, annotation '%s' on %s"
        (E.pp_edge n.C.edge)
        (E.pp_atom_option e.C.atom) (Misc.app_opt_def "_" pp_dir e.C.dir)

  let call_emit_access_dep state proc_num init n dp r1 n1 =
    let e = n.C.evt in
    match e.C.rmw,e.C.dir with
    | true,Some R -> Comp.emit_rmw_dep (as_rmw n) state proc_num init e n.C.next.C.evt dp r1 n1
    | true,(Some W|None) -> None,init,[],state
    | false,_ -> Comp.emit_access_dep state proc_num init e dp r1 n1

(* Encodes load of first non-initial value in chain,
   can poll on value in place of checking it *)
  let emit_access previous_load state proc_num init n =
    let init,ip,state = match O.overload,n.C.evt.C.loc with
    | Some ov,Data loc when insert_overload n ->
        emit_overload state proc_num init ov loc
    | _ -> init,[],state in
    let reg,init,code,state = match previous_load,n.C.evt.C.loc with
    | None,Data loc ->
        if U.do_poll n then
          let r,init,code,state =
            Comp.emit_load_one state proc_num init loc in
          Some r,init,code,state
        else call_emit_access state proc_num init n
    | None,Code _ -> call_emit_access state proc_num init n
    | Some (dp,r1,n1),_ -> call_emit_access_dep state proc_num init n dp r1 n1 in
    reg,init,ip@code,state

let edge_to_prev_load reg n = match reg with
| None -> None
| Some r ->
    let open E in
    begin match n.C.edge.E.edge with
    | Dp (dp,_,_) -> Some (dp,r,n)
    | _ -> None
    end

let get_fence n =
  match n.C.edge.E.edge with
  | E.Fenced (fence,_,_,_) -> Some fence
  | _ -> None


  let no_check_load init state = init,Misc.identity,state

  let rec collect_inserts = function
    | [] -> [],[]
    | n::ns as all -> match n.C.edge.E.edge with
      | E.Insert fence ->
          let fs,ns = collect_inserts ns in
          fence::fs,ns
      | _ -> [],all

  (** [compile_proc loc_writes proc_num state init nodes] compiles one process.

      - [loc_writes] identifies locations written by the cycle.
      - [proc_num] is the process number.
      - [state] is the current machine state.
      - [init] is the initial-state environment.
      - [nodes] are the remaining cycle nodes.
      - [check_init] records whether an initial-value check is needed.
      - [previous_load] is an optional previous load used for dependencies.
      - [prefix] stores code that must precede subsequently compiled
        instructions. For a [Node] followed by inserted fences, it accumulates
        the emitted fence code until the prefix is materialised. *)
  let compile_proc loc_writes proc_num state init nodes =
    let rec do_rec prefix check_init state previous_load init = function
  | [] -> init,prefix [],(C.EventMap.empty,[]),state
  | n::nodes ->
      if O.debug.Debug_gen.cycle then eprintf "COMPILE PROC: <%s>\n" (C.str_node n);
      begin match n.C.edge.E.edge with
      (* There are following fences *)
      | E.Node _ ->
          let fs,nodes = collect_inserts nodes in
          let prefix following_code =
            prefix
              (List.fold_right
                 (fun fence following_code ->
                   let _,fence_code,_ =
                     Comp.emit_fence state proc_num init n fence in
                   fence_code@following_code)
                 fs following_code) in
          do_rec prefix check_init state previous_load init nodes
      (* A single fence *)
      | E.Insert fence ->
          let loaded_reg,init,fence_code,state,n1 =
            match previous_load with
          | None ->
              let init,fence_code,state =
                Comp.emit_fence state proc_num init n fence in
              None,init,fence_code,state,n
          | Some (dp,r1,n1) ->
            let loaded_reg,init,fence_code,state =
              Comp.emit_fence_dp state proc_num init n fence dp r1 n1 in
              loaded_reg,init,fence_code,state,n1 in
          let previous_load = edge_to_prev_load loaded_reg n1 in
          let init,following_code,finals,state =
            do_rec prefix check_init state previous_load init nodes in
          init,fence_code@following_code,finals,state
      | _ -> (* Common case *)
          let reg,init,access_code,state =
            emit_access previous_load state proc_num init n in
          let next_check_init,add_check =
            match O.docheck,n.C.evt.C.dir,reg,nodes with
            | true,Some R,Some r,_::_ ->
                true,Comp.check_load proc_num r n.C.evt
            | _ -> check_init,no_check_load in
          let init,mk_c,state = add_check init state in
          let previous_load = edge_to_prev_load reg n in
          let init,following_code,finals,state =
            do_rec prefix next_check_init state previous_load init nodes in
          let init,current_fence_code,state =
            match get_fence n with
            | Some fence -> Comp.emit_fence state proc_num init n fence
            | None -> init,[],state in
          add_init_check check_init proc_num reg init,
          access_code@
          mk_c (current_fence_code @ following_code),
          (match n.C.evt.C.loc with
          | Data loc ->
            let call_add =
              StringSet.mem loc loc_writes && not (U.do_poll n) in
            if call_add then
              F.add_final (A.get_friends state) proc_num reg n finals
            else finals
          | Code _ ->
            begin match reg with
            | None -> finals (* Code write *)
            | Some r -> (* fetch! *)
              let m,fenv = finals in
              m,F.add_final_v proc_num r (IntSet.singleton @@ C.Value.to_int @@ U.fetch_val n)
                fenv
            end),
          state
      end in
    do_rec Misc.identity false state None init nodes
  (* END of compile_proc *)

(*************)
(* Observers *)
(*************)

let last_observation state proc_num init loc value =
  let reg,init,code,_st = Comp.emit_obs Ord state proc_num init loc in
  init,code,F.add_final_v proc_num reg value []

let observer needs_fence state proc_num init loc values =
  let rec do_rec state init = function
    | [] -> init,[],[]
    | [v] -> last_observation state proc_num init loc v
    | v::values ->
        let reg,init,code,state = Comp.emit_obs Ord state proc_num init loc in
        let init,separator_code,state =
          if needs_fence then
            Comp.emit_fence state proc_num init C.nil Comp.stronger_fence
          else init,[],state in
        let init,following_code,final = do_rec state init values in
        init,code@separator_code@following_code,
        F.add_final_v proc_num reg v final in
  do_rec state init values

let loop_observer state proc_num init loc = function
  | []|[_] -> init,[],[]
  | v::values ->
      let reg,init,code,state =
        Comp.emit_obs_not_zero state proc_num init loc in
      let rec do_loop state init prev_reg = function
        | [] -> assert false
        | [v] ->
            let reg,init,code,_ = Comp.emit_obs_not_eq state proc_num init loc prev_reg in
            init,code,F.add_final_v proc_num reg v []
        | v::values ->
            let reg,init,code,state = Comp.emit_obs_not_eq state proc_num init loc prev_reg in
            let init,following_code,final = do_loop state init reg values in
            init,code@following_code, F.add_final_v proc_num reg v final in
      let init,following_code,final = do_loop state init reg values in
      init,code@following_code,
      F.add_final_v proc_num reg v final

let rec do_opt_coherence values observers = function
  | [] -> [values]
  | (v,value_observers)::coherence ->
      let common_observers = IntSet.inter observers value_observers in
      if IntSet.is_empty common_observers then
        values::do_opt_coherence (IntSet.singleton v) value_observers coherence
      else
        do_opt_coherence (IntSet.add v values) value_observers coherence

let opt_coherence = function
  | [] -> assert false
  | (v,observers)::coherence ->
      do_opt_coherence (IntSet.singleton v) observers coherence

  let min_max xs =
    let ps = List.map (fun x -> IntSet.min_elt x,IntSet.max_elt x) xs in
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

  let build_observer state proc_num init loc values =
    let observer_values =
      if O.optcoherence && O.obs_type <> Config.Loop then
        let values = opt_coherence values in
        if O.debug.Debug_gen.cycle then begin
          eprintf "OPT:" ;
          List.iter
            (fun value_set ->
              eprintf " {%s}" (IntSet.pp_str "," (sprintf "%i") value_set))
            values ;
          eprintf "\n%!"
        end ;
        match values with
        | []|[_] -> None
        | first_value::values as all_values->
            if
              List.for_all
                (fun value_set ->
                  match IntSet.as_singleton value_set with
                  | Some _ -> true | None -> false)
                all_values then
              let last_value,values = Misc.pop_last first_value values in
              Some ((match values with [_] -> [] | _ -> values), [A.Location.Location_global loc,last_value])
            else
              Some (min_max all_values,[])
      else
        let values = List.map (fun (value,_obs) -> IntSet.singleton value) values in
        Some (values,[]) in
    Option.map
      (fun (values,coherence_final) ->
        let init,code,final =
          let open Config in
          match O.obs_type with
          | Straight -> observer false state proc_num init loc values
          | Fenced -> observer true state proc_num init loc values
          | Loop -> loop_observer state proc_num init loc values in
        init,code,F.add_int_sets final coherence_final)
      observer_values

  let rec build_observers proc_num init loc values_list =
    let open Config in
    match values_list,O.do_observers with
    | [],_ -> init,[],[]
    | []::values_list,_
    | [_]::values_list,(Avoid|Accept) ->
        build_observers proc_num init loc values_list
    | values::values_list,_ ->
        let saved_init = init in
        match build_observer A.st0 proc_num init loc values with
        | None -> build_observers proc_num init loc values_list
        | Some (init,code,cur_final) ->
          match code,O.do_observers with
          | _::_,Avoid -> Warn.fatal "Observer"
          | [],_ ->
              let init,extra_code,final = build_observers proc_num saved_init loc values_list in
              init,extra_code,cur_final@final
          | _,_ ->
              let init,extra_code,final = build_observers (proc_num+1) init loc values_list in
              init,code::extra_code,cur_final@final

  (* `env_wide` is a lookup table for the widths of locations and `atoms` is a set of all atom *)
  let check_writes env_wide atoms proc_num init coherences =

    let call_build_observers proc_num init loc values_list =
      if StringMap.mem loc env_wide then
        Warn.user_error "No observers on wide accesses"
      else
        let values_list =
          List.map
            (Misc.filter_map (fun (values,observed,_) ->
               if Array.length values > 0 then Some (values.(0),observed) else None))
            values_list in
        build_observers proc_num init loc values_list in

    let cons_one loc value final =
      match value with
      | None -> final
      | Some value ->
          let location = A.Location.Location_global loc in
          if StringMap.mem loc env_wide then
            F.cons_vec location value final
          else if Array.length value > 0 then
            (* For MTE locations, we may have only a tag write and not an *)
            (* Ord write. We therefore need a length check here. *)
            F.cons_int location value.(0) final
          else final in

    (* Add [value] at [loc] to [final]. *)
    let add_look_loc loc value final =
      match value with
      | None -> final
      | Some _ ->
          if not (StringSet.mem loc atoms) && O.optcond then final
          else cons_one loc value final in

    let last_write_event_adjacent_to_communication values =
      List.find_map
        (fun (value,_,check) -> if check then Some value else None)
        (List.rev values) in

    (* [coherences] is a [U.cos] value containing the final write values
       for every location. *)
    let build_write_checks proc_num init coherences =
      let open Config in
      let _,init,observer_code,final = List.fold_left
        (fun (proc_num,init,observer_code,final)
             (loc,(values_list : (C.Value.v array * IntSet.t * bool) list list)) ->
        let values_list =
          List.map
            (List.map
               (fun (values,observed,check) ->
                 Array.map C.Value.to_int values,observed,check))
            values_list in
        let values = List.flatten values_list in
        let value = last_write_event_adjacent_to_communication values in
        let init,code,cur_final = match O.cond with
          | Observe ->
            begin match values with
            | [] -> init,[],[]
            | _::_ -> init,[],cons_one loc value []
            end
          | Unicond -> assert false
          (* default configuration *)
          | Cycle -> begin
            (* If it is one location mode, `-oneloc`,
               we are more interested in checking the oldest value *)
            match values_list with
            | [] -> init,[],[]
            (* the common case with one write event *)

            | [[_]] -> init,[],add_look_loc loc value []
            | [[_;_]] ->
              begin match O.do_observers with
              | Local -> init,[],add_look_loc loc value []
              | Avoid|Accept|Three|Four|Infinity -> init,[],cons_one loc value []
              | Enforce ->
                let init,code,final = call_build_observers proc_num init loc values_list in
                init,code,add_look_loc loc value final
              end
            | _ ->
              begin match O.do_observers with
                | Local -> init,[],add_look_loc loc value []
                | Three ->
                  if List.length values > 3 then
                    Warn.fatal "More than three writes"
                  else init,[],cons_one loc value []
                | Four ->
                  if List.length values > 4 then
                    Warn.fatal "More than four writes"
                  else init,[],cons_one loc value []
                | Infinity ->
                  init,[],cons_one loc value []
                | _ ->
                  let init,code,final = call_build_observers proc_num init loc values_list in
                  init,code,add_look_loc loc value final
              end
        end in
        proc_num+List.length code,init,observer_code@code,final@cur_final
      ) (proc_num,init,[],[]) coherences in
      init,observer_code,final in
      (* END of build_write_checks definition *)
    build_write_checks proc_num init coherences
(* END of check_writes *)

  let compile_store state proc_num init n =
    let loaded_reg,init,code,state = call_emit_access state proc_num init n in
    assert (loaded_reg=None) ;
    init,code,state

  let rec compile_stores state proc_num init nodes code = match nodes with
    | [] -> init,code,state
    | n::nodes ->
       let store = n.C.store in
       if store == C.nil then
         compile_stores state proc_num init nodes code
       else
         let init,store_code,state = compile_store state proc_num init store in
         let init,code,state = compile_stores state proc_num init nodes code in
         init,store_code@code,state

      (* Local check of coherence *)

  let observer_bank_of_cell cell = match Array.length cell with
    | 1 -> Ord
    | 2 -> Pair
    | n -> Warn.fatal "No local observer for %d coherence cells" n

  let do_add_load bank state proc_num init final loc cell =
    let r,init,code,state = Comp.emit_obs bank state proc_num init loc in
    let rs = match bank with Pair -> r::A.get_friends state r | _ -> [r] in
    let final = List.fold_left2
      (fun final r v -> F.add_final_v proc_num r (IntSet.singleton (C.Value.to_int v)) final)
      final rs (Array.to_list cell) in
    init,code,final,state

  let do_add_loop state proc_num init final loc v w =
    let r,init,c,state = Comp.emit_obs_not_value state proc_num init loc v in
    init,c,F.add_final_v proc_num r (IntSet.singleton w) final,state

  let rec do_observe_local bank obs_type state proc_num init code final loc prev_v cell =
    let v = C.Value.to_int cell.(0) in
    match obs_type with
    | Config.Straight ->
        let init,c,final,state = do_add_load bank state proc_num init final loc cell in
        init,code@c,final,state
    | Config.Fenced ->
        let init,c,final,state = do_add_load bank state proc_num init final loc cell in
        let init,c',state = Comp.emit_fence state proc_num init C.nil Comp.stronger_fence in
        let c = c'@c in
        init,code@c,final,state
    | Config.Loop ->
       begin match prev_v with
       | Some prev_v ->
          begin match bank with
          | Pair ->
             Warn.user_error "No loop observer for pairs"
          | _ -> ()
          end ;
          let init,c,final,state = do_add_loop state proc_num init final loc prev_v v in
          init,code@c,final,state
       | None ->
          do_observe_local bank Config.Fenced state proc_num init code final loc None cell
       end

  let do_observe_local_simd state proc_num init code final loc bank nxt =
    let vs = nxt.C.vecreg in
    let r,init,c,state = Comp.emit_obs bank state proc_num init loc in
    let init,c,state =
      match O.obs_type with
      | Config.Straight -> init,c,state
      | Config.Fenced|Config.Loop -> (* No loop observed, too complex *)
         let init,c',state =
           Comp.emit_fence state proc_num init C.nil Comp.stronger_fence in
         init,c'@c,state in
    let rs = r::A.get_friends state r in
    let final =
      List.fold_right2
        (fun r v -> F.add_final_loc proc_num r (v |> List.map C.Value.to_int |> Code.add_vector O.hexa))
        rs vs final in
    init,code@c,final,state

  let do_add_local_check_pte avoid state proc_num init code final n loc =
    if StringSet.mem (Misc.add_pte loc) avoid then init,code,final,state
    else match U.find_next_pte_write n with
    | None -> assert false (* As U.check_here n returned true *)
    | Some nxt ->
        let v = C.Value.to_pte nxt.C.evt.C.v in
        let r,init,c,state = Comp.emit_obs Pte state proc_num init loc in
        init,code@c,F.add_final_pte proc_num r v final,state

  let add_co_local_check avoid_ptes lsts ns state proc_num init code final =
    let lst = Misc.last ns in
    if U.check_here lst then
      match lst.C.evt.C.loc,lst.C.evt.C.bank with
      | Data loc,(Ord|Pair|Instr) ->
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | VecReg _ ->
            do_observe_local_simd state proc_num init code final loc bank nxt
         | _ ->
            let v = C.Value.to_int nxt.C.v
            and prev_v = C.Value.to_int lst.C.evt.C.v in
            let all_lst =
              try StringMap.find loc lsts
              with Not_found -> C.evt_null in
            if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
            then
              init,code,F.cons_int_set (A.Location.Location_global loc,IntSet.singleton v) final,state
            else
              let bank = observer_bank_of_cell nxt.C.cell in
              do_observe_local bank O.obs_type state proc_num init code final loc (Some prev_v) nxt.C.cell
         end
      | Data loc,Tag ->
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,init,c,state = Comp.emit_obs Tag state proc_num init loc in
          init,code@c,F.add_final_loc proc_num r (Code.add_tag loc v) final,state
      | Data loc,CapaTag ->
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,init,c,state = Comp.emit_obs CapaTag state proc_num init loc in
          init,code@c,F.add_final_loc proc_num r (Code.add_capability loc v) final,state
      | Data loc,CapaSeal ->
          let v = C.Value.to_int lst.C.next.C.evt.C.v in
          let r,init,c,state = Comp.emit_obs CapaSeal state proc_num init loc in
          init,code@c,F.add_final_loc proc_num r (Code.add_capability loc v) final,state
      | Data loc,Pte ->
          do_add_local_check_pte avoid_ptes state proc_num init code final lst loc
      | Data loc,(VecReg _)->
         let nxt = lst.C.next.C.evt in
         let bank = nxt.C.bank in
         begin match bank with
         | Ord|Pair ->
            let bank = observer_bank_of_cell nxt.C.cell in
            do_observe_local bank O.obs_type state proc_num init code final loc None nxt.C.cell
         | VecReg _ ->
            do_observe_local_simd state proc_num init code final loc bank nxt
         | _ -> Warn.user_error "Mixing SIMD and other variants"
         end
      | Code _,_ -> init,code,final,state
    else init,code,final,state

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

  let gather_final_oks proc_num state =
    let npairs = A.get_noks state in
    if npairs > 0 then
      [A.Location.Location_global (as_data (Code.myok_proc proc_num)),IntSet.singleton npairs]
    else []

  let do_memtag = Variant_gen.has Variant_gen.MemTag O.variant
  let do_async = Variant_gen.has Variant_gen.Async O.variant
  let do_morello = Variant_gen.has Variant_gen.Morello O.variant
  let do_kvm = Variant_gen.is_kvm O.variant

  let compile_cycle check initial_values cycle =
    if O.debug.Debug_gen.cycle then begin
      Printf.eprintf "COMPILE CYCLE:\n%a" C.debug_cycle cycle
    end ;
    let open Config in
    Label.reset () ;
    let wide_env = C.get_wide cycle in
    let pair_locs =
      if StringMap.is_empty wide_env then StringSet.empty
      else C.get_pair cycle in
    let processes = C.split_procs cycle in
    (* Split before, as proc numbers added by side effect. *)
    let coherence = C.coherence cycle in
    let last_writes = U.last_map coherence in
    let coherences = U.compute_cos coherence in
    (* the post condition for checking PTE value *)
    let last_ptes = if do_kvm then C.last_ptes cycle else [] in
    if O.debug.Debug_gen.cycle then
      Printf.eprintf "Last_Ptes: %s\n"
        (String.concat ","
           (List.map
              (fun (loc,v) ->
                Printf.sprintf "%s->%s" loc (C.Value.pp_pte v)) last_ptes)) ;
    let avoid_local_ptes = StringSet.of_list (List.map fst last_ptes) in
    if O.debug.Debug_gen.cycle then U.pp_coherence coherence ;
    let loc_writes = U.comp_loc_writes cycle in
    let rec compile_processes proc_num init = function
      | [] -> List.rev init,[],(C.EventMap.empty,[]),[],A.LocMap.empty
      | nodes::remaining_processes ->
          let initial_state =
            A.remove_reg_allocator A.st0 (A.used_register init) in
          let init,code,(event_map,final),state =
            compile_proc loc_writes proc_num initial_state init nodes in
          let init,code,state =
            compile_stores state proc_num init nodes code in
          let exclusive_results = Comp.get_xstore_results code in
          let final =
            List.fold_left
              (fun final (reg,value) ->
                F.add_final_v proc_num reg (IntSet.singleton value) final)
              final exclusive_results in
          let init,code,final,state =
            match O.cond,O.do_observers with
            | Unicond,_
            | _,(Avoid|Accept|Enforce|Three|Four|Infinity) -> init,code,final,state
            | (Cycle|Observe),Local ->
                add_co_local_check avoid_local_ptes last_writes nodes state proc_num init code final in
          let init,code,state = Comp.postlude state proc_num init code in
          let process_env = A.get_env state in
          let ok_final = gather_final_oks proc_num state in
          let init,code_list,(event_maps,remaining_final),ios,env =
            compile_processes (proc_num+1) init remaining_processes in
          let io = U.io_of_thread nodes in
          init,code::code_list,
          (C.union_map event_map event_maps, F.add_int_sets (final@remaining_final) ok_final),
          io::ios,
          A.LocMap.union_std
            (fun loc t1 t2 ->
               if TypBase.equal t1 t2 then Some t1
               else
                 Warn.fatal
                   "Location %s defined with contradictory types %s and %s"
                   (A.pp_location loc)
                   (TypBase.pp t1) (TypBase.pp t2))
          process_env env in
    (* end of `compile_processes` *)
    let init,observer_code,final =
      match O.cond with
      | Unicond -> [],[],[]
      | Cycle|Observe ->
          let atoms = U.comp_atoms cycle in
          check_writes wide_env atoms 0 [] coherences in
    match processes with
    | [] -> Warn.fatal "No proc"
    | _ :: _ ->
        let init,code,(event_map,final),ios,type_env =
          if
            let process_count = List.length processes in
            O.nprocs <= 0 ||
            (if O.eprocs then process_count = O.nprocs else process_count <= O.nprocs)
          then
            let edge_lists = List.map (List.map (fun n -> n.C.edge)) processes in
            if check edge_lists then
              let init,process_code,(event_map,process_final),ios,type_env =
                compile_processes (List.length observer_code) init processes in
              init,observer_code@process_code, (event_map,final@process_final),ios,type_env
            else Warn.fatal "Last minute check"
          else Warn.fatal "Too many procs" in
        let type_env = A.LocMap.map (fun typ -> Typ typ) type_env in
        let type_env =
          StringMap.fold
            (fun loc size type_env ->
              let location = A.Location.Location_global loc in
              assert (not (A.LocMap.mem location type_env)) ;
              let typ =
                if StringSet.mem loc pair_locs then O.typ
                else TypBase.Int in
              A.LocMap.add location (Array (typ,size)) type_env)
          wide_env type_env in
        let type_env =
          let pte_locs = A.LocSet.of_list (F.extract_ptes final) in
          List.fold_left
            (fun type_env (loc,_) ->
              try
                (* Do not override previous typing bindings *)
                ignore (A.LocMap.find loc type_env); type_env
              with Not_found ->
                let typ =
                  if A.LocSet.mem loc pte_locs then TypBase.pteval_t
                  else O.typ in
                A.LocMap.add loc (Typ typ) type_env)
            type_env final in
        let type_env =
          let globals = C.get_globals ~init:initial_values cycle in
          let global_typ =
            if do_morello
            then TypBase.Std (TypBase.Unsigned,MachSize.S128)
            else O.typ in
          let global_typ = Typ global_typ in
          List.fold_left
            (fun type_env loc ->
              let location = A.Location.Location_global loc in
              if A.LocMap.mem location type_env then type_env
              else A.LocMap.add location global_typ type_env)
            type_env globals in
        (* Collect positive [Fault] and negative [~Fault] conditions. *)
        let faults =
          let get_faults nodes =
          (* TODO: the `if-else` pattern on flags is not a good idea as it may short circuit *)
           if Variant_gen.has Variant_gen.NoFault O.variant then
             F.FaultAtomSet.empty,F.FaultAtomSet.empty
           else if do_memtag || do_kvm || do_morello then
             List.fold_left
               (fun (positive_faults,negative_faults) node ->
                  let event = node.C.evt in
                  match event.C.check_fault,event.C.loc,event.C.bank with
                  | Some (label,do_fault),Data loc,(Ord|CapaTag|CapaSeal) ->
                    let proc_num = event.C.proc in
                    (* No location and label information if we are in `async` *)
                    let fault =
                      if do_async then ((proc_num,None),None,None)
                      else ((proc_num,Some label),Some (F.S loc),None) in
                    if do_fault then
                      F.FaultAtomSet.add fault positive_faults,negative_faults
                    else
                      positive_faults,
                      F.FaultAtomSet.add fault negative_faults
                  | _ -> positive_faults,negative_faults)
               (F.FaultAtomSet.empty,F.FaultAtomSet.empty) nodes
           else (* no fault-related flag *)
             F.FaultAtomSet.empty,F.FaultAtomSet.empty
          in
          get_faults (List.flatten processes) in
          (* END of get_faults *)
        let final =
          List.fold_left (fun final (loc,pte) ->
              F.cons_pteval (A.Location.Location_global loc) pte final)
            final last_ptes in
        let make_condition faults =
          match O.cond with
          | Unicond ->
              let events =
                List.map
                  (List.map (fun n -> n.C.evt))
                  processes in
              F.forall_condition events event_map faults
          | Cycle -> F.exists_condition ~is_pos:(not O.neg) final faults
          | Observe -> F.observe_condition in
        let observed_locations =
          match O.cond with
          | Unicond | Cycle -> []
          | Observe -> F.location_list final faults in
        let init = if do_kvm then A.complete_init O.hexa initial_values init else init in
        (init,code,make_condition faults,type_env,observed_locations),
        (U.compile_prefetch_ios (List.length observer_code) ios,
         U.compile_coms processes)
  (* END of compile_cycle *)


(********)
(* Dump *)
(********)

  module State = struct
  type state_atom = (A.location * (TestType.t * A.initval))
  type t = state_atom list

  let dump_state_atom state =
    let is_global = ( function A.Location.Location_global _ -> true | _ -> false ) in
    MiscParser.dump_state_atom
      is_global A.pp_location A.pp_initval state

    (* split the `state_atom list` to `state_atom list list`
       by grouping location for the same procedure *)
    let states_list states =
      Misc.group_by_int
      (function A.Location.Location_global _ -> None | A.Location.Location_reg (proc_num,_) -> Some proc_num)
      states

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
            | A.Location.Location_global l when Misc.is_pte l -> TestType.TyDef
            | _ -> A.LocMap.find_opt loc env
                    |> Option.map typ_to_testtype
                    |> Option.value ~default:TestType.TyDef in
          (loc,(typing,Option.value ~default:(A.S "0") init_opt))
          ) inits in
      (* Add those location exists in `env` but not in `inits`
         to the head of `type_inits` *)
      let locs = List.split inits |> fst |> A.LocSet.of_list in
      let extra_type_declaration =
        A.LocMap.filter ( fun k _ -> not @@ A.LocSet.mem k locs ) env
          |> A.LocMap.to_list
          (* give default 0 value *)
          |> List.map ( fun (loc, t) -> (loc, (typ_to_testtype t, A.S "0")) ) in
      List.sort (fun (l, _) (r, _) -> A.location_compare l r) (type_inits @ extra_type_declaration)
          |> List.filter_map ( fun (loc, (typ, value)) ->
              match typ,value with
              (* expand the array initial value from a single value `v` to
                 a list initial value for elements `{v, v, ..}` *)
              | TestType.TyArray (_,size), A.S v ->
                Some (loc, (typ, A.S ( sprintf "{%s}" ( List.init size ( fun _ -> v ) |> String.concat "," ) ) ) )
                (* rule out the initial array value if the `value` is not
                 a basic value `A.S v` *)
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

    type v = F.v
    let dump_v = F.dump_val

    type state = State.t
    let dump_state = State.dump_state

    type prop = F.prop
    let dump_prop = F.dump_prop
    let dump_constr = F.dump_constr

    type location = A.location
    let dump_location = A.pp_location

    type fault_type = FaultType.No.t
    let dump_fault_type = FaultType.No.pp
  end)

  let add_proc_to_prog prog =
    List.mapi ( fun index code ->
      ((index, None, MiscParser.Main),code)
    ) prog

  let bell_data = function
    | None -> []
    | (Some _ as scopes) ->
        [MiscParser.BellExtra ({
          BellInfo.regions = None;
          BellInfo.scopes = scopes;
          BellInfo.levels = None;
        })]

  type test = {
    name : string;
    com : string;
    edges : edge list;
    dump : Dumper.test;
  }

  let get_nprocs t = List.length t.dump.MiscParser.prog
  let get_name t = t.name
  let set_name t name = {t with name}
  let set_scope t scope =
    let info = ("Scopes",BellInfo.pp_scopes scope)::t.dump.MiscParser.info in
    let dump = {
      t.dump with
      MiscParser.info;
      MiscParser.extra_data = bell_data (Some scope);
    } in
    {t with dump}

  let extract_edges {edges; _} = edges

  let dump_test_channel_full chan (t : test) =
    let core_dumper_name = {
      Name.name = t.name;
      Name.file = "";
      Name.texname = "";
      Name.doc = if O.metadata then t.com else "";
    } in
    Dumper.dump_info chan core_dumper_name t.dump

  let dump_test_channel chan t =
    if O.cycleonly then
      if t.com <> "" then
        fprintf chan "%s: %s\n" t.name t.com
      else
       Warn.fatal "-cycleonly=true requested but no cycle generated"
    else
      dump_test_channel_full chan t

  let num_labels code =
    let rec num_ins proc_num labels = function
      | A.Label (lab,instruction) ->
          num_ins proc_num (StringMap.add lab proc_num labels) instruction
      | _ -> labels in
    let num_code proc_num = List.fold_left (num_ins proc_num) in
    let _,labels =
      List.fold_left
        (fun (proc_num,labels) c -> proc_num+1,num_code proc_num labels c)
        (0,StringMap.empty) code in
    labels

let tr_labs m init =
  List.map
    (fun bd -> match bd with
      | (loc,Some (A.S v)) ->
          begin try
            let v =
              let proc_num = StringMap.find v m in
              sprintf "%s:%s" (pp_proc proc_num) v in
            loc,Some (A.S v)
          with Not_found -> bd
          end
      | _ -> bd)
    init

let variant_info =
    List.filter_map
    ( fun t -> if Variant_gen.has t O.variant then Variant_gen.pp_herd_variant t else None)
    Variant_gen.all_t
    |> ( function
      | [] -> None
      | l -> Some ("Variant", StringSet.pp_id " " (StringSet.of_list l)) )
let basic_info scope prefetch com_edges cycle_description =
  let convert_to_option_pair key value =
    match key,value with "",_ | _,"" -> None | _ -> Some(key, value) in
  (* Some internal metadata might be empty, if the string value is "".
     We covert to option type then filter_map *)
  List.filter_map Fun.id
    ( ( convert_to_option_pair "Generator" O.generator )
    :: ( Option.map ( fun value -> ("Scopes", BellInfo.pp_scopes value) ) scope )
    (* Prefetch surpress in instruction fetch, `ifetch`, test cases *)
    :: ( if Variant_gen.has Variant_gen.Self O.variant then None else convert_to_option_pair "Prefetch" prefetch )
    :: ( convert_to_option_pair "Com" com_edges )
    :: ( convert_to_option_pair "Orig" cycle_description )
    :: [] )

(* Merge user specified `info` with internal defined `info`.
  The key is case inssensitive. Yet the final output result will
  capitalise the first letter. If there is overlap between `lhs`
  and `rhs`. The value in the `rhs` will be concatenated to the one
  in the `lhs` *)
let merge_to_left lhs rhs =
  let rec update_if_match list key value =
    let key = String.capitalize_ascii key in
    match list with
    (* do not find, so append *)
    | [] -> [(key,value)]
    | (k,v) :: tail when String.capitalize_ascii k = key ->
        (key, v ^ " " ^ value) :: tail
    | kv :: tail -> kv :: update_if_match tail key value in
  List.fold_left
    ( fun lhs (k,v) -> update_if_match lhs k v )
    lhs rhs

let test_of_cycle name
  ?com ?(info=[]) ?(check=(fun _ -> true)) ?scope ?(init=[]) edges c =
  let com = match com with None -> E.pp_edges ~separate:true edges | Some com -> com in
  let initial_values = init in
  let (init,prog,final,env,obs),(prf,coms) =
    compile_cycle check initial_values c in
  let m_labs = num_labels prog in
  let init = tr_labs m_labs init in
  let coms = String.concat " " coms in
  (* We still print out `variant` and `archinfo` even when metadata is false *)
  let mandatory_info =
    match variant_info with
    | None -> Comp.get_archinfo c
    | Some variant -> variant :: Comp.get_archinfo c in
  let info =
    if O.metadata then
      merge_to_left ( mandatory_info @ (basic_info scope prf coms com) ) info
    else mandatory_info in
  let dump = {
    MiscParser.info = info;
    MiscParser.init = State.init_type_env_to_states init env;
    MiscParser.prog = add_proc_to_prog prog;
    filter = None;
    MiscParser.condition = final;
    MiscParser.locations = obs;
    MiscParser.extra_data = bell_data scope;
  } in
  {name; com; edges; dump}

let make_test name ?com ?info ?check ?scope es =
  try
    if O.debug.Debug_gen.cycle then begin
      eprintf "**Test %s**\n" name ;
      eprintf "**Cycle %s**\n" (E.pp_edges ~separate:true es) ;
      eprintf "**Internal %s**\n" (E.pp_edges es)
    end ;
    let es,c,initial_values = C.make es in
    test_of_cycle name ?com ?info ?check ?scope ~init:initial_values es c
  with
  | Misc.Fatal msg|Misc.UserError msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (E.pp_edges es) msg

end
