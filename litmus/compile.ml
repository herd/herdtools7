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

module type Config = sig
  val numeric_labels : bool
  val timeloop : int
  val barrier : Barrier.t
  val mode : Mode.t
  val variant : Variant_litmus.t -> bool
end

module Default = struct
  let numeric_labels = false
  let timeloop = 0
  let barrier = Barrier.UserFence
  let mode = Mode.Std
  let variant _ = false
end

let get_fmt hexa base = match CType.get_fmt hexa base with
| Some fmt -> fmt
| None -> Warn.fatal "No format for type '%s'" base

let base =  CType.Base "int"
let pointer = CType.Pointer base

module Generic (A : Arch_litmus.Base)
    (C:Constr.S
    with type location = A.location and module LocSet = A.LocSet) = struct

      module G = Global_litmus
      open CType

      let base =  A.base_type
      let pointer = CType.Pointer base
      let code_pointer = Pointer (Base "ins_t")
      let tag = Base "tag_t"
      let pteval_t = Base "pteval_t"

      let typeof = function
        | Constant.Concrete _ -> base
        | Constant.Symbolic _ -> pointer
        | Constant.Label _ -> code_pointer
        | Constant.Tag _ -> tag
        | Constant.PteVal _ -> pteval_t

      let misc_to_c  = function
        | MiscParser.TyDef -> base
        | MiscParser.TyDefPointer  -> pointer
        | MiscParser.Ty t -> Base t
        | MiscParser.Atomic t -> Atomic (Base t)
        | MiscParser.Pointer t -> Pointer (Base t)
        | MiscParser.TyArray (t,sz) -> Array (t,sz)

      let type_in_init p reg =
        let rec find_rec = function
          | [] -> None
          | (loc,(t,_))::rem ->
              begin match loc with
              | A.Location_reg (q,r) when q = p &&  A.reg_compare reg r = 0
                -> Some (misc_to_c t)
              | _ -> find_rec rem
              end in
        find_rec


      let type_in_final p reg final flocs =
        Misc.proj_opt
          base
          (ConstrGen.fold_constr
             (fun a t ->
               let open ConstrGen in
               match a with
               | LV (A.Location_reg (q,r),v) when p=q && A.reg_compare reg r = 0 ->
                   begin match typeof v,t with
                   | (Base _, Some (Base _)) ->
                   t (* location takes precedence *)
                   | (Pointer (Base s1), Some (Pointer (Base s2)))
                     when Misc.string_eq s1 s2 ->
                       t
                   | (ty, None) -> Some ty
                   | (loc_ty, Some cond_ty) ->
                       Warn.fatal
                         "Type mismatch between the locations \
                         (type: %s) and the final condition (type: %s)"
                               (CType.dump loc_ty)
                               (CType.dump cond_ty)
                   end
               | _ -> t)
             final
             (List.fold_right
                (fun (loc,t) k -> match loc with
                | A.Location_reg (q,r) when p=q && A.reg_compare reg r = 0 ->
                    begin match t with
                    | MiscParser.TyDef -> None
                    | MiscParser.TyDefPointer -> Some pointer
                    | MiscParser.Ty s -> Some (Base s)
                    | MiscParser.Atomic s -> Some (Atomic (Base s))
                    | MiscParser.Pointer s -> Some (Pointer (Base s))
                    | MiscParser.TyArray _ -> assert false (* No array register *)
                    end
                | _ -> k)
                flocs
                None)
          )

      let add_addr_type a ty env =
(*      Printf.eprintf "Type %s : %s\n"  a (CType.dump ty) ; *)
        try
          let tz = G.Map.find a env in
          match ty,tz with
          | (Pointer (Base s1), Pointer (Base s2))
          | (Atomic (Base s1), Atomic (Base s2))
          | (Base s1, Base s2)
            when Misc.string_eq s1 s2 -> env
          | _,_ (* (Pointer _|Base _),(Pointer _|Base _) *) ->
              Warn.fatal
                "Type mismatch detected on location %s, required %s vs. found %s"
                (Global_litmus.pp a) (dump ty) (dump tz)
        with
          Not_found -> G.Map.add a ty env

(********************)
(* Complete typing  *)
(********************)

(* Typing of final items, find as much as possible,
   do not overwrite known type *)
      let type_atom_final a env =
        let open ConstrGen in
        match a with
        | LV (loc,v) ->
            begin try
              ignore (A.LocMap.find loc env) ;
              env
            with Not_found ->  A.LocMap.add loc (typeof v) env end
        | LL _|FF _ -> env

      let type_final final env =
        ConstrGen.fold_constr type_atom_final final env

      let type_prop prop env = ConstrGen.fold_prop type_atom_final prop env

(* locations, default and explicit types *)
      let type_locations flocs env =
        List.fold_left
          (fun env (loc,t) ->
            try
              ignore (A.LocMap.find loc env) ; env
            with
            | Not_found ->
                A.LocMap.add loc (misc_to_c t) env)
          env flocs

(* init, default and explicit types *)
      open Printf

      let type_init init env =
        List.fold_left
          (fun env (loc,(t,v)) -> match t with
          | MiscParser.TyDef ->
              begin try
                ignore (A.LocMap.find loc env) ;
                env
              with Not_found ->
                A.LocMap.add loc (typeof v) env
              end
          | _ -> A.LocMap.add loc (misc_to_c t) env)
          env init

      let type_init_values init env =
        let open Constant in
        List.fold_left
          (fun env (loc,(t,v)) -> match loc,v with
          | _,Constant.Concrete _ -> env
          | A.Location_global _,Symbolic s ->
              let a = A.Location_global (G.tr_symbol s) in
              begin try
                ignore (A.LocMap.find a env) ;
                env
              with Not_found ->
                let open MiscParser in
                let tv = match t with
                | TyDefPointer|TyDef -> TyDef
                | Pointer s -> Ty s
                | _ ->
                    Warn.user_error
                      "variable %s should be of pointer type"
                      (A.pp_location loc) in
                A.LocMap.add a (misc_to_c tv) env
              end
          | _,_ -> env)
          env init

      let dump_type_env tag env =
        let bds =
          A.LocMap.fold
            (fun loc ty k -> (loc,ty)::k)
            env [] in
        let pp =
          List.map
            (fun (loc,ty) ->
              sprintf "<%s,%s>"
                (A.pp_location loc)
                (CType.dump ty))
            bds in
        eprintf "%s: %s\n" tag (String.concat " " pp)

      let debug = false

      let build_type_env init final filter flocs =
        let env = A.LocMap.empty in
        let env = type_init init env in
        if debug then dump_type_env "INIT" env ;
        let env = type_init_values init env in
        if debug then dump_type_env "INIT VALUES" env ;
        let env = type_locations flocs env in
        if debug then dump_type_env "LOCS" env ;
        let env = type_final final env in
        if debug then dump_type_env "FINAL" env ;
        let env = match filter with
        | None -> env
        | Some f ->
            let env = type_prop f env in
            if debug then dump_type_env "FILTER" env ;
            env in
        env

      let find_type loc env =
        try A.LocMap.find loc env
        with Not_found -> Warn.fatal "no type for %s" (A.pp_location loc)

(* All observed locations *)
      let observed final locs =
        A.LocSet.union
          (C.locations final)
          (A.LocSet.of_list (List.map fst locs))

      let all_observed final filter locs =
        let obs = observed final locs in
        match filter with
        | None -> obs
        | Some f -> A.LocSet.union obs (C.locations_prop f)
    end

module Make
    (O:Config)
    (A:Arch_litmus.S)
    (T:Test_litmus.S with
module A.V = A.V and
type A.reg = A.reg and
type A.location = A.location and
module A.LocSet = A.LocSet and
module A.LocMap = A.LocMap and
type A.Out.t = A.Out.t and
type P.code = MiscParser.proc * A.pseudo list)
    (C:XXXCompile_litmus.S with module A = A) =
  struct
    open Printf
    open Constant

    let do_self = O.variant Variant_litmus.Self
    and do_precise = O.variant Variant_litmus.Precise
    let is_pte =
      let open Mode in
      match O.mode with
      | Std|PreSi -> false
      | Kvm -> true

    module G = Global_litmus
    module A = A
    module V = A.V
    module Constr = T.C
    module Generic = Generic(A)(Constr)
    open A.Out

    let rec do_extract_pseudo nop f ins = match ins with
    | A.Nop -> nop
    | A.Label (_,ins) -> do_extract_pseudo nop f ins
    | A.Instruction ins -> f ins
    | A.Symbolic _ (*no symbolic in litmus *)
    | A.Macro (_,_) -> assert false

    let extract_pseudo = do_extract_pseudo G.Set.empty C.extract_addrs

    let extract_addrs code =
      List.fold_right
        (fun ins env ->
          G.Set.union (extract_pseudo ins) env)
        code
        G.Set.empty

    let stable_regs code =
      List.fold_right
        (fun ins env ->
          A.RegSet.union
            (do_extract_pseudo A.RegSet.empty C.stable_regs ins) env)
        code
        A.RegSet.empty



(**********************************)
(* Label compilation as an offset *)
(**********************************)

    let find_offset prog p lbl =
      let is = List.assoc p prog in
      A.find_offset lbl is

(*******************************)
(* Assoc label -> small number *)
(*******************************)

    let rec lblmap_pseudo c m i = match i with
    | A.Nop|A.Instruction _ -> c,m
    | A.Label(lbl,i) ->
        let m = StringMap.add lbl c m in
        lblmap_pseudo (c+1) m i
    | A.Symbolic _ (*no symbolic in litmus *)
    | A.Macro _ -> assert false

    let lblmap_code =
      let rec do_rec c m = function
        | [] -> m
        | i::code ->
            let c,m = lblmap_pseudo c m i in
            do_rec c m code in
      do_rec 0 StringMap.empty

(*******************************)
(* Count specific instructions *)
(*******************************)

    let count_ins p code =
      List.fold_left
        (A.pseudo_fold (fun k i  -> if p i then k+1 else k))
        0 code

    let count_ret =
      if do_self then fun code -> count_ins C.is_ret code else fun _ -> 0

    let count_nop = count_ins C.is_nop

(****************)
(* Compile code *)
(****************)

    exception CannotIntern

    let tr_label_fail m lbl =  sprintf "%i" (StringMap.find lbl m)

    let tr_label m lbl =
      try
        tr_label_fail m lbl
      with
      | Not_found -> lbl

    let emit_label m lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label (tr_label m lbl)) ;
        label = Some lbl ; branch=[Next] ; }

    let as_int = function
      | Concrete i -> i
      | Symbolic _|Label _|Tag _|PteVal _ -> raise CannotIntern

    let compile_pseudo_code code k =
      let m =
        if O.numeric_labels then lblmap_code code
        else StringMap.empty in

      let tr_lab seen lbl =
        try
          let x = tr_label_fail m lbl in
          (if StringSet.mem lbl seen then sprintf "%sb" else sprintf "%sf") x
        with Not_found -> lbl in

      let rec compile_pseudo seen ins = match ins with
      | A.Nop -> seen,[]
      | A.Label (lbl,ins) ->
          let seen = StringSet.add lbl seen in
          let ilab = emit_label m lbl in
          let seen,k = compile_pseudo seen ins  in
          seen,ilab::k
      | A.Instruction ins ->
          seen,C.compile_ins (tr_lab seen) ins []
      | A.Symbolic _ (*no symbolic in litmus *)
      | A.Macro (_,_) -> assert false in

      let rec do_rec seen = function
        | [] -> k
        | ins::code ->
            let seen,ins = compile_pseudo seen ins in
            let k = do_rec seen code in
            ins @ k in
      do_rec StringSet.empty code

    let compile_code code =
      let code = compile_pseudo_code code [] in
      let code =
        if O.timeloop > 0 then C.emit_loop code
        else code in
      let code = match O.barrier with
      | Barrier.TimeBase -> (* C.emit_tb_wait *) code
      | _ -> code  in

      code


    module RegSet = A.RegSet

    let pp_reg_set chan rs =
      RegSet.pp chan ","
        (fun chan r -> fprintf chan "%s" (A.pp_reg r))
        rs

    module LabEnv =
      Map.Make
        (struct
          type t = string
          let compare = String.compare
        end)

(* Compute the set of registers that are to be inititialized
   by real code. This is standard live-in calculation *)

(* One instruction *)
    let live_in_ins ins (env,live_in_next) =
      let live_out =
        List.fold_left
          (fun k flow ->
            let rs =  match flow with
            | Next -> live_in_next
            | Any ->
                let rss =
                  LabEnv.fold
                    (fun _ rs k -> rs::k)
                    env [live_in_next;] in
                RegSet.unions rss
            | Branch lbl ->
                try LabEnv.find lbl env
                with Not_found -> RegSet.empty in
            RegSet.union rs k)
          RegSet.empty ins.branch in
      let live_in =
        RegSet.union
          (RegSet.of_list ins.inputs)
          (RegSet.diff live_out
(* Conditional instruction, a la ARM *)
             (if ins.cond then RegSet.empty
             else RegSet.of_list ins.outputs)) in
      (match ins.label with
      | None -> env
      | Some lbl -> LabEnv.add lbl live_in env),
      live_in

(* One sequence of instruction *)
    let live_in_code code env live_in_final =
      List.fold_right live_in_ins code (env,live_in_final)

    let debug = false
(* Fixpoint *)
    let comp_fix  code live_in_final =
      if debug then
        eprintf "FINAL: {%a}\n" pp_reg_set live_in_final ;
      let rec do_rec env0 =
        let env,r = live_in_code code env0 live_in_final in
        if debug then
          eprintf "FIX: {%a}\n" pp_reg_set r ;
        let c =
          LabEnv.compare RegSet.compare env env0 in
        if c = 0 then r
        else do_rec env in
      do_rec LabEnv.empty

    let comp_initset proc initenv code inputs_final =
      let reg_set  = comp_fix code inputs_final in
      let reg_set =
        if do_self || A.arch = `X86_64 then (* Bypass livein analysis for X64 arch *)
          RegSet.union
            reg_set
            (RegSet.of_list
               (List.fold_left
                  (fun k (r,_) -> match A.of_proc proc r with
                  | Some r -> r::k
                  | None -> k) [] initenv))
        else reg_set in
      RegSet.elements reg_set

    let compile_init proc initenv observed code  =
      let inputs_final = observed in
      let inputs = comp_initset proc initenv code inputs_final in
      List.map
        (fun reg ->
          let v = A.find_in_state (A.Location_reg (proc,reg)) initenv in
          reg,v)
        inputs

    let compile_final _proc observed = RegSet.elements observed

    let mk_templates ty_env name stable_info init code observed =
      let outs =
        List.map
          (fun (proc,code) ->
            let nrets = count_ret code in
            let nnops = count_nop code in
            let addrs = extract_addrs code in
            let stable = stable_regs code in
            let code = compile_code code in
            proc,addrs,stable,code,nrets,nnops)
          code in
      let pecs = outs in
      List.map
        (fun (proc,addrs,stable,code,nrets,nnops) ->
          let addrs,ptes =
            G.Set.fold
              (fun s (a,p) -> match s with
              | G.Addr s -> StringSet.add s a,p
              | G.Pte s -> a,StringSet.add s p
              | G.Phy _ -> assert false)
              addrs (StringSet.empty,StringSet.empty) in
          let all_clobbers =
            List.fold_left
              (fun k i -> match i.A.Out.clobbers with
              | [] -> k
              | _::_ as rs -> RegSet.of_list rs::k)
              [] code in
          let all_clobbers = RegSet.elements (RegSet.unions all_clobbers) in
          let observed_proc =
            A.LocSet.fold
              (fun loc k -> match A.of_proc proc loc with
              | Some r -> RegSet.add r k
              | _ -> k)
              observed RegSet.empty in
          let my_ty_env =
            A.LocMap.fold
              (fun loc t k -> match loc with
              | A.Location_reg (p,r) when Misc.int_eq p proc ->
                  (r,t)::k
              | _ -> k)
              ty_env [] in
          proc,
          let t =
            { init = compile_init proc init observed_proc code ;
              addrs = StringSet.elements addrs ;
              ptes = StringSet.elements ptes ;
              stable = [];
              final = compile_final proc observed_proc;
              all_clobbers;
              code = code; name=name; nrets; nnops;
              ty_env = my_ty_env;
            } in
          { t with stable = A.RegSet.elements (A.RegSet.inter (A.RegSet.union stable stable_info) (A.Out.all_regs t)) ; })
        pecs

    let _pp_env env =
      StringMap.pp_str
        (fun loc t ->
          sprintf "<%s,%s>" loc (CType.dump t))
        env

(* Compile globals, with their type *)
    let comp_globals env init code =
      let env =
(* First from typing env, this catches all globals listed in init,final,flocs *)
        A.LocMap.fold
          (fun loc t k -> match loc with
          | A.Location_global a -> G.Map.add a t k
          | _ -> k)
          env G.Map.empty in
(* Then extract types from code, notice that env types have precedence *)
      let env =
        List.fold_right
          (fun (_,t) ->
            List.fold_right
              (fun a env ->
                let a = G.Addr a in
                try
                  ignore (G.Map.find a env) ; env
                with Not_found ->
                  Generic.add_addr_type a base env)
              t.addrs)
          code env in
(* Add uninitialised globals referenced as values in init,
   Those may be accessed by code *)
      let env =
        List.fold_right
          (fun (_,(t,v)) env ->
            match t,v with
            | (MiscParser.TyDef|MiscParser.TyDefPointer),
              Constant.Symbolic s ->
                let a = G.tr_symbol s in
                begin try
                  let _ = G.Map.find a env in
                  env
                with Not_found  ->
                  G.Map.add a Generic.base env
                end
            | _ -> env)
          init env in
      G.Map.fold
        (fun a ty k -> match a with G.Addr a -> (a,ty)::k | G.Pte _| G.Phy _ -> k)
        env []

    let type_out env p t =
      List.map
        (fun reg -> reg,Generic.find_type (A.Location_reg (p,reg)) env)
        t.final

    let type_outs env code =
      List.map
        (fun (p,t) -> p,(t, (type_out env p t, [])))
        code

    let compile name t =
      let
          { MiscParser.init = init ;
            info = info;
            prog = code;
            condition = final;
            filter ;
            locations = locs ;
            extra_data ;_
          } = t in
      let initenv = List.map (fun (loc,(_,v)) -> loc,v) init in
      let observed = Generic.all_observed final filter locs in
      let ty_env1 = Generic.build_type_env init final filter locs
      and ty_env2 =
        try
          let ps = List.assoc MiscParser.align_key info in
          List.fold_left
            (fun m (x,i) ->
              StringMap.add x (CType.type_for_align i) m)
            StringMap.empty (InfoAlign.parse ps)
        with Not_found -> StringMap.empty in
      let ty_env = ty_env1,ty_env2 in
      let code = List.map (fun ((p,_),c) -> p,c) code in
      let code =
        if do_self || is_pte then
          let do_append_nop = is_pte && do_precise in
          List.map (fun (p,c) ->
            let nop = A.Instruction A.nop in
            let c = A.Instruction A.nop::c in
            let c =
              if do_append_nop then c@[nop] else c in
            p,c) code
        else code in
      let stable_info = match MiscParser.get_info  t MiscParser.stable_key with
      | None -> A.RegSet.empty
      | Some s ->
          let rs = Misc.split_comma s in
          let rs =
            List.fold_left
              (fun k r -> match A.parse_reg r with
              | None -> Warn.warn_always "'%s' i snot a register" r ; k
              | Some r -> r::k)
              [] rs in
          A.RegSet.of_list rs in
      let code = mk_templates ty_env1 name stable_info initenv code observed in
      let bellinfo =
        let open MiscParser in
        match extra_data with
        | NoExtra|CExtra _ -> None
        | BellExtra i -> Some i in
      let code_typed = type_outs ty_env1 code in
        { T.init = initenv ;
          info = info;
          code = code_typed;
          condition = final;
          filter = filter;
          globals = comp_globals ty_env1 init code;
          flocs = List.map fst locs ;
          global_code = [];
          src = t;
          type_env = ty_env;
          bellinfo;
        }

  end
