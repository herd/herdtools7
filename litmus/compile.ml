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
  val precision : Fault.Handling.t
  val variant : Variant_litmus.t -> bool
  val driver : Driver.t
end

module Default = struct
  let numeric_labels = false
  let timeloop = 0
  let barrier = Barrier.UserFence
  let mode = Mode.Std
  let precision = Fault.Handling.default
  let variant _ = false
  let driver = Driver.Shell
end

let get_fmt hexa base = match CType.get_fmt hexa base with
| Some fmt -> fmt
| None -> Warn.fatal "No format for type '%s'" base

let base =  CType.Base "int"
let pointer = CType.Pointer base

module Generic
    (A : Arch_litmus.Base)
    (C:Constr.S
    with type location = A.location and module LocSet = A.LocSet) = struct

      module G = Global_litmus
      open CType

      let base =  A.base_type
      let pointer = CType.Pointer base
      let code_pointer = Pointer (CType.ins_t)
      let tag = Base "tag_t"
      let base_array sz = CType.Array ("int", sz)
      let pteval_t = CType.pteval_t
      let parel1_t = CType.parel1_t
      let ins_t = CType.ins_t

      let typeof = function
        | Constant.Concrete _ -> base
        | Constant.ConcreteVector vs -> base_array (List.length vs)
        | Constant.Symbolic _ as symb when Constant.is_label symb -> code_pointer
        | Constant.Symbolic _ -> pointer
        | Constant.Tag _ -> tag
        | Constant.PteVal _ -> pteval_t
        | Constant.AddrReg _ -> parel1_t
        | Constant.Instruction _ -> ins_t
        | Constant.Frozen _ | Constant.ConcreteRecord _ -> assert false

      let misc_to_c loc = function
        | TestType.TyDef when A.is_pte_loc loc -> pteval_t
        | TestType.TyDef ->
           begin match loc with
           | A.Location_reg (_,r) -> A.type_reg r
           | A.Location_global _  -> base
           end
        | TestType.TyDefPointer  -> pointer
        | TestType.Ty t -> Base t
        | TestType.Atomic t -> Atomic (Base t)
        | TestType.Pointer t -> Pointer (Base t)
        | TestType.TyArray (t,sz) -> Array (t,sz)

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
               | LV (Loc (A.Location_reg (q,r)),v)
                     when p=q && A.reg_compare reg r = 0 ->
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
                    | TestType.TyDef -> None
                    | TestType.TyDefPointer -> Some pointer
                    | TestType.Ty s -> Some (Base s)
                    | TestType.Atomic s -> Some (Atomic (Base s))
                    | TestType.Pointer s -> Some (Pointer (Base s))
                    | TestType.TyArray _ -> assert false (* No array register *)
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
        | LV (Loc loc,v) ->
            begin try
              ignore (A.LocMap.find loc env) ;
              env
            with Not_found ->  A.LocMap.add loc (typeof v) env end
        | LV (Deref _,_)
        | LL _|FF _ -> env

      let type_final final env =
        ConstrGen.fold_constr type_atom_final final env

      let type_prop prop env = ConstrGen.fold_prop type_atom_final prop env

(* locations, default and explicit types *)
      let type_locations flocs env =
        let open LocationsItem in
        List.fold_left
          (fun env i -> match i with
          | Loc (ConstrGen.Loc loc,t) ->
              begin try
                ignore (A.LocMap.find loc env) ; env
              with
              | Not_found ->
                  A.LocMap.add loc (misc_to_c loc t) env
              end
          | Loc (ConstrGen.Deref _,_)
          | Fault _ -> env)
          env flocs

(* init, default and explicit types *)
      open Printf

      let type_init init env =
        List.fold_left
          (fun env (loc,(t,v)) -> match t with
          | TestType.TyDef ->
              begin try
                ignore (A.LocMap.find loc env) ;
                env
              with Not_found ->
                A.LocMap.add loc (typeof v) env
              end
          | _ -> A.LocMap.add loc (misc_to_c loc t) env)
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
                let open TestType in
                let tv = match t with
                | TyDefPointer|TyDef -> TyDef
                | Pointer s -> Ty s
                | _ ->
                    Warn.user_error
                      "variable %s should be of pointer type"
                      (A.pp_location loc) in
                A.LocMap.add a (misc_to_c a tv) env
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
      let observed_in_rloc =
        let open ConstrGen in
        function
        | Loc loc,_ -> loc
        | Deref _,_ -> prerr_endline "TODO" ; assert false

      let observed final locs =
        A.LocSet.union
          (C.locations final)
          (LocationsItem.fold_locs A.LocSet.add locs A.LocSet.empty)

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
type instruction = A.instruction and
module A.V = A.V and
type A.reg = A.reg and
type A.location = A.location and
module A.LocSet = A.LocSet and
module A.LocMap = A.LocMap and
type A.Out.t = A.Out.t and
type P.code = MiscParser.proc * A.pseudo list and
module A.FaultType = A.FaultType)
    (C:XXXCompile_litmus.S with module A = A) =
  struct
    open Printf
    open Constant

    let do_self = O.variant Variant_litmus.Self
    and do_precise = Fault.Handling.is_fatal O.precision
    let is_pte =
      let open Mode in
      match O.mode with
      | Std|PreSi -> false
      | Kvm -> true

    module G = Global_litmus
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
    | A.Pagealign -> assert false
    | A.Skip _ -> assert false (* used internally in herd7 only *)

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

(*******************************)
(* Assoc label -> small number *)
(*******************************)

(* All labels *)
    let rec ins_labels k i  = match i with
      | A.Nop
      | A.Instruction _
      | A.Symbolic _
      | A.Macro _
      | A.Pagealign -> k
      | A.Skip _ -> k
      | A.Label (lbl,i) ->
          ins_labels (lbl::k) i

    let code_labels (p,_,c) k =
      let lbls =  List.fold_left ins_labels [] c in
      List.fold_left
        (fun k lbl -> Label.Full.Set.add (p,lbl) k)
        k lbls

    let all_labels prog =
      List.fold_right code_labels prog Label.Full.Set.empty

(* All target labels *)

    let ins_target = A.fold_labels (fun k lbl -> lbl::k)

    let code_targets (p,_,c) k =
      let lbls = List.fold_left ins_target [] c in
      List.fold_left
        (fun k lbl -> Label.Full.Set.add (p,lbl) k)
        k lbls

    let init_targets init =
      List.fold_left
        (fun k (_,v) ->
          match v with
          | Symbolic (Virtual {Constant.name=Symbol.Label (_,lbl); _}) ->
              Label.Set.add lbl k
          |Concrete _|ConcreteVector _|ConcreteRecord _
          |Symbolic _|Tag _|PteVal _|AddrReg _
          |Instruction _|Frozen _
           -> k)
        Label.Set.empty init

    let prog_targets prog =
      List.fold_right code_targets prog Label.Full.Set.empty

    let pp_full = Label.Full.Set.pp_str "," Label.Full.pp

(* Translate labls to integers (local labels), when possible *)
    let rec lblmap_pseudo cm i = match i with
    | A.Nop|A.Instruction _|A.Pagealign|A.Skip _ -> cm
    | A.Label(lbl,i) ->
       let cm  =
         let c,m = cm in
         c+1,Label.Map.add lbl c m in
       lblmap_pseudo cm i
    | A.Symbolic _ (*no symbolic in litmus *)
    | A.Macro _ -> assert false

    let first_label = if is_pte then C.max_handler_label else 0

    let lblmap_code =
      let rec do_rec cm = function
        | [] -> cm
        | i::code ->
            let cm = lblmap_pseudo cm i in
            do_rec cm code in
      fun code co ->
        let (_,m) as cm = do_rec (first_label,Label.Map.empty) code in
        match co with
        | None -> m
        | Some code ->
           let _,m = do_rec cm code in
           m

(*******************************)
(* Count specific instructions *)
(*******************************)

    let count_ins p code =
      List.fold_left
        (A.pseudo_fold (fun k i  -> if p i then k+1 else k))
        0 code

    let count_ret =
      if do_self then fun code -> count_ins C.is_ret code else fun _ -> 0

    let count_nop = count_ins A.is_nop

(****************)
(* Compile code *)
(****************)

    exception CannotIntern

    let tr_label_fail m lbl =  sprintf "%i" (Label.Map.find lbl m)

    let tr_label m lbl =
      try
        tr_label_fail m lbl
      with
      | Not_found -> lbl

    let emit_label m lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label (tr_label m lbl)) ;
        label = Some lbl ; branch=[Next] ; }

    let compile_pseudo_code code fhandler =
      let m =
        if O.numeric_labels then lblmap_code code fhandler
        else Label.Map.empty in

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
      | A.Pagealign | A.Skip _
      | A.Macro (_,_) -> assert false in

      let rec do_rec seen = function
        | [] -> seen,[]
        | ins::code ->
            let seen,ins = compile_pseudo seen ins in
            let seen,k = do_rec seen code in
            seen,ins @ k in
      let seen,code = do_rec StringSet.empty code in
      match fhandler with
      | None -> code,[]
      | Some fhandler ->
         let _,fhandler = do_rec seen fhandler in
         code,fhandler

    let rec pp_pseudo ins k =
      match ins with
      | A.Nop -> k
      | A.Label (lbl,ins) ->
         (Label.pp lbl ^ ":")
         ::pp_pseudo ins k
      | A.Instruction ins ->
         A.dump_instruction ins::k
      | A.Macro _|A.Symbolic _
        -> assert false
      | A.Pagealign| A.Skip _ -> assert false (* support for .pagealign not implemented yet*)

    let pp_code code =
      let k = List.fold_right pp_pseudo code [] in
      String.concat "; " k

    let ret_as_branch proc code =
      if List.exists (A.pseudo_exists C.is_ret) code then
        let lab = Label.return proc in
        List.fold_right
          (fun i k ->
            (A.pseudo_map
               (fun i ->
                 if C.is_ret i then C.branch lab
                 else i)) i::k)
          code [A.Label (lab,A.Nop)]
      else code

    let compile_code proc user code fhandler =
      (* In telechat mode, return instruction is interpreted as "jump to end of code" *)
      let code =
        if O.variant Variant_litmus.Telechat then
          ret_as_branch proc code
        else code in

      let fhandler =
        match fhandler with
        | [] -> None
        | _::_ -> Some fhandler in
      let has_handler = Misc.is_some fhandler in
      let code,fhandler_c = compile_pseudo_code code fhandler in
      let code =
        if O.timeloop > 0 then C.emit_loop code
        else code in
      let code =
        if user then
          C.user_mode has_handler proc
          @code
          @C.kernel_mode has_handler
        else code
      and fhandler =
        match fhandler with
        | None -> fhandler_c
        | Some _ ->
           let asmhandler =
             let open Driver in
             match O.driver with
             | C|Shell -> proc
             | XCode ->
             Warn.user_error "No custom handler for XCode" in
           C.fault_handler_prologue user asmhandler
           @fhandler_c@C.fault_handler_epilogue user fhandler_c in
      code,fhandler


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
    let int_label k = Printf.sprintf "_%d" k

    let live_in_ins i ins (env,live_in_next) =
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
            | Disp j ->
               let lbl = int_label (i+j) in
               begin
                 try LabEnv.find lbl env
                 with Not_found -> assert false
               end
            | Branch lbl ->
               begin
                 try LabEnv.find lbl env
                 with Not_found -> RegSet.empty
               end in
            RegSet.union rs k)
          RegSet.empty ins.branch in
      let live_in =
        RegSet.union
          (RegSet.of_list ins.inputs)
          (RegSet.diff live_out
(* Conditional instruction, a la ARM *)
             (if ins.cond then RegSet.empty
             else RegSet.of_list ins.outputs)) in
      let env =
        let lbl = int_label i in
        if LabEnv.mem lbl env then LabEnv.add lbl live_in env
        else env in
      (match ins.label with
      | None -> env
      | Some lbl -> LabEnv.add lbl live_in env),
      live_in

(* One sequence of instruction *)
    let live_in_code t env live_in_final =
      let rec do_rec k =
        if k >= Array.length t then (env,live_in_final)
        else live_in_ins k t.(k) (do_rec (k+1)) in
      do_rec 0

    let debug = false
(* Fixpoint *)
    let comp_fix  code live_in_final =
      let t = Array.of_list code in
      if debug then
        eprintf "FINAL: {%a}\n" pp_reg_set live_in_final ;
      let rec do_rec env0 =
        let env,r = live_in_code t env0 live_in_final in
        if debug then
          eprintf "FIX: {%a}\n" pp_reg_set r ;
        let c =
          LabEnv.compare RegSet.compare env env0 in
        if c = 0 then r
        else do_rec env in
      let env0 =
        let rec do_rec k env =
          if k >= Array.length t then env
          else
            let env =
              List.fold_left
                (fun env -> function
                  | Disp i ->LabEnv.add (int_label (k+i)) RegSet.empty env
                  | Next|Branch _|Any -> env)
                env t.(k).branch in
            do_rec (k+1) env in
        do_rec 0
          (LabEnv.add
             (int_label (Array.length t)) live_in_final LabEnv.empty) in
      do_rec env0

    let comp_initset proc initenv code inputs_final =
      let reg_set  = comp_fix code inputs_final in
      let reg_set =
        (* Bypass livein analysis for X64 arch, kvm and self mode as register must
           be initialized in all those situations, due to partial writes,
           code modification and faults *)
        if do_self || is_pte || A.arch = `X86_64 then
          RegSet.union
            (if is_pte then RegSet.union inputs_final reg_set else reg_set)
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

    let mk_templates procs_user ty_env name stable_info init prog observed =
      let outs =
        List.map
          (fun (proc,(code,fhandler)) ->
            let is_user = ProcsUser.is procs_user proc in
            let nrets = count_ret code in
            (* For user mode one nop added at the assembly level *)
            let nnops = count_nop code + (if is_user then 1 else 0) in
            let addrs =  G.Set.union (extract_addrs code) (extract_addrs fhandler) in
            let stable = stable_regs code in
            let code,fhandler =  compile_code proc is_user code fhandler in
            proc,addrs,stable,code,fhandler,nrets,nnops)
          prog in
      List.map
        (fun (proc,addrs,stable,code,fhandler,nrets,nnops) ->
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
          let ty_env =
            A.LocMap.fold
              (fun loc t k -> match loc with
              | A.Location_reg (p,r) when Misc.int_eq p proc ->
                  RegMap.add r t k
              | _ -> k)
              ty_env RegMap.empty in
          let init = compile_init proc init observed_proc code in
          let final = compile_final proc observed_proc in
          let addrs = StringSet.elements addrs in
          let stable =
               A.RegSet.inter
                 (A.RegSet.union stable stable_info)
                 (A.Out.all_regs code fhandler final) in
          let stable = A.RegSet.elements stable in
          let t =
          { init ;
            addrs ;
            ptes = StringSet.elements ptes ;
            stable;
            final;
            all_clobbers;
            code; fhandler; name; nrets; nnops;
            ty_env;
            code_ty_env = RegMap.empty;
          } in
          let code_ty_env = A.Out.get_reg_env A.error A.warn t in
          let t = { t with code_ty_env;} in
        proc,t) outs

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
            | (TestType.TyDef|TestType.TyDefPointer),
              Constant.Symbolic s when not (Constant.is_label v) ->
                let a = G.get_base_symbol s in
                begin try
                  let _ = G.Map.find a env in
                  env
                  with Not_found  ->
                    let t = match t with
                    | TestType.TyDef -> Generic.base
                    | TestType.TyDefPointer -> Generic.pointer
                    | _ -> assert false in
                  G.Map.add a t env
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
      let procs_user = ProcsUser.get info in
      if Misc.consp procs_user && do_self && is_pte then
        Warn.user_error "litmus7 cannot handle -variant self -mode kvm when there are processes in userspace" ;
      let initenv = List.map (fun (loc,(_,v)) -> loc,v) init in
      let observed = Generic.all_observed final filter locs in
      let ty_env1 = Generic.build_type_env init final filter locs in
      let ty_env2 =
        try
          let ps = List.assoc MiscParser.align_key info in
          List.fold_left
            (fun m (x,i) ->
              let loc = A.Location_global (Global_litmus.Addr x) in
              let t = A.LocMap.find loc ty_env1 in
              let () =
                match CType.sizeof t with
                | Some n ->
                   if i < n then
                     Warn.fatal
                       "Location %s cannot be aligned to less than its size" x
                | None ->
                   Warn.warn_always
                     "Beware, cannot compute size of aligned location %s"
                     x in
              StringMap.add x (CType.type_for_align i) m)
            StringMap.empty (InfoAlign.parse ps)
        with Not_found -> StringMap.empty in
      let ty_env = ty_env1,ty_env2 in
      let label_init = T.get_exported_labels_init_code initenv code in
      let prog = A.code_by_proc code in
      let prog =
        if do_self || is_pte || not (Label.Full.Set.is_empty label_init) then
          let do_append_nop = is_pte && do_precise in
          List.map
            (fun (p,(c,f)) ->
              (* Add nop to signal code start *)
              let is_user = ProcsUser.is procs_user p in
              let nop =
                match A.nop with
                | None ->
                    Warn.fatal
                      "Architecture %s has no NOP instruction, compilation is impossible"
                      (Archs.pp A.arch)
                | Some nop ->
                    A.Instruction nop in
              (* Except in user mode, where it will be added later *)
              let c = if not is_user then nop::c else c in
              let c = (* Append nop for faukt handler to return at end of code *)
                if do_append_nop then c@[nop] else c in
              p,(c,f)) prog
        else prog in
      let stable_info = match MiscParser.get_info  t MiscParser.stable_key with
      | None -> A.RegSet.empty
      | Some s ->
          let rs = Misc.split_comma s in
          let rs =
            List.fold_left
              (fun k r -> match A.parse_reg r with
              | None -> Warn.warn_always "'%s' is not a register" r ; k
              | Some r -> r::k)
              [] rs in
          A.RegSet.of_list rs in
      let code =
        mk_templates
          procs_user ty_env1 name stable_info initenv prog observed in
      let bellinfo =
        let open MiscParser in
        match extra_data with
        | [BellExtra i] -> Some i
        | _ -> None in
      let code_typed = type_outs ty_env1 code in
      let flocs,ffaults = LocationsItem.locs_and_faults locs in
        { T.init = initenv ;
          info = info;
          code = code_typed;
          condition = final;
          filter = filter;
          globals = comp_globals ty_env1 init code;
          flocs; ffaults;
          global_code = [];
          src = t;
          type_env = ty_env;
          bellinfo;
        }

  end
