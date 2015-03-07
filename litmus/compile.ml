(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val numeric_labels : bool
  val timeloop : int
  val barrier : Barrier.t
end

let get_fmt hexa base = match CType.get_fmt hexa base with
| Some fmt -> fmt
| None -> Warn.fatal "No format for type '%s'" base

let base =  CType.Base "int"
let pointer = CType.Pointer base

module Generic (A : Arch.Base) (C:Constr.S with module A = A) = struct
  open CType

  let base =  base
  let pointer = pointer

  let typeof = function
    | Constant.Concrete _ -> base
    | Constant.Symbolic _ -> pointer

  let misc_to_c  = function
    | MiscParser.TyDef -> base
    | MiscParser.TyDefPointer  -> pointer
    | MiscParser.Ty t -> Base t
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
                | (Base _ as t, Some (Base _)) ->
                    Some t (* location takes precedence *)
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
        let tz = StringMap.find a env in
        match ty,tz with
        | (Pointer (Base s1), Pointer (Base s2))
        | (Base s1, Base s2) when Misc.string_eq s1 s2 -> env
        | _,_ (* (Pointer _|Base _),(Pointer _|Base _) *) ->
            Warn.fatal
              "Type mismatch detected on location %s, required %s vs. found %s"
              a (dump ty) (dump tz)
      with
        Not_found -> StringMap.add a ty env

    let add_value v env = match v with
    | Constant.Concrete _ -> env
    | Constant.Symbolic a -> add_addr_type a base env

(********************)
(* Complete typing  *)
(********************)

(* final, only default types *)
    let type_final final env =
      let type_atom a env = match a with
      | ConstrGen.LV (loc,v) ->
          A.LocMap.add loc (typeof v) env
      | ConstrGen.LL _ -> env in
      ConstrGen.fold_constr type_atom final env

(* locations, default and explicit types *)
  let type_locations flocs env =
    List.fold_left
      (fun env (loc,t) -> A.LocMap.add loc (misc_to_c t) env)
      env flocs

(* init, default and explicit types *)
  open Printf

  let type_init init env =
    List.fold_left
      (fun env (loc,(t,v)) ->
(*
        Printf.eprintf "add %s -> %s\n"
          (A.pp_location loc)
          (MiscParser.pp_run_type t) ;
*)
        match t with
        | MiscParser.TyDef ->
          begin try
            ignore (A.LocMap.find loc env) ;
            env
          with Not_found ->
            A.LocMap.add loc (typeof v) env
          end
        | _ -> A.LocMap.add loc (misc_to_c t) env)
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


  let build_type_env init final flocs =
    let env = type_final final A.LocMap.empty in
    if false then dump_type_env "FINAL" env ;
    let env = type_locations flocs env in
    if false then dump_type_env "LOCS" env ;
    let env = type_init init env in
    if false then dump_type_env "INIT" env ;
    env

  let find_type loc env =
    try A.LocMap.find loc env
    with Not_found -> assert false

(* All observed locations *)
  let observed final locs =
    A.LocSet.union
      (C.locations final)
      (A.LocSet.of_list (List.map fst locs))

end

module Make
    (O:Config)
    (A:Arch.S)
    (T:Test.S with
     type A.reg = A.reg and
     type A.location = A.location and
     module A.LocSet = A.LocSet and
     module A.LocMap = A.LocMap and
     type A.Out.t = A.Out.t and
     type P.code = int * A.pseudo list)
    (C:XXXCompile.S with module A = A) =
  struct
    open Printf
    open Constant

    module A = A
    module V = A.V
    module Constr = T.C
    module Generic = Generic(A)(Constr)
    open A.Out

    let rec do_extract_pseudo nop f ins = match ins with
    | A.Nop -> nop
    | A.Label (_,ins) -> do_extract_pseudo nop f ins
    | A.Instruction ins -> f ins
    | A.Macro (_,_) -> assert false

    let extract_pseudo = do_extract_pseudo StringSet.empty C.extract_addrs

    let extract_addrs code =
      List.fold_right
        (fun ins env ->
          StringSet.union (extract_pseudo ins) env)
        code
        StringSet.empty

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



    let rec lblmap_pseudo c m i = match i with
    | A.Nop|A.Instruction _ -> c,m
    | A.Label(lbl,i) ->
        let m = StringMap.add lbl c m in
        lblmap_pseudo (c+1) m i
    | A.Macro _ -> assert false

    let lblmap_code =
      let rec do_rec c m = function
        | [] -> m
        | i::code ->
            let c,m = lblmap_pseudo c m i in
            do_rec c m code in
      do_rec 0 StringMap.empty



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
      | Symbolic _ -> raise CannotIntern


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


    module RegSet =
      MySet.Make
        (struct
          type t = A.reg
          let compare = A.reg_compare
        end)

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

(* Fixpoint *)
    let comp_fix code live_in_final =
(*
  eprintf "FINAL: {%a}\n" pp_reg_set live_in_final ;
 *)
      let rec do_rec env0 =
        let env,r = live_in_code code env0 live_in_final in
(*
  eprintf "FIX: {%a}\n" pp_reg_set r ;
 *)
        let c =
          LabEnv.compare RegSet.compare env env0 in
        if c = 0 then r
        else do_rec env in
      do_rec LabEnv.empty

    let comp_initset code inputs_final =
      RegSet.elements (comp_fix code inputs_final)

    let compile_init proc initenv observed code  =
      let inputs_final = observed in
      let inputs = comp_initset code inputs_final in
      List.map
        (fun reg ->
          let v = A.find_in_state (A.Location_reg (proc,reg)) initenv in
          reg,v)
        inputs

    let compile_final proc observed = RegSet.elements observed

    let mk_templates init code observed =
      let outs =
        List.map
          (fun (proc,code) ->
            let addrs = extract_addrs code in
            let stable = stable_regs code in
            let code = compile_code code in
            proc,addrs,stable,code)
          code in
      let pecs = outs in
      List.map
        (fun (proc,addrs,stable,code) ->
          let observed_proc = 
            A.LocSet.fold
              (fun loc k -> match A.of_proc proc loc with
              | Some r -> RegSet.add r k
              | _ -> k)
              observed RegSet.empty in
          proc,
          { init = compile_init proc init observed_proc code ;
            addrs = StringSet.elements addrs ;
            stable = A.RegSet.elements stable;
            final = compile_final proc observed_proc;
            code = code; })
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
          | A.Location_global a -> StringMap.add a t k
          | _ -> k)
          env StringMap.empty in
(* Then extract types from code, notice that env types have precedence *)
      let env =
        List.fold_right
          (fun (_,t) ->
            List.fold_right
              (fun a env ->
                try
                  ignore (StringMap.find a env) ; env
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
            | MiscParser.TyDef,Constant.Symbolic a ->
                begin try
                  let _ = StringMap.find a env in
                  env
                with Not_found  ->
                  StringMap.add a Generic.base env
                end
            | _ -> env)
          init env in
      StringMap.fold
        (fun a ty k -> (a,ty)::k)
        env []

    let type_out env p t =
      List.map
        (fun reg -> reg,Generic.find_type (A.Location_reg (p,reg)) env)
        t.final

    let type_outs env code =
      List.map
        (fun (p,t) -> p,(t, (type_out env p t, [])))
        code
(*
  let pp_out (p,(_,(env,_))) =
  let pp =
  List.map
  (fun (reg,t) -> sprintf "<%i:%s,%s>"
  p (A.pp_reg reg) (CType.dump t))
  env in
  String.concat " " pp

  let pp_outs outs =
  let pp = List.map pp_out outs in
  String.concat " " pp
 *)
    let compile t =
      let
          { MiscParser.init = init ;
            info = info;
            prog = code;
            condition = final;
            locations = locs ; _
          } = t in
      let initenv = List.map (fun (loc,(_,v)) -> loc,v) init in
      let observed = Generic.observed final locs in
      let ty_env = Generic.build_type_env init final locs in
      let code = mk_templates initenv code observed in
      let code_typed = type_outs ty_env code in
      { T.init = initenv ;
        info = info;
        code = code_typed;
        condition = final;
        globals = comp_globals ty_env init code;
        flocs = List.map fst locs ;
        global_code = [];
        src = t;
      }

  end
