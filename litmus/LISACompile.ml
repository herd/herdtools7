(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(V:Constant.S) =
  struct
    module A = LISAArch_litmus.Make(V)
    open A
    open A.Out
    open CType
    open Printf
    let is_ret _ = assert false

(***************************************************)
(* Extract explicit [symbolic] addresses from code *)
(***************************************************)
    let abs_to_string abs =  ParsedConstant.vToName abs

    let extract_ra = function
      | Rega _ -> StringSet.empty
      | Abs a -> StringSet.singleton (abs_to_string a)

    let extract_iar = function
      | IAR_roa ra -> extract_ra ra
      | IAR_imm _ -> StringSet.empty

    let extract_ao = function
      | Addr_op_atom ra
      | Addr_op_add (ra,_)
        -> extract_ra ra

    let extract_op = function
      | RAI iar -> extract_iar iar
      | OP (_,iar1,iar2)
          ->  StringSet.union (extract_iar iar1) (extract_iar iar2)

    let extract_addrs = function
      | Pld (_,ao,_)
      | Pst (ao,_,_)
        -> extract_ao ao
      | Prmw (_,op,ao,_) ->
          StringSet.union (extract_op op) (extract_ao ao)
      | Pmov  (_,op) -> extract_op op
      | Pnop|Pfence _|Pcall _|Pbranch _ -> StringSet.empty

(*****************************)
(* Compilation (to kernel C) *)
(*****************************)
    let compile_iar = function
      | IAR_imm i -> sprintf "%i" i,[]
      | IAR_roa (Rega r) -> reg_to_string r,[r]
      | IAR_roa (Abs (Constant.Symbolic ((s,None),0))) -> s,[]
      | IAR_roa
          (Abs Constant.(Symbolic _|Concrete _|Label _|Tag _))
        -> assert false

    let compile_roi = function
      | Imm i -> sprintf "%i" i,[]
      | Regi r -> reg_to_string r,[r;]

    let add_par s = "(" ^ s ^ ")"

    let type_vo = function
      | Some (Imm _) -> Compile.pointer
      | Some (Regi _)|None  -> voidstar

    let compile_addr_op vo = function
      | Addr_op_atom (Abs (Constant.Symbolic ((s,None),0))) -> s,[],[]
      | Addr_op_atom (Rega r) -> reg_to_string r,[r;],[r,type_vo vo]
      | Addr_op_add (Abs (Constant.Symbolic ((s,None),0)),roi) ->
          let m,i = compile_roi roi in
          add_par (s ^ "+" ^ m),i,[]
      | Addr_op_add (Rega r,roi) ->
          let m,i = compile_roi roi in
          add_par (reg_to_string r ^ "+" ^ m),r::i,[r,type_vo vo]
      | Constant.(Addr_op_atom (Abs (Concrete _|Label _|Tag _|Symbolic _))
      | Addr_op_add (Abs  (Concrete _|Label _|Tag _|Symbolic _),_))
        ->
          assert false

    let compile_ins tr_lab ins k = match ins with
    | Pld (r,a,["once"]) ->
        let m,i,tenv = compile_addr_op None a in
        { empty_ins with
          memo = sprintf "%s = READ_ONCE(*%s);" (reg_to_string r) m;
          inputs = i;reg_env=tenv;
          outputs = [r;] }::k
    | Pld (r,a,["acquire"]) ->
        let m,i,env = compile_addr_op None a in
        { empty_ins with
          memo = sprintf "%s = smp_load_acquire(%s);" (reg_to_string r) m;
          outputs = r::i; reg_env=env;}::k
    | Pld (r,a,["lderef"|"deref"]) ->
        let m,i,env = compile_addr_op None a in
        { empty_ins with
          memo = sprintf "%s = lockless_dereference(*%s);" (reg_to_string r) m;
          outputs = r::i; reg_env=env;}::k
    | Pst (a,roi,["once"]) ->
        let m_roi,i_roi = compile_roi roi
        and m_a,i_a,env = compile_addr_op (Some roi) a in
        { empty_ins with
          memo = sprintf "WRITE_ONCE(*%s,%s);" m_a m_roi;
          inputs = i_roi@i_a; reg_env=env;}::k
    | Pst (a,roi,["release"]) ->
        let m_roi,i_roi = compile_roi roi
        and m_a,i_a,env = compile_addr_op (Some roi) a in
        { empty_ins with
          memo = sprintf "smp_store_release(%s,%s);" m_a m_roi;
          inputs = i_roi@i_a; reg_env=env;}::k
    | Pst (a,(Regi _ as roi),["assign"]) ->
        let m_roi,i_roi = compile_roi roi
        and m_a,i_a,env = compile_addr_op None a in
        { empty_ins with
          memo = sprintf "rcu_assign_pointer(*%s,%s);" m_a m_roi;
          inputs = i_roi@i_a; reg_env=env;}::k
    | Pfence (Fence ([fence],None)) ->
        let fence = match fence with
        | "mb" -> "smp_mb"
        | "rmb" -> "smp_rmb"
        | "wmb" -> "smp_wmb"
        | "rb_dep" -> "smp_read_barrier_depends"
        | "rcu_read_lock" -> "rcu_read_lock"
        | "rcu_read_unlock" -> "rcu_read_unlock"
        | "sync" -> "synchronize_rcu_expedited"
        | _ -> Warn.fatal "bad fence: '%s'" fence in
        { empty_ins with memo = sprintf "%s();" fence; }::k
    | Pmov (r,RAI (IAR_imm i)) ->
        { empty_ins with
          memo = sprintf "%s = %i;" (reg_to_string r) i;
          outputs=[r;]; }::k
    | Pmov (r,RAI (IAR_roa (Rega r0))) ->
        { empty_ins with
          memo = sprintf "%s = %s;" (reg_to_string r) (reg_to_string r0);
          outputs=[r;]; inputs=[r0;]}::k
    | Pmov (r,RAI (IAR_roa (Abs (Constant.Symbolic ((x,None),0))))) ->
        { empty_ins with
          memo = sprintf "%s = %s;" (reg_to_string r) x;
          outputs=[r;] }::k
    | Pmov (r,OP (op,iar1,iar2)) ->
        let memo1,input1 = compile_iar iar1 in
        let memo2,input2 = compile_iar iar2 in
        let op = match op with
        | Add -> "+"
        | Xor -> "^^"
        | And -> "&"
        | Eq -> "=="
        | Neq -> "!=" in
        { empty_ins with
          memo = sprintf "%s = %s %s %s;"
            (reg_to_string r) memo1 op memo2;
          outputs = [r;]; inputs = input1@input2; }::k
    | Pbranch (None,lbl,_) ->
        { empty_ins with
          memo = sprintf "goto %s;" (A.Out.dump_label (tr_lab lbl));
          branch =[Branch lbl;] }::k
    | Pbranch (Some r,lbl,_) ->
        { empty_ins with
          memo = sprintf "if (%s) goto %s; barrier();"
            (reg_to_string r) (A.Out.dump_label (tr_lab lbl));
          inputs=[r;];
          branch =[Branch lbl;Next;] }::k
    | _ -> Warn.fatal "Cannot compile '%s'" (dump_instruction ins)


(********)
(* Vrac *)
(********)

    let stable_regs _ins = A.RegSet.empty

    let emit_loop _k =  Warn.fatal "no time loop for LISA"

    let signaling_write _i _k = Warn.fatal "no signaling write for LISA"

    let emit_tb_wait _ = Warn.fatal "no time base for LISA"
  end
