(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(V:Constant.S)(C:Arch_litmus.Config) =
  struct
    module A = MIPSArch_litmus.Make(C)(V)
    open A
    open A.Out
    open Printf

    let is_ret _ = assert false
(* No addresses in code *)
    let extract_addrs _ins = Global_litmus.Set.empty
    let stable_regs _ins = A.RegSet.empty

(************************)
(* Template compilation *)
(************************)

(* Arithmetic *)
    let op3regs op rD rA rB =
      let memo = A.pp_op op in
      { empty_ins with
        memo=memo^ " ^o0,^i0,^i1";
        inputs=[rA; rB];
        outputs=[rD]; }

    let op2regsI op rD rA i =
      let memo = A.pp_opi op in
      { empty_ins with
        memo= sprintf "%s ^o0,^i0,%i" memo i;
        inputs=[rA];
        outputs=[rD]; }

(* Moves *)
    let li r1 i =
      { empty_ins with
        memo = sprintf "li ^o0,%i" i ;
        inputs = [] ;
        outputs = [r1]; }

(* Memory *)
    let lw r1 k r2 =
      { empty_ins with
        memo = sprintf "lw ^o0,%i(^i0)" k ;
        inputs = [r2] ;
        outputs = [r1] ; }

    let ll r1 k r2 =
      { empty_ins with
        memo = sprintf "ll ^o0,%i(^i0)" k ;
        inputs = [r2] ;
        outputs = [r1] ; }

    let sw r1 k r2 =
      { empty_ins with
        memo = sprintf "sw ^i0,%i(^i1)" k ;
        inputs = [r1;r2;] ;
        outputs = []; }

    let sc r1 k r2 =
      { empty_ins with
        memo = sprintf "sc ^i0,%i(^i1)" k ;
        inputs = [r1;r2] ;
        outputs = [r1] ; }

    let b tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

    let bc tr_lab cond r1 r2 lbl =
      { empty_ins with
        memo = sprintf "b%s ^i0,^i1,%s"
          (A.pp_cond cond) (A.Out.dump_label (tr_lab lbl)) ;
        inputs=[r1;r2;];
        branch=[Next; Branch lbl] ; }

    let bcz tr_lab cond r1 lbl =
      { empty_ins with
        memo = sprintf "b%s ^i0,%s"
          (A.pp_condz cond) (A.Out.dump_label (tr_lab lbl)) ;
        inputs=[r1;];
        branch=[Next; Branch lbl] ; }

    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }

    let decr _r _i = assert false

    let no_tr lbl = lbl

    let emit_loop _k = assert false

    let compile_ins tr_lab ins k = match ins with
    | NOP -> { empty_ins with memo = "nop"; }::k
    | LI (r,i) -> li r i::k
    | OP (op,r1,r2,r3) -> op3regs op r1 r2 r3::k
    | OPI (op,r1,r2,i) -> op2regsI op r1 r2 i::k
    | B lbl ->b tr_lab lbl::k
    | BC (cond,r1,r2,lbl) -> bc tr_lab cond r1 r2 lbl::k
    | BCZ (cond,r1,lbl) -> bcz tr_lab cond r1 lbl::k
    | LW (r1,i,r2) -> lw r1 i r2::k
    | SW (r1,i,r2) -> sw r1 i r2::k
    | LL (r1,i,r2) -> ll r1 i r2::k
    | SC (r1,i,r2) -> sc r1 i r2::k
    | SYNC -> { empty_ins with memo="sync"; }::k

    let do_branch cond r i lab k = match i with
    | 0 ->
        bc no_tr cond r A.r0 lab::k
    | _ ->
        li A.tmp1 i::bc no_tr cond r A.tmp1 lab::k

    let branch_neq r i lab k = do_branch A.NE r i lab k

    let branch_eq r i lab k = do_branch A.EQ r i lab k

    let signaling_write _i _k = Warn.fatal "no signaling write for MIPS"

    let emit_tb_wait _ = Warn.fatal "no time base for MIPS"
  end
