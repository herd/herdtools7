(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
include Arch.MakeArch(struct
  open AArch64Base

  module A = struct
    include AArch64Base
    include MakePP(struct let is_morello = false end)
  end

  include Arch.MakeCommon(A)

  let match_k k k' subs =
    let open MetaConst in
    match k,k' with
    | Meta k,k' -> add_subs [Cst (k,k');] subs
    | Int k, k' when k=k' -> Some subs
    | _,_ -> None

  module Ext = struct
    open AArch64Base.Ext

    let match_sext e e' subs =
      match e,e' with
      | UXTB,UXTB
      | UXTH,UXTH
      | UXTW,UXTW
      | UXTX,UXTX
      | SXTB,SXTB
      | SXTH,SXTH
      | SXTW,SXTW
      | SXTX,SXTX
        -> Some subs
      | _,_
        -> None

    let match_ext e e' subs =
      match e,e' with
      | (se,None),(se',None) -> match_sext se se' subs
      | (se,Some k),(se',Some k') ->
         match_sext se se' subs >>> match_k k k'
      | _ -> None
  end

  let match_mode m m' subs =
    let open AArch64Base in
    match m,m' with
    | (Idx,Idx)
      | (PreIdx,PreIdx)
      | (PostIdx,PostIdx)
      -> Some subs
    | _,_ -> None

  let match_idx (k,m) (k',m')  subs =
    match_mode m m' subs >>> match_k k k'

  module MemExt = struct

    module E = AArch64Base.MemExt


    let match_rext e e' subs =
      let open E in
      match e,e' with
      | (UXTW,UXTW)
      | (LSL,LSL)
      | (SXTW,SXTW)
      | (SXTX,SXTX)
        -> Some subs
      | _,_ -> None

    let match_ext e e' subs =
      match e,e' with
      | E.Imm idx,E.Imm idx' ->
         match_idx idx idx' subs
      | E.Reg (_,r,e,k),E.Reg (_,r',e',k')
        ->
         match_rext e e' subs
         >>> match_k k k'
         >>> add_subs [Reg(sr_name r,r');]
      | _,_ -> None

  end

  module OpExt = struct

    let match_shift s s' subs =
      let open OpExt in
      match s,s' with
      | (LSL k,LSL k')
      | (LSR k,LSR k')
      | (ASR k,ASR k')
      | (ROR k,ROR k')
        -> match_k k k' subs
      | _ -> None

    let match_ext e e' subs =
      match e,e' with
      | OpExt.Imm (k1,k2),OpExt.Imm (k1',k2') ->
         match_k k1 k1' subs >>> match_k k2 k2'
      | OpExt.Reg (r,s),OpExt.Reg (r',s') ->
         match_shift s s' subs >>> add_subs [Reg(sr_name r,r')]
      | _,_ -> None
  end

  let match_kr subs kr kr' =  match kr,kr' with
    | K k,K k' ->  match_k k k' subs
    | RV(_,r),RV(_,r') -> add_subs [Reg(sr_name r,r')] subs
    | _ -> None

  let match_lbl lp li subs =
    let open BranchTarget in
    match lp,li with
    | Lbl lp,Lbl li -> add_subs [Lab(lp,li)] subs
    | Offset ip,Offset ii
         when Misc.int_eq ip ii -> Some subs
    | _,_ -> None

  let match_instr subs pattern instr = match pattern,instr with
    | I_NOP,I_NOP -> Some subs
    | I_FENCE fp,I_FENCE fi when fp = fi
                            -> Some subs

    | I_B lp, I_B li
      -> match_lbl lp li subs

    | I_BC(cp,lp), I_BC(ci,li) when cp = ci
      -> match_lbl lp li subs

    | I_CBZ(_,r,lp),I_CBZ(_,r',li)
    | I_CBNZ(_,r,lp),I_CBNZ(_,r',li)
      ->
       match_lbl lp li subs >>> add_subs [Reg(sr_name r,r')]

    | I_MOV(_,r,kr),I_MOV(_,r',kr') ->
        match_kr subs kr kr' >>> add_subs [Reg(sr_name r,r');]


    | I_LDAR(_,tp,r1,r2),I_LDAR(_,ti,r1',r2') when tp = ti
     -> add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs

    | I_STLR(_,r1,r2),I_STLR(_,r1',r2')
    | I_SXTW(r1,r2),I_SXTW(r1',r2')
      ->
        add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs

    | I_STXR(_,tp,r1,r2,r3),I_STXR(_,ti,r1',r2',r3') when tp = ti
      ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2'); Reg(sr_name r3,r3')]
          subs
    | I_UBFM(_,r1,r2,k1,k2),I_UBFM(_,r1',r2',k1',k2')
    | I_SBFM(_,r1,r2,k1,k2),I_SBFM(_,r1',r2',k1',k2')
      ->
        begin match (match_kr subs (K k1) (K k1'),
        match_kr subs (K k2) (K k2')) with
        | Some(x),Some(_) -> Some(x)
        | _ -> None end  >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | I_LDUR(_,r1,r2,k),I_LDUR(_,r1',r2',k')
      ->
        match_kr subs (K k) (K k') >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | I_LDRSW (r1,r2,e),I_LDRSW (r1',r2',e')
    | I_LDRS((_,B),r1,r2,e),I_LDRS((_,B),r1',r2',e')
    | I_LDRS((_,H),r1,r2,e),I_LDRS((_,H),r1',r2',e')
    | I_LDR(_,r1,r2,e),I_LDR(_,r1',r2',e')
    | I_STR(_,r1,r2,e),I_STR(_,r1',r2',e')
      ->
       MemExt.match_ext e e' subs >>>
       add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | I_STRBH(_,r1,r2,e),I_STRBH(_,r1',r2',e')
    | I_LDRBH(_,r1,r2,e),I_LDRBH(_,r1',r2',e')
      ->
        MemExt.match_ext e e' subs >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]

    | I_LDP (t,a,r1,r2,r3,idx),I_LDP (t',a',r1',r2',r3',idx')
    | I_STP (t,a,r1,r2,r3,idx),I_STP (t',a',r1',r2',r3',idx')
         when t=t' && a=a'
      ->
       match_idx idx idx' subs >>>
       add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2'); Reg (sr_name r3,r3')]
    | I_LDPSW (r1,r2,r3,idx),I_LDPSW (r1',r2',r3',idx')
      ->
       match_idx idx idx' subs >>>
       add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2'); Reg (sr_name r3,r3')]
    | I_STG (r1,r2,idx),I_STG (r1',r2',idx')
    | I_STZG (r1,r2,idx),I_STZG (r1',r2',idx')
      ->
       match_idx idx idx' subs >>>
       add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]

    | I_ADDSUBEXT(_,op,r1,r2,(_,r3),ext),
      I_ADDSUBEXT(_,op',r1',r2',(_,r3'),ext')
         when op = op'
      ->
       Ext.match_ext ext ext' subs >>>
       add_subs
           [Reg(sr_name r1,r1'); Reg(sr_name r2,r2');
            Reg (sr_name r3,r3');]

    | I_OP3(_,opp,r1,r2,ext),I_OP3(_,opi,r1',r2',ext') when opp=opi
      ->
        OpExt.match_ext ext ext' subs >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | I_EXTR (_,r1,r2,r3,k),I_EXTR (_,r1',r2',r3',k') ->
       match_k k k' subs >>>
       add_subs
         [Reg(sr_name r1,r1');
          Reg(sr_name r2,r2');
          Reg(sr_name r3,r3');]
    | _,_ -> None

  let expl_instr subs =

    let return x n = x,n in

  let conv_reg = conv_reg subs in

    let find_lab lbl =
      let open BranchTarget in
      match lbl with
      | Lbl lbl -> find_lab subs lbl >! fun lbl -> Lbl lbl
      | Offset _ as tgt -> unitT tgt in

    let find_cst = find_cst subs in
    let find_shift = function
      | S_LSL(n) ->
          find_cst n >! fun n -> S_LSL(n)
      | S_LSR(n) ->
          find_cst n >! fun n -> S_LSR(n)
      | S_MSL(n) ->
          find_cst n >! fun n -> S_MSL(n)
      | S_ASR(n) ->
          find_cst n >! fun n -> S_ASR(n)
      | S_NOEXT -> return S_NOEXT in

    let expl_kr = function
      | RV(a,r) ->
          conv_reg r >! fun r -> RV(a,r)
      | K k ->
         find_cst k >! fun k -> K k in

    let module Ext = struct
        let expl = function
          | (e,None) -> fun st -> (e,None),st
          | (e,Some k) ->  find_cst k >! fun k -> (e,Some k)
    end in

    let conv_idx (k,m) =  find_cst k >! fun k -> k,m in

    let module MemExt = struct
      module E = AArch64Base.MemExt

      let expl = function
        | E.Imm idx ->
           conv_idx idx >! fun idx -> E.Imm idx
        | E.Reg (v,r,e,k) ->
           find_cst k >> fun k -> conv_reg r >! fun r -> E.Reg (v,r,e,k)
        | _ -> assert false

      end in

    let module OpExt = struct
      module E = AArch64Base.OpExt

      let expl = function
        | E.Imm (k1,k2) ->
           find_cst k1 >> fun k1 -> find_cst k2  >! fun k2 -> E.Imm (k1,k2)
        | E.Reg (r,s) ->
           conv_reg r
           >> fun r ->
              (match s with
              | E.LSL k ->
                 find_cst k >> fun k -> unitT (E.LSL k)
              | E.LSR k ->
                 find_cst k >> fun k -> unitT (E.LSR k)
              | E.ASR k ->
                 find_cst k >> fun k -> unitT (E.ASR k)
              | E.ROR k ->
                 find_cst k >> fun k -> unitT (E.ROR k))
          >! fun s ->
            E.Reg  (r,s)

    end in

    function
    | (I_FENCE _|I_NOP|I_RET None|I_ERET|I_SVC _|I_UDF _) as i -> unitT i
    | I_B l ->
        find_lab l >! fun l -> I_B l
    | I_BR r ->
        conv_reg r >! fun r -> I_BR r
    | I_RET (Some r) ->
        conv_reg r >! fun r -> I_RET (Some r)
    | I_BL l ->
        find_lab l >! fun l -> I_BL l
    | I_BLR r ->
        conv_reg r >! fun r -> I_BLR r

    | I_BC(a,l) ->
        find_lab l >! fun l -> I_BC (a,l)
    | I_CBZ(a,r,l) ->
        conv_reg r >> fun r ->
        find_lab l >! fun l ->
        I_CBZ (a,r,l)
    | I_CBNZ(a,r,l) ->
        conv_reg r >> fun r ->
        find_lab l >! fun l ->
        I_CBNZ (a,r,l)
    | I_TBNZ(a,r,k,l) ->
        conv_reg r >> fun r ->
        find_cst k >> fun k ->
        find_lab l >! fun l ->
        I_TBNZ (a,r,k,l)
    | I_TBZ(a,r,k,l) ->
        conv_reg r >> fun r ->
        find_cst k >> fun k ->
        find_lab l >! fun l ->
        I_TBZ (a,r,k,l)
    | I_MOV(a,r,kr) ->
        conv_reg r >> fun r ->
        expl_kr kr >! fun kr ->
        I_MOV(a,r,kr)
    | I_MOVZ(a,r,k,s) ->
        conv_reg r >> fun r  ->
        find_cst k >> fun k ->
        find_shift s >! fun s->
        I_MOVZ(a,r,k,s)
    | I_MOVN(a,r,k,s) ->
        conv_reg r >> fun r  ->
        find_cst k >> fun k ->
        find_shift s >! fun s->
        I_MOVN(a,r,k,s)
    | I_MOVK(a,r,k,s) ->
        conv_reg r >> fun r  ->
        find_cst k >> fun k ->
        find_shift s >! fun s->
        I_MOVK(a,r,k,s)
    | I_ADR (r,lbl) ->
        conv_reg r >> fun r ->
        find_lab lbl >! fun lbl ->
        I_ADR (r,lbl)
    | I_RBIT (v,r1,r2) ->
       conv_reg r1 >> fun r1 ->
       conv_reg r2 >! fun r2 ->
       I_RBIT (v,r1,r2)
    | I_ABS (v,r1,r2) ->
       conv_reg r1 >> fun r1 ->
       conv_reg r2 >! fun r2 ->
       I_ABS (v,r1,r2)
    | I_REV (v,r1,r2) ->
       conv_reg r1 >> fun r1 ->
       conv_reg r2 >! fun r2 ->
       I_REV (v,r1,r2)
    | I_EXTR (v,r1,r2,r3,k) ->
       conv_reg r1 >> fun r1 ->
       conv_reg r2 >> fun r2 ->
       conv_reg r3 >> fun r3 ->
       find_cst k >!  fun k ->
       I_EXTR (v,r1,r2,r3,k)
    | I_LDAR(a,b,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_LDAR(a,b,r1,r2)
    | I_LDARBH(a,b,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_LDARBH(a,b,r1,r2)
    | I_STLR(a,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_STLR(a,r1,r2)
    | I_STLRBH(a,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_STLRBH(a,r1,r2)
    | I_SXTW(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_SXTW(r1,r2)
    | I_SBFM(a,r1,r2,k1,k2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k1 >> fun k1 ->
        find_cst k2 >! fun k2 ->
        I_SBFM(a,r1,r2,k1,k2)
    | I_UBFM(a,r1,r2,k1,k2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k1 >> fun k1 ->
        find_cst k2 >! fun k2 ->
        I_UBFM(a,r1,r2,k1,k2)
    | I_STXR(a,b,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_STXR(a,b,r1,r2,r3)
    | I_STXRBH(a,b,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_STXRBH(a,b,r1,r2,r3)

    | I_LDR(a,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_LDR(a,r1,r2,e)
    | I_LDRSW(r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_LDRSW(r1,r2,e)
    | I_LDRS(v,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_LDRS(v,r1,r2,e)
    | I_LDUR(a,r1,r2,k) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k >! fun k ->
        I_LDUR(a,r1,r2,k)
    | I_LDP(t,a,r1,r2,r3,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_idx idx >! fun idx ->
        I_LDP (t,a,r1,r2,r3,idx)
    | I_LDPSW(r1,r2,r3,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_idx idx >! fun idx ->
        I_LDPSW(r1,r2,r3,idx)
    | I_LDXP(t,a,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_LDXP(t,a,r1,r2,r3)
    | I_STP(t,a,r1,r2,r3,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_idx idx >! fun idx ->
        I_STP(t,a,r1,r2,r3,idx)
    | I_STXP (a,b,r1,r2,r3,r4) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_reg r4 >! fun r4 ->
        I_STXP (a,b,r1,r2,r3,r4)
    | I_LDRBH(a,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_LDRBH(a,r1,r2,e)
    | I_STR(a,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_STR(a,r1,r2,e)
    | I_STRBH(a,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        MemExt.expl e >! fun e ->
        I_STRBH(a,r1,r2,e)
    | I_ADDSUBEXT (v1,op,r1,r2,(v3,r3),e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        Ext.expl e >! fun e ->
        I_ADDSUBEXT (v1,op,r1,r2,(v3,r3),e)
    | I_MOPL (sop,r1,r2,r3,r4) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_reg r4 >! fun r4 ->
        I_MOPL (sop,r1,r2,r3,r4)
    | I_MOP (op, v,r1,r2,r3,r4) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_reg r4 >! fun r4 ->
        I_MOP (op,v,r1,r2,r3,r4)
    | I_OP3 (a,b,r1,r2,e) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        OpExt.expl e >! fun e ->
        I_OP3(a,b,r1,r2,e)
    | I_CSEL(v,r1,r2,r3,c,op) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CSEL(v,r1,r2,r3,c,op)
    | I_CAS (v,a,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CAS(v,a,r1,r2,r3)
    | I_CASBH (v,a,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CASBH(v,a,r1,r2,r3)
    | I_CASP (v,a,r1,r2,r3,r4,r5) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        conv_reg r4 >> fun r4 ->
        conv_reg r5 >! fun r5 ->
        I_CASP(v,a,r1,r2,r3,r4,r5)
    | I_SWP (v,a,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_SWP(v,a,r1,r2,r3)
    | I_SWPBH (v,a,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_SWPBH(v,a,r1,r2,r3)
    | I_LDOP (op,v,rmw,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_LDOP (op,v,rmw,r1,r2,r3)
    | I_LDOPBH (op,v,rmw,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_LDOPBH (op,v,rmw,r1,r2,r3)
    | I_STOP (op,v,rmw,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_STOP (op,v,rmw,r1,r2)
    | I_STOPBH (op,v,rmw,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
          I_STOPBH (op,v,rmw,r1,r2)
    | I_IC (op,r) ->
        conv_reg r >! fun r -> I_IC (op,r)
    | I_DC (op,r) ->
        conv_reg r >! fun r -> I_DC (op,r)
    | I_TLBI (op,r) ->
        conv_reg r >! fun r -> I_TLBI (op,r)
    | I_MRS (r,sr) -> conv_reg r >! fun r -> I_MRS (r,sr)
    | I_MSR (sr,r) -> conv_reg r >! fun r -> I_MSR (sr,r)
    | I_STG (r1,r2,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_idx idx >! fun idx ->
        I_STG (r1,r2,idx)
    | I_STZG (r1,r2,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_idx idx >! fun idx ->
        I_STZG (r1,r2,idx)
    | I_STZ2G (r1,r2,idx) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_idx idx >! fun idx ->
        I_STZ2G (r1,r2,idx)
    | I_LDG (r1,r2,k) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k >! fun k ->
        I_LDG (r1,r2,k)
    | I_ALIGND(r1,r2,k) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k >! fun k ->
        I_ALIGND(r1,r2,k)
    | I_ALIGNU(r1,r2,k) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        find_cst k >! fun k ->
        I_ALIGNU(r1,r2,k)
    | I_BUILD(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_BUILD(r1,r2,r3)
    | I_CHKEQ(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_CHKEQ(r1,r2)
    | I_CHKSLD(r1) ->
        conv_reg r1 >! fun r1 ->
        I_CHKSLD(r1)
    | I_CHKTGD(r1) ->
        conv_reg r1 >! fun r1 ->
        I_CHKTGD(r1)
    | I_CLRTAG(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_CLRTAG(r1,r2)
    | I_CPYTYPE(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CPYTYPE(r1,r2,r3)
    | I_CPYVALUE(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CPYVALUE(r1,r2,r3)
    | I_CSEAL(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_CSEAL(r1,r2,r3)
    | I_GC(op,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_GC(op,r1,r2)
    | I_SC(op,r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_SC(op,r1,r2,r3)
    | I_SEAL(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_SEAL(r1,r2,r3)
    | I_UNSEAL(r1,r2,r3) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >! fun r3 ->
        I_UNSEAL(r1,r2,r3)
    | I_LDCT(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_LDCT(r1,r2)
    | I_STCT(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        I_STCT(r1,r2)
    (* Neon Extension *)
    | I_LD1 _ | I_LD1M _ | I_LD1R _ | I_LDAP1 _
    | I_LD2 _ | I_LD2M _ | I_LD2R _
    | I_LD3 _ | I_LD3M _ | I_LD3R _
    | I_LD4 _ | I_LD4M _ | I_LD4R _
    | I_ST1 _ | I_ST1M _ | I_STL1 _
    | I_ST2 _ | I_ST2M _
    | I_ST3 _ | I_ST3M _
    | I_ST4 _ | I_ST4M _
    | I_LDP_SIMD _
    | I_STP_SIMD _
    | I_LDR_SIMD _ | I_STR_SIMD _
    | I_LDUR_SIMD _ | I_LDAPUR_SIMD _
    | I_STUR_SIMD _ | I_STLUR_SIMD _
    | I_ADDV _ | I_DUP _ | I_FMOV_TG _
    | I_MOV_V _ | I_MOV_VE _ | I_MOV_S _
    | I_MOV_FG _ | I_MOV_TG _
    | I_MOVI_V _ | I_MOVI_S _
    | I_OP3_SIMD _ | I_ADD_SIMD _ | I_ADD_SIMD_S _
        -> Warn.fatal "Neon instructions are not implemented yet"
    (* Scalable Vector Extension *)
    | I_WHILELT _ | I_WHILELE _ | I_WHILELO _ | I_WHILELS _
    | I_UADDV _ | I_DUP_SV _ | I_ADD_SV _ | I_NEG_SV _ | I_MOVPRFX _
    | I_OP3_SV _
    | I_LD1SP _ | I_LD2SP _ | I_LD3SP _ | I_LD4SP _
    | I_ST1SP _ | I_ST2SP _ | I_ST3SP _ | I_ST4SP _
    | I_MOV_SV _ | I_PTRUE _
    | I_INDEX_SI _ | I_INDEX_IS _  | I_INDEX_SS _ | I_INDEX_II _
    | I_RDVL _ | I_ADDVL _ | I_CNT_INC_SVE _
    -> Warn.fatal "SVE instructions are not implemented yet"
    | I_SMSTART _ | I_SMSTOP _ | I_LD1SPT _ | I_ST1SPT _
    | I_MOVA_TV _ | I_MOVA_VT _ | I_ADDA _
    -> Warn.fatal "SME instructions are not implemented yet"
end)
