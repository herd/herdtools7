(****************************************************************************)
(*                           the diy toolsuite                              *)
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

  include Arch.MakeCommon(AArch64Base)

  let match_kr subs kr kr' = match kr,kr' with
    | K(MetaConst.Meta m),K i ->
       Some(add_subs [Cst(m, i)] subs)
    | RV(_,r),RV(_,r') -> Some(add_subs [Reg(sr_name r,r')] subs)
    | K(MetaConst.Int i),K(j) when i=j -> Some subs
    | _ -> None

  let match_instr subs pattern instr = match pattern,instr with
    | I_FENCE fp,I_FENCE fi when fp = fi
                            -> Some subs

    | I_B lp, I_B li
      -> Some(add_subs [Lab(lp,li)] subs)

    | I_BC(cp,lp), I_BC(ci,li) when cp = ci
                               -> Some(add_subs [Lab(lp,li)] subs)

    | I_CBZ(_,r,lp),I_CBZ(_,r',li)
    | I_CBNZ(_,r,lp),I_CBNZ(_,r',li)
      -> Some(add_subs [Reg(sr_name r,r');
                        Lab(lp,li)] subs)

    | I_MOV(_,r,K MetaConst.Meta m),I_MOV(_,r',K i)
      -> Some(add_subs [Reg(sr_name r,r');
                        Cst(m,i)] subs)

    | I_LDAR(_,tp,r1,r2),I_LDAR(_,ti,r1',r2') when tp = ti
     -> Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)

    | I_STLR(_,r1,r2),I_STLR(_,r1',r2')
    | I_SXTW(r1,r2),I_SXTW(r1',r2')
      -> Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)

    | I_STXR(_,tp,r1,r2,r3),I_STXR(_,ti,r1',r2',r3') when tp = ti
      -> Some(add_subs [Reg(sr_name r1,r1');
                        Reg(sr_name r2,r2');
                        Reg(sr_name r3,r3')]
                subs)

    | I_LDR(_,r1,r2,kr),I_LDR(_,r1',r2',kr')
    | I_STR(_,r1,r2,kr),I_STR(_,r1',r2',kr')
      -> begin match match_kr subs kr kr' with
      | Some subs
        -> Some(add_subs [Reg(sr_name r1,r1');
                          Reg(sr_name r2,r2')]
                  subs)
      | None -> None
      end

    | I_OP3(_,opp,r1,r2,kr),I_OP3(_,opi,r1',r2',kr') when opp=opi
      -> begin match match_kr subs kr kr' with
      | Some subs
        -> Some(add_subs [Reg(sr_name r1,r1');
                          Reg(sr_name r2,r2')]
                  subs)
      | None -> None
      end

    | _,_ -> None

  let expl_instr subs free label_env reg_env =
    let conv_reg = conv_reg subs free reg_env in
    let find_lab = find_lab subs free label_env in
    let find_cst = find_cst subs free in
    let expl_kr = function
      | RV(a,r) -> RV(a,conv_reg r)
      | K(MetaConst.Meta v) -> K(find_cst v)
      | kr -> kr in
    function
    | I_FENCE b -> I_FENCE b
    | I_B l -> I_B(find_lab l)
    | I_BC(a,l) -> I_BC(a,find_lab l)
    | I_CBZ(a,r,l) -> I_CBZ(a,conv_reg r,find_lab l)
    | I_CBNZ(a,r,l) -> I_CBNZ(a,conv_reg r,find_lab l)
    | I_MOV(a,r,K MetaConst.Meta v) -> I_MOV(a,conv_reg r,K (find_cst v))
    | I_MOV(a,r,c) -> I_MOV(a,conv_reg r,c)
    | I_LDAR(a,b,r1,r2) -> I_LDAR(a,b,conv_reg r1,conv_reg r2)
    | I_LDARBH(a,b,r1,r2) -> I_LDARBH(a,b,conv_reg r1,conv_reg r2)
    | I_STLR(a,r1,r2) -> I_STLR(a,conv_reg r1,conv_reg r2)
    | I_STLRBH(a,r1,r2) -> I_STLRBH(a,conv_reg r1,conv_reg r2)
    | I_SXTW(r1,r2) -> I_SXTW(conv_reg r1,conv_reg r2)
    | I_STXR(a,b,r1,r2,r3) -> I_STXR(a,b,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_STXRBH(a,b,r1,r2,r3) -> I_STXRBH(a,b,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_LDR(a,r1,r2,kr) -> I_LDR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_LDP(t,a,r1,r2,r3,kr) ->
        I_LDP(t,a,conv_reg r1,conv_reg r2,conv_reg r3,expl_kr kr)
    | I_STP(t,a,r1,r2,r3,kr) ->
        I_STP(t,a,conv_reg r1,conv_reg r2,conv_reg r3,expl_kr kr)
    | I_LDRBH(a,r1,r2,kr) -> I_LDRBH(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_STR(a,r1,r2,kr) -> I_STR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_STRBH(a,r1,r2,kr) -> I_STRBH(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_OP3(a,b,r1,r2,kr) -> I_OP3(a,b,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_CSEL(v,r1,r2,r3,c,op) ->
        I_CSEL(v,conv_reg r1,conv_reg r2,conv_reg r3,c,op)
    | I_CAS (v,a,r1,r2,r3) ->
        I_CAS(v,a,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_CASBH (v,a,r1,r2,r3) ->
        I_CASBH(v,a,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_SWP (v,a,r1,r2,r3) ->
        I_SWP(v,a,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_SWPBH (v,a,r1,r2,r3) ->
        I_SWPBH(v,a,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_LDOP (op,v,rmw,r1,r2,r3) ->
        I_LDOP (op,v,rmw,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_LDOPBH (op,v,rmw,r1,r2,r3) ->
        I_LDOPBH (op,v,rmw,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_STOP (op,v,rmw,r1,r2) ->
        I_STOP (op,v,rmw,conv_reg r1,conv_reg r2)
    | I_STOPBH (op,v,rmw,r1,r2) ->
        I_STOPBH (op,v,rmw,conv_reg r1,conv_reg r2)
end)
