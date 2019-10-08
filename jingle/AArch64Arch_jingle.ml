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

  let match_kr subs kr kr' =  match kr,kr' with
    | K(MetaConst.Meta m),K i ->  add_subs [Cst(m, i)] subs
    | RV(_,r),RV(_,r') -> add_subs [Reg(sr_name r,r')] subs
    | K(MetaConst.Int i),K(j) when i=j -> Some subs
    | _ -> None

  let match_instr subs pattern instr = match pattern,instr with
    | I_NOP,I_NOP -> Some subs
    | I_FENCE fp,I_FENCE fi when fp = fi
                            -> Some subs

    | I_B lp, I_B li
      -> add_subs [Lab(lp,li)] subs

    | I_BC(cp,lp), I_BC(ci,li) when cp = ci
                               -> add_subs [Lab(lp,li)] subs

    | I_CBZ(_,r,lp),I_CBZ(_,r',li)
    | I_CBNZ(_,r,lp),I_CBNZ(_,r',li)
      ->  add_subs [Reg(sr_name r,r'); Lab(lp,li)] subs

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
    | I_LDR(_,r1,r2,kr),I_LDR(_,r1',r2',kr')
    | I_STR(_,r1,r2,kr),I_STR(_,r1',r2',kr')
      ->
        match_kr subs kr kr' >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]

    | I_OP3(_,opp,r1,r2,kr),I_OP3(_,opi,r1',r2',kr') when opp=opi
      ->
        match_kr subs kr kr' >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | _,_ -> None

  let expl_instr subs =
    let conv_reg = conv_reg subs in
    let find_lab = find_lab subs in
    let find_cst = find_cst subs in
    let expl_kr = function
      | RV(a,r) ->
          conv_reg r >! fun r -> RV(a,r)
      | K k ->
          find_cst k >! fun k -> K k in
    function
    | (I_FENCE _|I_NOP|I_RET None) as i -> unitT i
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
    | I_MOV(a,r,kr) ->
        conv_reg r >> fun r ->
        expl_kr kr >! fun kr ->
        I_MOV(a,r,kr)
    | I_ADDR (r,lbl) ->
        conv_reg r >> fun r ->
        find_lab lbl >! fun lbl ->
        I_ADDR (r,lbl)
    | I_RBIT (v,r1,r2) ->
        conv_reg r1 >> fun r1 -> conv_reg r2 >! fun r2 -> I_RBIT (v,r1,r2)
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
    | I_LDR(a,r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_LDR(a,r1,r2,kr)
    | I_LDP(t,a,r1,r2,r3,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        expl_kr kr  >! fun kr ->
        I_LDP(t,a,r1,r2,r3,kr)
    | I_STP(t,a,r1,r2,r3,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        conv_reg r3 >> fun r3 ->
        expl_kr kr >! fun kr ->
        I_STP(t,a,r1,r2,r3,kr)
    | I_LDRBH(a,r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_LDRBH(a,r1,r2,kr)
    | I_STR(a,r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_STR(a,r1,r2,kr)
    | I_STRBH(a,r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_STRBH(a,r1,r2,kr)
    | I_OP3(a,b,r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_OP3(a,b,r1,r2,kr)
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
    | I_MRS (r,sr) -> conv_reg r >! fun r -> I_MRS (r,sr)
    | I_STG (r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_STG (r1,r2,kr)
    | I_LDG (r1,r2,kr) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >> fun r2 ->
        expl_kr kr >! fun kr ->
        I_LDG (r1,r2,kr)
end)
