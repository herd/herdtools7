(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
include Arch.MakeArch(struct

  open RISCVBase

  include Arch.MakeCommon(RISCVBase)

  let match_const k k' subs = match k with
  | MetaConst.Meta k -> add_subs [Cst (k,k')] subs
  | MetaConst.Int k -> if k=k' then Some subs else None

  let match_2r subs r1 r2 r1' r2' =
    match_reg r1 r1' subs >>> match_reg r2 r2'

  let match_const_2r subs k r1 r2 k' r1' r2' =
    match_const k k' subs >>> fun subs -> match_2r subs r1 r2 r1' r2'

  let match_3r subs r1 r2 r3 r1' r2' r3' =
    match_2r subs r1 r2 r1' r2' >>> match_reg r3 r3'

  let match_instr subs pattern instr = match pattern,instr with
  | OpI (op,r1,r2,k),OpI(op',r1',r2',k') when op=op' ->
      match_const_2r subs k r1 r2 k' r1' r2'
  | OpI2 (op,r1,k),OpI2(op',r1',k') when op=op' ->
      match_reg r1 r1' subs >>> match_const k k'
  | OpA (op,r1,lbl),OpA(op',r1',lbl') when op=op' ->
      add_subs [Lab (lbl,lbl'); Reg (sr_name r1, r1')] subs
  | OpIW (op,r1,r2,k),OpIW(op',r1',r2',k') when op=op' ->
      match_const_2r subs k r1 r2 k' r1' r2'
  | Op (op,r1,r2,r3),Op(op',r1',r2',r3') when op=op' ->
      match_3r subs r1 r2 r3 r1' r2' r3'
  | OpW (op,r1,r2,r3),OpW(op',r1',r2',r3') when op=op' ->
      match_3r subs r1 r2 r3 r1' r2' r3'
  | AUIPC (r1,k1), AUIPC (r1',k1') ->
      match_reg r1 r1' subs >>> match_const k1 k1'
  | J lbl,J lbl' ->
      add_subs [Lab (lbl,lbl')] subs
  | Bcc (c,r1,r2,lbl),Bcc (c',r1',r2',lbl') when c=c' ->
      add_subs
           [Lab (lbl,lbl'); Reg (sr_name r1,r1'); Reg (sr_name r2,r2');]
           subs
  | Load (w,s,m,r1,i,r2),Load (w',s',m',r1',i',r2')
    when w=w' && s=s' && m=m' && i=i' ->
      match_2r subs r1 r2 r1' r2'
  | Store (w,m,r1,i,r2),Store (w',m',r1',i',r2')
    when w=w' && m=m' && i=i' ->
      match_2r subs r1 r2 r1' r2'
  | LoadReserve (w,m,r1,r2),LoadReserve (w',m',r1',r2')
    when w=w' && m=m' ->
      match_2r subs r1 r2 r1' r2'
  | Ext (s,w,r1,r2),Ext (s',w',r1',r2') when s=s' && w=w' ->
      match_2r subs r1 r2 r1' r2'
  | StoreConditional (w,m,r1,r2,r3),StoreConditional (w',m',r1',r2',r3')
    when w=w' && m=m' ->
      match_3r subs r1 r2 r3 r1' r2' r3'
  | Amo (op,w,m,r1,r2,r3),Amo (op',w',m',r1',r2',r3')
    when op=op' && w=w' && m=m' ->
      match_3r subs r1 r2 r3 r1' r2' r3'
  | FenceIns b,FenceIns b'
    when b=b'  -> Some subs
  | _,_ -> None

  let expl_instr subs =
    let conv_reg = conv_reg subs
    and find_lab = find_lab subs
    and find_cst = find_cst subs in
    function
      | OpI (op,r1,r2,k) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          find_cst k  >! fun k ->
          OpI (op,r1,r2,k)
      | OpI2 (op,r1,k) ->
          conv_reg r1 >> fun r1 ->
          find_cst k  >! fun k ->
          OpI2 (op,r1,k)
      | OpA (op,r1,lbl) ->
          conv_reg r1 >> fun r1 ->
          find_lab lbl >! fun lbl ->
          OpA (op,r1,lbl)
      | OpIW (op,r1,r2,k) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          find_cst k  >! fun k ->
          OpIW (op,r1,r2,k)
      | Op (op,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          Op (op,r1,r2,r3)
      | OpW (op,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          OpW (op,r1,r2,r3)
      | Ext (s,w,r1,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          Ext (s,w,r1,r2)
      | J lbl->
          find_lab lbl >! fun lbl -> J lbl
      | AUIPC (r1,k) ->
          conv_reg r1 >> fun r1 ->
          find_cst k  >! fun k ->
          AUIPC (r1,k)
      | Bcc (c,r1,r2,lbl) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          find_lab lbl >! fun lbl ->
          Bcc (c,r1,r2,lbl)
      | Load (w,s,m,r1,i,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          Load (w,s,m,r1,i,r2)
      | Store (w,m,r1,i,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          Store (w,m,r1,i,r2)
      | LoadReserve (w,m,r1,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          LoadReserve (w,m,r1,r2)
      | StoreConditional (w,m,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          StoreConditional (w,m,r1,r2,r3)
      | Amo (op,w,m,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          Amo (op,w,m,r1,r2,r3)
      | INop|Ret|FenceIns _ as i -> unitT i
end)
