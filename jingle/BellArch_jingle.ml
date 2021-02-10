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
  open BellBase

  include Arch.MakeCommon(BellBase)

  let rec annots_compare s s' =
    match s,s' with
    | [],[] -> true
    | x::s,y::s' when String.compare x y = 0 -> annots_compare s s'
    | _ -> false

  let match_reg_or_imm subs ri ri' = match ri,ri' with
    | Regi r,Regi r'
      -> add_subs [Reg(sr_name r,r')] subs
    | Imm (MetaConst.Meta m),Imm i
      -> add_subs [Cst(m,i)] subs
    | Imm (MetaConst.Int m),Imm i when m=i -> Some subs
    | _ -> None

  let match_reg_or_addr subs ra ra' = match ra,ra' with
    | Rega r,Rega r' -> add_subs [Reg(sr_name r,r')] subs
    | Abs x,Abs y ->
       add_subs [Addr(x,y)] subs
    | _,_ -> None

  let match_iar subs iar iar' = match iar,iar' with
    | IAR_roa ra,IAR_roa ra'
      -> match_reg_or_addr subs ra ra'
    | IAR_imm (MetaConst.Meta m), IAR_imm i
      -> add_subs [Cst(m,i)] subs
    | IAR_imm (MetaConst.Int v), IAR_imm i
      when v = i -> Some subs
    | _,_ -> None

  let match_op subs op op' = match op,op' with
    | OP (op,iar1,iar2),OP (op',iar1',iar2') when op=op' ->
       begin match (match_iar subs iar1 iar1') with
       | Some subs -> match_iar subs iar2 iar2'
       | None -> None
       end
    | RAI(iar),RAI(iar') -> match_iar subs iar iar'
    | _,_ -> None

  let match_addr_op subs ao ao' = match ao,ao' with
    | Addr_op_atom ra,Addr_op_atom ra'
      -> match_reg_or_addr subs ra ra'
    | Addr_op_add(ra,ri),Addr_op_add(ra',ri') ->
        match_reg_or_addr subs ra ra' >>> fun subs ->
        match_reg_or_imm subs ri ri'
    | _,_ -> None

  let match_instr subs pattern instr = match pattern,instr with
    | Pld(r,ao,s),Pld(r',ao',s') ->
       if annots_compare s s'
       then
         match_addr_op subs ao ao' >>> add_subs [Reg(sr_name r,r')]
       else None

    | Pst(ao,ri,s),Pst(ao',ri',s') ->
       if annots_compare s s' then
         match_addr_op subs ao ao' >>> fun subs ->
         match_reg_or_imm subs ri ri'
       else None

    | Prmw(r,op,ao,s),Prmw(r',op',ao',s') ->
       if annots_compare s s' then
        match_op subs op op' >>> fun subs ->
        match_addr_op subs ao ao' >>>
        add_subs [Reg(sr_name r,r')]
       else None

    | Pfence Fence(s,_), Pfence Fence(s',_) ->
       if annots_compare s s' then Some subs else None

    | Pbranch(None,lp,s), Pbranch(None,li,s') ->
        if annots_compare s s' then
          add_subs [Lab(lp,li)] subs
       else None

    | Pbranch(Some r,lp,s), Pbranch(Some r',li,s') ->
       if annots_compare s s'
       then
         add_subs [Reg(sr_name r,r');Lab(lp,li)] subs
       else None

    | Pmov(r,op),Pmov(r',op') ->
       match_op subs op op' >>> add_subs [Reg(sr_name r,r')]

    | _,_ -> None

  let expl_instr subs =

    let conv_reg = conv_reg subs
    and find_lab = find_lab subs
    and find_cst = find_cst subs in

    let rec expl i = match i with
    | Pld(r,ao,s) ->
        conv_reg r >> fun r ->
        expl_ao ao >! fun ao ->
        Pld (r,ao,s)
    | Pst(ao,ri,s) ->
        expl_ao ao >> fun ao ->
        expl_ri ri >! fun ri ->
        Pst (ao,ri,s)
    | Prmw(r,op,ao,s) ->
        conv_reg r >> fun r ->
        expl_op op >> fun op  ->
        expl_ao ao >! fun ao ->
        Prmw(r,op,ao,s)
    | Pbranch(None,lab,b) ->
        find_lab lab >! fun lab -> Pbranch (None,lab,b)
    | Pbranch(Some r,lab,b) ->
        conv_reg r >> fun r ->
        find_lab lab >! fun lab ->
        Pbranch (Some r,lab,b)
    | Pmov(r,op) ->
        conv_reg r >> fun r ->
        expl_op op >! fun op ->
        Pmov (r,op)
    | i -> unitT i

    and expl_ao ao =match ao with
    | Addr_op_atom ra ->
        expl_ra ra >! fun ra -> Addr_op_atom ra
    | Addr_op_add(ra,ri) ->
        expl_ra ra >> fun ra ->
        expl_ri ri >! fun ri ->
        Addr_op_add (ra,ri)

    and expl_ri ri = match ri with
    | Regi r -> conv_reg r >! fun r -> Regi r
    | Imm k -> find_cst k >! fun k -> Imm k

    and expl_op = function
      | OP(op,iar1,iar2) ->
          expl_iar iar1 >> fun iar1 ->
          expl_iar iar2 >! fun iar2 ->
          OP (op,iar1,iar2)
      | RAI(iar) -> expl_iar iar >! fun iar -> RAI iar

    and expl_ra = function
      | Rega r -> conv_reg r >! fun r -> Rega r
      | abs -> unitT abs

    and expl_iar = function
      | IAR_roa ra -> expl_ra ra >! fun ra -> IAR_roa ra
      | IAR_imm k  -> find_cst k >! fun k -> IAR_imm k

    in expl
end)
