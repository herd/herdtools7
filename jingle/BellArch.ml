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
      -> Some(add_subs [Reg(sr_name r,r')] subs)
    | Imm (MetaConst.Meta m),Imm i
      -> Some(add_subs [Cst(m,i)] subs)
    | Imm (MetaConst.Int m),Imm i when m=i -> Some(subs)
    | _ -> None
       
  let match_reg_or_addr subs ra ra' = match ra,ra' with
    | Rega r,Rega r' -> Some(add_subs [Reg(sr_name r,r')] subs)
    | Abs x,Abs y -> 
       let s = SymbConstant.pp false x in
       let regn = SymbConstant.pp false y in
       Some(add_subs [Addr(s,regn)] subs)
    | _,_ -> None
       
  let match_iar subs iar iar' = match iar,iar' with
    | IAR_roa ra,IAR_roa ra' 
      -> match_reg_or_addr subs ra ra'
    | IAR_imm (MetaConst.Meta m), IAR_imm i 
      -> Some(add_subs [Cst(m,i)] subs)
    | IAR_imm (MetaConst.Int v), IAR_imm i 
      when v = i -> Some subs
    | _,_ -> None
       
  let match_op subs op op' = match op,op' with
    | Add(iar1,iar2),Add(iar1',iar2') 
    | And(iar1,iar2),And(iar1',iar2')
    | Xor(iar1,iar2),Xor(iar1',iar2') 
    | Eq(iar1,iar2),Eq(iar1',iar2') 
    | Neq(iar1,iar2),Neq(iar1',iar2') ->
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
       begin
	 match match_reg_or_addr subs ra ra' with
	 | Some subs -> match_reg_or_imm subs ri ri'
	 | None -> None
       end
    | _,_ -> None
       
  let match_instr subs pattern instr = match pattern,instr with
    | Pld(r,ao,s),Pld(r',ao',s') ->
       if annots_compare s s'
       then match match_addr_op subs ao ao' with
       | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
       | None -> None
       else None
	 
    | Pst(ao,ri,s),Pst(ao',ri',s') ->
       if annots_compare s s'
       then match match_addr_op subs ao ao' with
       | Some subs -> match_reg_or_imm subs ri ri'
       | None -> None
       else None
	 
    | Prmw(r,op,ao,s),Prmw(r',op',ao',s') ->
       if annots_compare s s'
       then match match_op subs op op' with
       | None -> None
       | Some subs ->
	  match match_addr_op subs ao ao' with
	  | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
	  | None -> None
       else None
	 
    | Pfence Fence(s,_), Pfence Fence(s',_) ->
       if annots_compare s s' 
       then Some subs
       else None
	 
    | Pbranch(_,lp,s), Pbranch(_,li,s') -> 
       if annots_compare s s'
       then Some(add_subs [Lab(lp,li)] subs)
       else None
	 
    | Pmov(r,op),Pmov(r',op') ->
       begin match match_op subs op op' with
       | None -> None
       | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
       end
	 
    | _,_ -> None

  let expl_instr subs free = 
    let conv_reg = conv_reg subs free in
    let find_lab = find_lab subs free in
    let find_cst = find_cst subs free in
    let rec expl = function
      | Pld(r,ao,s) -> Pld(conv_reg r,expl_ao ao,s)
      | Pst(ao,ri,s) -> Pst(expl_ao ao,expl_ri ri,s)
      | Prmw(r,op,ao,s) -> Prmw(conv_reg r,expl_op op,expl_ao ao,s)
      | Pbranch(a,l,b) -> Pbranch(a,find_lab l,b)
      | Pmov(r,op) -> Pmov(conv_reg r,expl_op op)
      | i -> i
    and expl_ao = function
      | Addr_op_atom ra -> Addr_op_atom(expl_ra ra)
      | Addr_op_add(ra,ri) -> Addr_op_add(expl_ra ra, expl_ri ri)
    and expl_ri = function
      | Regi r -> Regi(conv_reg r)
      | Imm(MetaConst.Meta v) -> Imm(find_cst v)
      | i -> i
    and expl_op = function
      | Add(iar1,iar2) -> Add(expl_iar iar1,expl_iar iar2)
      | And(iar1,iar2) -> And(expl_iar iar1,expl_iar iar2)
      | Xor(iar1,iar2) -> Xor(expl_iar iar1,expl_iar iar2)
      | Eq(iar1,iar2) -> Eq(expl_iar iar1,expl_iar iar2)
      | Neq(iar1,iar2) -> Neq(expl_iar iar1,expl_iar iar2)
      | RAI(iar) -> RAI(expl_iar iar)
    and expl_ra = function
      | Rega r -> Rega(conv_reg r)
      | abs -> abs
    and expl_iar = function
      | IAR_roa ra -> IAR_roa(expl_ra ra)
      | IAR_imm(MetaConst.Meta v) -> IAR_imm(find_cst v)
      | i -> i
    in expl
end)
