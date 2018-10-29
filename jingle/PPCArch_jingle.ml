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
  open PPCBase

  include Arch.MakeCommon(PPCBase)

  let match_instr subs pattern instr = match pattern,instr with
    | Padd(s,r1,r2,r3),Padd(s',r1',r2',r3')
    | Psub(s,r1,r2,r3),Psub(s',r1',r2',r3')
    | Psubf(s,r1,r2,r3),Psubf(s',r1',r2',r3')
    | Por(s,r1,r2,r3),Por(s',r1',r2',r3')
    | Pand(s,r1,r2,r3),Pand(s',r1',r2',r3')
    | Pxor(s,r1,r2,r3),Pxor(s',r1',r2',r3')
    | Pmull(s,r1,r2,r3),Pmull(s',r1',r2',r3')
    | Pdiv(s,r1,r2,r3),Pdiv(s',r1',r2',r3')
      when s = s' -> Some(add_subs
			    [Reg(sr_name r1,r1');
			     Reg(sr_name r2,r2');
			     Reg(sr_name r3,r3')]
			    subs)
       
    | Paddi(r1,r2,MetaConst.Meta k),Paddi(r1',r2',k')
    | Pandi(r1,r2,MetaConst.Meta k),Pandi(r1',r2',k')
    | Pori(r1,r2,MetaConst.Meta k),Pori(r1',r2',k')
    | Pxori(r1,r2,MetaConst.Meta k),Pxori(r1',r2',k')
    | Pmulli(r1,r2,MetaConst.Meta k),Pmulli(r1',r2',k')
    | Plwzu(r1,MetaConst.Meta k,r2),Plwzu(r1',k',r2')
    | Pstwu(r1,MetaConst.Meta k,r2),Pstwu(r1',k',r2')
    | Plmw(r1,MetaConst.Meta k,r2),Plmw(r1',k',r2')
    | Pstmw(r1,MetaConst.Meta k,r2),Pstmw(r1',k',r2')
      -> Some(add_subs [Cst(k,k');
			Reg(sr_name r1,r1');
			Reg(sr_name r2,r2')]
		subs)

    | Paddi(r1,r2,MetaConst.Int k),Paddi(r1',r2',k')
    | Pandi(r1,r2,MetaConst.Int k),Pandi(r1',r2',k')
    | Pori(r1,r2,MetaConst.Int k),Pori(r1',r2',k')
    | Pxori(r1,r2,MetaConst.Int k),Pxori(r1',r2',k')
    | Pmulli(r1,r2,MetaConst.Int k),Pmulli(r1',r2',k')
    | Plwzu(r1,MetaConst.Int k,r2),Plwzu(r1',k',r2')
    | Pstwu(r1,MetaConst.Int k,r2),Pstwu(r1',k',r2')
    | Plmw(r1,MetaConst.Int k,r2),Plmw(r1',k',r2')
    | Pstmw(r1,MetaConst.Int k,r2),Pstmw(r1',k',r2')
      when k = k'-> Some(add_subs [Reg(sr_name r1,r1');
				   Reg(sr_name r2,r2')]
			   subs)

    | Pmr(r1,r2),Pmr(r1',r2')
    | Pdcbf(r1,r2),Pdcbf(r1',r2')
      -> Some(add_subs [Reg(sr_name r1,r1');
			Reg(sr_name r2,r2')] subs)
       
    | Plwarx(r1,r2,r3),Plwarx(r1',r2',r3')
    | Pstwcx(r1,r2,r3),Pstwcx(r1',r2',r3')
      -> Some(add_subs [Reg(sr_name r1,r1');
			Reg(sr_name r2,r2');
			Reg(sr_name r3,r3')]
		subs)

    | Pmtlr r,Pmtlr r'
    | Pmflr r,Pmflr r'
      -> Some(add_subs [Reg(sr_name r,r')] subs)

    | Pcmpwi(i,r,k),Pcmpwi(i',r',k')
      when i = i' -> (match k with
      | MetaConst.Int k
	  when k = k'
	  -> Some(add_subs [Reg(sr_name r,r')] subs)
      | MetaConst.Meta s
	-> Some(add_subs [Reg(sr_name r,r');
			  Cst(s,k')] subs)
      | _ -> None)
    | Pcmpw(i,r1,r2),Pcmpw(i',r1',r2')
      when i = i' -> Some(add_subs [Reg(sr_name r1,r1');
				    Reg(sr_name r2,r2')]
			    subs)

    | Pli(r,MetaConst.Meta k),Pli(r',k')
      -> Some(add_subs [Reg(sr_name r,r');Cst(k,k')] subs)
    | Pli(r,MetaConst.Int k),Pli(r',k')
      when k = k' -> Some(add_subs [Reg(sr_name r,r')] subs)

    | Psync,Psync
    | Peieio,Peieio
    | Pisync,Pisync
    | Plwsync,Plwsync
    | Pblr,Pblr
    | Pcomment _,Pcomment _
      -> Some subs

    | Pb l,Pb l'
    | Pbl l,Pbl l'
      -> Some(add_subs [Lab(l,l')] subs)

    | Pbcc(c,l),Pbcc(c',l')
      when c = c' -> Some(add_subs [Lab(l,l')] subs)

    | Pload(s,r1,MetaConst.Int k,r2),Pload(s',r1',k',r2')
    | Pstore(s,r1,MetaConst.Int k,r2),Pstore(s',r1',k',r2')
      when s = s' && k = k' ->
       Some(add_subs [Reg(sr_name r1,r1');
		      Reg(sr_name r2,r2')]
	      subs)
	 
    | Pload(s,r1,MetaConst.Meta k,r2),Pload(s',r1',k',r2')
    | Pstore(s,r1,MetaConst.Meta k,r2),Pstore(s',r1',k',r2')
      when s = s' ->
       Some(add_subs [Reg(sr_name r1,r1');
		      Reg(sr_name r2,r2');
		      Cst(k,k')]
	      subs)
	 
    | Ploadx(s,r1,r2,r3),Ploadx(s',r1',r2',r3')
    | Pstorex(s,r1,r2,r3),Pstorex(s',r1',r2',r3')
      when s = s' ->
       Some(add_subs [Reg(sr_name r1,r1');
		      Reg(sr_name r2,r2');
		      Reg(sr_name r3,r3')]
	      subs)

    | Pneg(s,r1,r2),Pneg(s',r1',r2')
      when s = s' -> Some(add_subs [Reg(sr_name r1,r1');
				    Reg(sr_name r2,r2')]
			    subs)
       
    | Pnor(s,r1,r2,r3),Pnor(s',r1',r2',r3')
    | Pslw(s,r1,r2,r3),Pslw(s',r1',r2',r3')
    | Psraw(s,r1,r2,r3),Psraw(s',r1',r2',r3')
      when s = s' -> Some(add_subs [Reg(sr_name r1,r1');
				    Reg(sr_name r2,r2');
				    Reg(sr_name r3,r3')]
			    subs)
       
    | Psrawi(s,r1,r2,k),Psrawi(s',r1',r2',k')
      when s = s' -> (match k with
      | MetaConst.Int k
	  when k = k'
	  -> Some(add_subs [Reg(sr_name r1,r1');
			    Reg(sr_name r2,r2')]
		    subs)
      | MetaConst.Meta s
	-> Some(add_subs [Reg(sr_name r1,r1');
			  Reg(sr_name r2,r2');
			  Cst(s,k')] subs)
      | _ -> None)
    | _,_ -> None


  let expl_instr subs free label_env reg_env =
    let conv_reg = conv_reg subs free reg_env in
    let find_lab = find_lab subs free label_env in
    let find_cst = find_cst subs free in
    function
    | Padd(s,r1,r2,r3) ->
       Padd(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Psub(s,r1,r2,r3) ->
       Psub(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Psubf(s,r1,r2,r3) ->
       Psubf(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Por(s,r1,r2,r3) ->
       Por(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Pand(s,r1,r2,r3) ->
       Pand(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Pxor(s,r1,r2,r3) ->
       Pxor(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Pmull(s,r1,r2,r3) ->
       Pmull(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Pdiv(s,r1,r2,r3) ->
       Pdiv(s,conv_reg r1, conv_reg r2, conv_reg r3)
    | Paddi(r1,r2,MetaConst.Meta k) ->
       Paddi(conv_reg r1, conv_reg r2, find_cst k)
    | Paddi(r1,r2,c) ->
       Paddi(conv_reg r1, conv_reg r2, c)
    | Pandi(r1,r2,MetaConst.Meta k) ->
       Pandi(conv_reg r1, conv_reg r2, find_cst k)
    | Pandi(r1,r2,c) ->
       Pandi(conv_reg r1, conv_reg r2, c)
    | Pori(r1,r2,MetaConst.Meta k) ->
       Pori(conv_reg r1, conv_reg r2, find_cst k)
    | Pori(r1,r2,c) ->
       Pori(conv_reg r1, conv_reg r2, c)
    | Pxori(r1,r2,MetaConst.Meta k) ->
       Pxori(conv_reg r1, conv_reg r2, find_cst k)
    | Pxori(r1,r2,c) ->
       Pxori(conv_reg r1, conv_reg r2, c)
    | Pmulli(r1,r2,MetaConst.Meta k) ->
       Pmulli(conv_reg r1, conv_reg r2, find_cst k)
    | Pmulli(r1,r2,c) ->
       Pmulli(conv_reg r1, conv_reg r2, c)
    | Pli(r,MetaConst.Meta k) ->
       Pli(conv_reg r,find_cst k)
    | Pli(r,c) ->
       Pli(conv_reg r,c)
    | Pb l -> Pb(find_lab l)
    | Pbcc(c,l) -> Pbcc(c,find_lab l)
    | Pcmpwi(i,r,MetaConst.Meta k) ->
       Pcmpwi(i,conv_reg r, find_cst k)
    | Pcmpwi(i,r,c) ->
       Pcmpwi(i,conv_reg r, c)
    | Pcmpw(i,r1,r2) ->
       Pcmpw(i,conv_reg r1,conv_reg r2)
    | Plwzu(r1,MetaConst.Meta k,r2) ->
       Plwzu(conv_reg r1, find_cst k, conv_reg r2)
    | Plwzu(r1,c,r2) ->
       Plwzu(conv_reg r1, c, conv_reg r2)
    | Pmr(r1,r2) -> Pmr(conv_reg r1,conv_reg r2)
    | Pstwu(r1,MetaConst.Meta k,r2) ->
       Pstwu(conv_reg r1, find_cst k, conv_reg r2)
    | Pstwu(r1,c,r2) ->
       Pstwu(conv_reg r1, c, conv_reg r2)
    | Plwarx(r1,r2,r3) ->
       Plwarx(conv_reg r1,conv_reg r2,conv_reg r3)
    | Pstwcx(r1,r2,r3) ->
       Pstwcx(conv_reg r1,conv_reg r2,conv_reg r3)
    | Pload(s,r1,MetaConst.Meta k,r2) ->
       Pload(s,conv_reg r1, find_cst k, conv_reg r2)
    | Pload(s,r1,c,r2) ->
       Pload(s,conv_reg r1, c, conv_reg r2)
    | Ploadx(s,r1,r2,r3) ->
       Ploadx(s,conv_reg r1,conv_reg r2,conv_reg r3)
    | Pstore(s,r1,MetaConst.Meta k,r2) ->
       Pstore(s,conv_reg r1, find_cst k, conv_reg r2)
    | Pstore(s,r1,c,r2) ->
       Pstore(s,conv_reg r1, c, conv_reg r2)
    | Pstorex(s,r1,r2,r3) ->
       Pstorex(s,conv_reg r1,conv_reg r2,conv_reg r3)
    | Psync -> Psync
    | Peieio -> Peieio
    | Pisync -> Pisync
    | Plwsync -> Plwsync
    | Pdcbf(r1,r2) -> Pdcbf(conv_reg r1,conv_reg r2)
    | Pnor(s,r1,r2,r3) ->
       Pnor(s,conv_reg r1,conv_reg r2,conv_reg r3)
    | Pneg(s,r1,r2) ->
       Pneg(s,conv_reg r1,conv_reg r2)
    | Pslw(s,r1,r2,r3) ->
       Pslw(s,conv_reg r1,conv_reg r2,conv_reg r3)
    | Psrawi(s,r1,r2,MetaConst.Meta k) ->
       Psrawi(s,conv_reg r1,conv_reg r2,find_cst k)
    | Psrawi(s,r1,r2,c) ->
       Psrawi(s,conv_reg r1,conv_reg r2,c)
    | Psraw(s,r1,r2,r3) ->
       Psraw(s,conv_reg r1,conv_reg r2,conv_reg r3)
    | Pbl l -> Pbl(find_lab l)
    | Pblr -> Pblr
    | Pmtlr r -> Pmtlr(conv_reg r)
    | Pmflr r -> Pmflr(conv_reg r)
    | Plmw(r1,MetaConst.Meta k,r2) ->
       Plmw(conv_reg r1, find_cst k, conv_reg r2)
    | Plmw(r1,c,r2) ->
       Plmw(conv_reg r1, c, conv_reg r2)
    | Pstmw(r1,MetaConst.Meta k,r2) ->
       Pstmw(conv_reg r1, find_cst k, conv_reg r2)
    | Pstmw(r1,c,r2) ->
       Pstmw(conv_reg r1, c, conv_reg r2)
    | Pcomment s -> Pcomment s
       
end)

