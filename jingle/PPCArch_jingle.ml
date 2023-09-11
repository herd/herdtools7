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

  let match_const subs k k' =
    let open MetaConst in
    match k with
    | Int i ->
        if Misc.int_eq i k' then Some subs else None
    | Meta k -> add_subs [Cst(k,k');] subs

  let match_instr subs pattern instr = match pattern,instr with
    | Padd(s,r1,r2,r3),Padd(s',r1',r2',r3')
    | Psub(s,r1,r2,r3),Psub(s',r1',r2',r3')
    | Psubf(s,r1,r2,r3),Psubf(s',r1',r2',r3')
    | Por(s,r1,r2,r3),Por(s',r1',r2',r3')
    | Pand(s,r1,r2,r3),Pand(s',r1',r2',r3')
    | Pxor(s,r1,r2,r3),Pxor(s',r1',r2',r3')
    | Pmull(s,r1,r2,r3),Pmull(s',r1',r2',r3')
    | Pdiv(s,r1,r2,r3),Pdiv(s',r1',r2',r3')
      when s = s' ->
        add_subs
	  [Reg(sr_name r1,r1');
	   Reg(sr_name r2,r2');
	   Reg(sr_name r3,r3')]
	  subs

    | Paddi(r1,r2,k),Paddi(r1',r2',k')
    | Pandi(r1,r2,k),Pandi(r1',r2',k')
    | Pori(r1,r2,k),Pori(r1',r2',k')
    | Pxori(r1,r2,k),Pxori(r1',r2',k')
    | Pmulli(r1,r2,k),Pmulli(r1',r2',k')
    | Plwzu(r1,k,r2),Plwzu(r1',k',r2')
    | Plwa(r1,k,r2),Plwa(r1',k',r2')
    | Pstwu(r1,k,r2),Pstwu(r1',k',r2')
    | Plmw(r1,k,r2),Plmw(r1',k',r2')
    | Pclrldi(r1,r2,k), Pclrldi (r1',r2',k')
    | Pstmw(r1,k,r2),Pstmw(r1',k',r2')
      ->
        match_const subs k k' >>>
        add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')]
    | Prlwimi(r1,r2,k1,k2,k3),Prlwimi(r1',r2',k1',k2',k3')
    | Prlwinm(r1,r2,k1,k2,k3),Prlwinm(r1',r2',k1',k2',k3')
      ->
          add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2');
                    Cst(cv_name k1,k1'); Cst (cv_name k2,k2');
                    Cst(cv_name k3,k3')]
                   subs

    | Pmr(r1,r2),Pmr(r1',r2')
    | Pextsw(r1,r2),Pextsw(r1',r2')
    | Pdcbf(r1,r2),Pdcbf(r1',r2')
      ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
          subs

    | Plwarx(r1,r2,r3),Plwarx(r1',r2',r3')
    | Pstwcx(r1,r2,r3),Pstwcx(r1',r2',r3')
      ->
        add_subs
          [Reg(sr_name r1,r1');	Reg(sr_name r2,r2'); Reg(sr_name r3,r3')]
	  subs

    | Pmtlr r,Pmtlr r'
    | Pmflr r,Pmflr r'
      ->
        add_subs [Reg(sr_name r,r')] subs

    | Pcmplwi(i,r,k),Pcmplwi(i',r',k')
    | Pcmpwi(i,r,k),Pcmpwi(i',r',k')
      when i = i' ->
        match_const subs k k'  >>> add_subs [Reg(sr_name r,r')]
    | Pcmpw(i,r1,r2),Pcmpw(i',r1',r2')
      when i = i' ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2');]
	  subs

    | Pli(r,k),Pli(r',k')
    | Plis(r,k),Plis(r',k')
      ->
        match_const subs k k' >>> add_subs [Reg(sr_name r,r')]

    | Psync,Psync
    | Peieio,Peieio
    | Pisync,Pisync
    | Plwsync,Plwsync
    | Pblr,Pblr
    | Pcomment _,Pcomment _
      -> Some subs

    | Pb l,Pb l'
    | Pbl l,Pbl l'
      ->
        add_subs [Lab(l,l')] subs

    | Pbcc(c,l),Pbcc(c',l')
      when c = c' ->
        add_subs [Lab(l,l')] subs

    | Pload(s,r1,k,r2),Pload(s',r1',k',r2')
    | Pstore(s,r1,k,r2),Pstore(s',r1',k',r2')
      when s = s' ->
        match_const subs k k' >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]

    | Ploadx(s,r1,r2,r3),Ploadx(s',r1',r2',r3')
    | Plwax(s,r1,r2,r3),Plwax(s',r1',r2',r3')
    | Pstorex(s,r1,r2,r3),Pstorex(s',r1',r2',r3')
      when s = s' ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2'); Reg(sr_name r3,r3')]
	  subs

    | Pneg(s,r1,r2),Pneg(s',r1',r2')
      when s = s' ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
	  subs

    | Pnor(s,r1,r2,r3),Pnor(s',r1',r2',r3')
    | Pslw(s,r1,r2,r3),Pslw(s',r1',r2',r3')
    | Psraw(s,r1,r2,r3),Psraw(s',r1',r2',r3')
      when s = s' ->
        add_subs
          [Reg(sr_name r1,r1'); Reg(sr_name r2,r2'); Reg(sr_name r3,r3')]
	  subs

    | Psrawi(s,r1,r2,k),Psrawi(s',r1',r2',k')
      when s = s' ->
        match_const subs k k' >>>
        add_subs [Reg(sr_name r1,r1'); Reg(sr_name r2,r2')]
    | _,_ -> None


  let par2 f x y k =  f x >> fun x -> f y >! k x
  let par3 f x y z k =  f x >> fun x -> f y >> fun y -> f z >! k x y
  let par21 f x y g z k = f x >> fun x -> f y >> fun y -> g z >! k x y

  let expl_instr subs =
    let conv_reg = conv_reg subs
    and find_lab = find_lab subs
    and find_cst = find_cst subs in
   function
     | Padd(s,r1,r2,r3) ->
         par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Padd (s,r1,r2,r3))
    | Psub(s,r1,r2,r3) ->
         par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Psub (s,r1,r2,r3))
    | Psubf(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Psubf (s,r1,r2,r3))
    | Por(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Por (s,r1,r2,r3))
    | Pand(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Pand (s,r1,r2,r3))
    | Pxor(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Pxor (s,r1,r2,r3))
    | Pmull(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Pmull (s,r1,r2,r3))
    | Pdiv(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3 (fun r1 r2 r3 -> Pdiv (s,r1,r2,r3))
    | Paddi(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Paddi (r1,r2,k))
    | Paddis(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Paddis (r1,r2,k))
    | Prlwimi (r1,r2,k1,k2,k3) ->
        find_cst k1 >> fun k1 ->
        find_cst k2 >> fun k2 ->
        find_cst k3 >> fun k3 ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 -> Prlwimi (r1,r2,k1,k2,k3)
    | Prlwinm (r1,r2,k1,k2,k3) ->
        find_cst k1 >> fun k1 ->
        find_cst k2 >> fun k2 ->
        find_cst k3 >> fun k3 ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 -> Prlwinm (r1,r2,k1,k2,k3)
    | Pclrldi (r1,r2,k1) ->
        find_cst k1 >> fun k1 ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 -> Pclrldi (r1,r2,k1)
    | Pandi(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Pandi (r1,r2,k))
    | Pori(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Pori (r1,r2,k))
    | Pxori(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Pxori (r1,r2,k))
    | Pmulli(r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k (fun r1 r2 k ->  Pxori (r1,r2,k))
    | Pli(r,k) ->
        conv_reg r >> fun r -> find_cst k >! fun k -> Pli (r,k)
    | Plis(r,k) ->
        conv_reg r >> fun r -> find_cst k >! fun k -> Plis (r,k)
    | Pb l ->
        find_lab l >! fun l -> Pb l
    | Pbcc(c,l) ->
        find_lab l >! fun l -> Pbcc (c, l)
    | Pcmpwi(i,r,k) ->
        conv_reg r >> fun r -> find_cst k >! fun k -> Pcmpwi (i,r,k)
    | Pcmplwi(i,r,k) ->
        conv_reg r >> fun r -> find_cst k >! fun k -> Pcmplwi (i,r,k)
    | Pcmpw(i,r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        Pcmpw (i,r1,r2)
    | Plwzu(r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Plwzu(r1,k,r2))
    | Plwa(r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Plwa(r1,k,r2))
    | Pmr(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        Pmr(r1,r2)
    | Pextsw(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        Pextsw(r1,r2)
    | Pstwu(r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Pstwu (r1,k,r2))
    | Plwarx(r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Plwarx(r1,r2,r3))
    | Pstwcx(r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Pstwcx (r1,r2,r3))
    | Pload(s,r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Pload (s,r1,k,r2))
    | Ploadx(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Ploadx (s,r1,r2,r3))
    | Plwax(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Plwax (s,r1,r2,r3))
    | Pstore(s,r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Pstore (s,r1,k,r2))
    | Pstorex(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Pstorex (s,r1,r2,r3))
    | Psync|Peieio|Pisync| Plwsync as i
      -> unitT i
    | Pdcbf(r1,r2) ->
        conv_reg r1 >> fun r1 ->
        conv_reg r2 >! fun r2 ->
        Pdcbf(r1,r2)
    | Pnor(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Pnor (s,r1,r2,r3))
    | Pneg(s,r1,r2) ->
        par2 conv_reg r1 r2
          (fun r1 r2 -> Pneg (s,r1,r2))
    | Pslw(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 -> Pslw (s,r1,r2,r3))
    | Psrawi(s,r1,r2,k) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k ->  Psrawi (s,r1,r2,k))
    | Psraw(s,r1,r2,r3) ->
        par3 conv_reg r1 r2 r3
          (fun r1 r2 r3 ->  Psraw (s,r1,r2,r3))
    | Pnop | Pbl _ | Pblr as i -> unitT i
    | Pmtlr r ->
        conv_reg r >! fun r -> Pmtlr r
    | Pmflr r ->
        conv_reg r >! fun r -> Pmflr r
    | Pmfcr r ->
        conv_reg r >! fun r -> Pmfcr r
    | Plmw(r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Plmw (r1,k,r2))
    | Pstmw(r1,k,r2) ->
        par21 conv_reg r1 r2 find_cst k
          (fun r1 r2 k -> Pstmw (r1,k,r2))
    | Pcomment _ as i -> unitT i

end)
