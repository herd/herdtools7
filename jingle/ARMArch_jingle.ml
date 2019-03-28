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

  open ARMBase

  include Arch.MakeCommon(ARMBase)


let match_instr subs pattern instr = match pattern,instr with
  | I_ADD(_,r1,r2,MetaConst.Meta m),I_ADD(_,r1',r2',i)
  | I_SUB(_,r1,r2,MetaConst.Meta m),I_SUB(_,r1',r2',i)
  | I_AND(_,r1,r2,MetaConst.Meta m),I_AND(_,r1',r2',i) ->
     Some (add_subs [Cst(m,i);
                     Reg(sr_name r1,r1');
                     Reg(sr_name r2,r2')] subs)
  | I_ADD(_,r1,r2,MetaConst.Int i),I_ADD(_,r1',r2',i')
  | I_SUB(_,r1,r2,MetaConst.Int i),I_SUB(_,r1',r2',i')
  | I_AND(_,r1,r2,MetaConst.Int i),I_AND(_,r1',r2',i') when i=i'->
     Some (add_subs [Reg(sr_name r1,r1');
                     Reg(sr_name r2,r2')] subs)
  | I_ADD3(_,r1,r2,r3),I_ADD3(_,r1',r2',r3')
  | I_SUB3(_,r1,r2,r3),I_SUB3(_,r1',r2',r3')
  | I_XOR(_,r1,r2,r3),I_XOR(_,r1',r2',r3') ->
     Some (add_subs [Reg(sr_name r1,r1');
                     Reg(sr_name r2,r2');
                     Reg(sr_name r3,r3')] subs)
  | I_B l,I_B l'
  | I_BEQ l,I_BEQ l'
  | I_BNE l,I_BNE l' ->
     Some (add_subs [Lab(l,l')] subs)
  | I_CB(b,r,l),I_CB(b',r',l') when b=b' ->
     Some (add_subs [Lab(l,l');Reg(sr_name r,r')] subs)
  | I_CMPI(r,MetaConst.Meta m),I_CMPI(r',i) ->
     Some (add_subs [Reg(sr_name r,r');Cst(m,i)] subs)
  | I_CMPI(r,MetaConst.Int i),I_CMPI(r',i') when i=i' ->
     Some (add_subs [Reg(sr_name r,r')] subs)
  | I_CMP(r1,r2),I_CMP(r1',r2')
  | I_LDREX(r1,r2),I_LDREX(r1',r2') ->
     Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)
  | I_LDR(r1,r2,c),I_LDR(r1',r2',c')
  | I_STR(r1,r2,c),I_STR(r1',r2',c')
  | I_MOV(r1,r2,c),I_MOV(r1',r2',c') when c=c' ->
     Some (add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)
  | I_LDR3(r1,r2,r3,c),I_LDR3(r1',r2',r3',c')
  | I_STR3(r1,r2,r3,c),I_STR3(r1',r2',r3',c')
  | I_STREX(r1,r2,r3,c),I_STREX(r1',r2',r3',c') when c=c' ->
     Some (add_subs [Reg(sr_name r1,r1');
                     Reg(sr_name r2,r2');
                     Reg(sr_name r3,r3')] subs)
  | I_MOVI(r,MetaConst.Meta m,c),I_MOVI(r',i,c') when c=c' ->
     Some (add_subs [Reg(sr_name r,r');Cst(m,i)] subs)
  | I_MOVI(r,MetaConst.Int i,c),I_MOVI(r',i',c') when i=i' && c=c' ->
     Some (add_subs [Reg(sr_name r,r')] subs)
  | I_DMB b,I_DMB b'
  | I_DSB b,I_DSB b' when b = b' ->
     Some subs
  | I_ISB,I_ISB -> Some subs
  | I_SADD16(r1,r2,r3),I_SADD16(r1',r2',r3')
  | I_SEL(r1,r2,r3),I_SEL(r1',r2',r3') ->
     Some (add_subs [Reg(sr_name r1,r1');
                     Reg(sr_name r2,r2');
                     Reg(sr_name r3,r3')] subs)
  | _,_ -> None


  let expl_instr subs =
    let conv_reg = conv_reg subs in
    let find_lab = find_lab subs in
    let find_cst = find_cst subs in
    function
      | I_ADD(f,r1,r2,v) ->
          conv_reg r1 >>
          fun r1 -> conv_reg r2 >>
          fun r2 -> find_cst v >!
          fun v -> I_ADD(f,r1,r2,v)
      | I_SUB(f,r1,r2,v) ->
          conv_reg r1 >>
          fun r1 -> conv_reg r2 >>
          fun r2 -> find_cst v >!
          fun v -> I_SUB(f,r1,r2,v)
      | I_AND(f,r1,r2,v) ->
          conv_reg r1 >>
          fun r1 -> conv_reg r2 >>
          fun r2 -> find_cst v >!
          fun v -> I_AND(f,r1,r2,v)
      | I_ADD3(f,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
            I_ADD3 (f,r1,r2,r3)
      | I_SUB3(f,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_SUB3 (f,r1,r2,r3)
      | I_XOR(f,r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_SUB3 (f,r1,r2,r3)
      | I_B l ->
          find_lab l >! fun l -> I_B l
      | I_BEQ l ->
          find_lab l >! fun l ->I_BEQ l
      | I_BNE l ->
          find_lab l >! fun l -> I_BNE l
      | I_CB(b,r,l) ->
          conv_reg r >> fun r ->
          find_lab l >! fun l ->
          I_CB(b,r,l)
      | I_CMPI(r,v) ->
          conv_reg r >> fun r ->
          find_cst v >! fun v ->
          I_CMPI(r,v)
      | I_CMP(r1,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          I_CMP (r1,r2)
      | I_LDREX(r1,r2) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          I_LDREX(r1,r2)
      | I_LDR(r1,r2,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          I_LDR(r1,r2,c)
      | I_STR(r1,r2,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          I_STR(r1,r2,c)
      | I_MOV(r1,r2,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >! fun r2 ->
          I_MOV(r1,r2,c)
      | I_LDR3(r1,r2,r3,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_LDR3(r1,r2,r3,c)
      | I_STR3(r1,r2,r3,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_STR3(r1,r2,r3,c)
      | I_STREX(r1,r2,r3,c) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_STREX(r1,r2,r3,c)
      | I_MOVI(r,v,c) ->
          conv_reg r >> fun r ->
          find_cst v >! fun v ->
          I_MOVI(r,v,c)
      | I_NOP
      | I_DMB _
      | I_DSB _
      | I_ISB as i
          -> unitT i
      | I_SADD16(r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_SADD16(r1,r2,r3)
      | I_SEL(r1,r2,r3) ->
          conv_reg r1 >> fun r1 ->
          conv_reg r2 >> fun r2 ->
          conv_reg r3 >! fun r3 ->
          I_SEL(r1,r2,r3)
end)
