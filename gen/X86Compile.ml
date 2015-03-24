(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code

module Make(C:CompileCommon.Config) : XXXCompile.S =

struct
  let do_sta = !Config.sta

  module X86 = X86Arch
  include CompileCommon.Make(C)(X86)

(******)
  let ppo _f k = k
(******)

  open X86

  let next_reg x = alloc_reg x

  let emit_store addr v =
    I_MOV
      (Effaddr_rm32 (Rm32_abs (SymbConstant.nameToV addr)),
       Operand_immediate v)


  let emit_sta addr r v =
    [
     I_MOV
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate v) ;
     I_XCHG
       (Effaddr_rm32 (Rm32_abs (SymbConstant.nameToV addr)),
        Effaddr_rm32 (Rm32_reg r))
   ]

  let emit_load_ins addr r =
    let addr = SymbConstant.nameToV addr in
    I_MOV
      (Effaddr_rm32 (Rm32_reg r),
       Operand_effaddr (Effaddr_rm32 (Rm32_abs addr)))

  and emit_cmp_zero_ins r =
    I_CMP
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate 0)

  and emit_cmp_one_ins r =
    I_CMP
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate 1)

  and emit_je_ins lab = I_JCC (C_EQ,lab)

  and emit_jne_ins lab = I_JCC (C_NE,lab)

  let pseudo = List.map (fun i -> X86.Instruction i)

  let emit_load st _p init x =
    let rA,st = next_reg st in
    rA,init,pseudo [emit_load_ins x rA],st

  let emit_load_not_zero st _p init x =
    let rA,st = next_reg st in
    let lab = Label.next_label "L" in
    rA,init,
    Label (lab,Nop)::
    pseudo
      [emit_load_ins x rA ;
       emit_cmp_zero_ins rA ;
       emit_je_ins lab],
    st

  let emit_load_one st _p init x =
    let rA,st = next_reg st in
    let lab = Label.next_label "L" in
    rA,init,
    Label (lab,Nop)::
    pseudo
      [emit_load_ins x rA ;
       emit_cmp_one_ins rA ;
       emit_jne_ins lab],
    st

  let emit_load_not  _st _p _init _x _cmp =
    Warn.fatal "Loop observers not implemented for X86"

  let emit_load_not_eq  st =  emit_load_not st
  let emit_load_not_value  st = emit_load_not st

  let emit_fno  _st _p _init _x =  Warn.fatal "FNO is irrelevant for X86"

  let emit_fno2 = emit_fno

  let emit_open_fno = emit_fno
  and emit_close_fno _st _p _init _lab _r _x =
    Warn.fatal "FNO is irrelevant for X86"


  let emit_access st _p init e = match e.C.dir with
  |R ->
      let rA,st = next_reg st in
      begin match e.C.atom with
      | None ->
          Some rA,init,pseudo [emit_load_ins e.C.loc rA],st
      | Some Atomic ->
          Warn.fatal "No atomic load for X86"
      end
  |W ->
      if
        do_sta ||
        (match e.C.atom with Some Atomic -> true | None -> false)
      then
        let rX,st = next_reg st in
        None,init,pseudo (emit_sta e.C.loc rX e.C.v),
        st
      else
        None,init,pseudo [emit_store e.C.loc e.C.v],st


  let emit_exch st _p init er ew =
    let rA,st = next_reg st in
    rA,init,
    pseudo  (emit_sta er.C.loc rA ew.C.v),
    st
(*
  let emit_access_dep st p init e r1 =
    let r2,st = next_reg st in
    let init = (Reg (p,r2),e.loc)::init in
    let r3,st = next_reg st in
    let c =
      [emit_move r3 r1 ;
       emit_xor r3 r3 ;
       emit_add r2 r3 ;] in
    match e.dir with
  | R ->
      let rA,st = next_reg st in
      Some rA,init,c@[emit_load_ind r2 rA],st
  | W ->
      None,init,c@[emit_store_ind r2 e.v],st
*)

  let emit_access_dep _st _p _init _e _r1 =
    Warn.fatal "Dependent access is irrelevant for X86"

  let emit_exch_dep _st =
    Warn.fatal "Dependent access is irrelevant for X86"

  let emit_fence = function
    | MFence -> X86.Instruction I_MFENCE

  let stronger_fence = MFence

  let postlude st _p init cs = init,cs,st
        
end
