(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Code

module Make(C:CompileCommon.Config) : XXXCompile_gen.S =

struct

  module X86 = X86Arch_gen
  include CompileCommon.Make(C)(X86)

(******)
  let ppo _f k = k
(******)

  open X86

  let next_reg x = alloc_reg x


  let emit_store addr v =
    I_MOV
      (Effaddr_rm32 (Rm32_abs (ParsedConstant.nameToV addr)),
       Operand_immediate v)

  let emit_store_reg addr r =
    I_MOV
      (Effaddr_rm32 (Rm32_abs (ParsedConstant.nameToV addr)),
       Operand_effaddr (Effaddr_rm32 (Rm32_reg r)))


  let emit_sta addr r v =
    [
     I_MOV
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate v) ;
     I_XCHG
       (Effaddr_rm32 (Rm32_abs (ParsedConstant.nameToV addr)),
        Effaddr_rm32 (Rm32_reg r))
   ]

  let emit_load_ins addr r =
    let addr = ParsedConstant.nameToV addr in
    I_MOV
      (Effaddr_rm32 (Rm32_reg r),
       Operand_effaddr (Effaddr_rm32 (Rm32_abs addr)))

  and emit_cmp_zero_ins r =
    I_CMP
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate 0)

  and emit_cmp_one_ins r =
    I_CMP
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate 1)

  and emit_cmp_int_ins r i =
    I_CMP
      (Effaddr_rm32 (Rm32_reg r), Operand_immediate i)

  and emit_je_ins lab = I_JCC (C_EQ,lab)

  and emit_jne_ins lab = I_JCC (C_NE,lab)

  let pseudo = List.map (fun i -> X86.Instruction i)

  let emit_load st _p init x =
    let rA,st = next_reg st in
    rA,init,pseudo [emit_load_ins x rA],st

  let emit_obs _ = emit_load

  let emit_obs_not_zero st _p init x =
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

  let emit_obs_not_eq  st =  emit_load_not st
  let emit_obs_not_value  st = emit_load_not st

  let emit_joker st init = None,init,[],st

  let emit_access st _p init e = match e.C.dir,e.C.loc with
  | None,_ -> Warn.fatal "TODO"
  | Some R,Data loc ->
      let rA,st = next_reg st in
      begin match e.C.atom with
      | None ->
          Some rA,init,pseudo [emit_load_ins loc rA],st
      | Some Atomic ->
          Warn.fatal "No atomic load for X86"
      end
  | Some W,Data loc ->
      if
        (match e.C.atom with Some Atomic -> true | None -> false)
      then
        let rX,st = next_reg st in
        None,init,pseudo (emit_sta loc rX e.C.v),
        st
      else
        None,init,pseudo [emit_store loc e.C.v],st
  | Some J,_ -> emit_joker st init
  | _,Code _ -> Warn.fatal "No code location for X86"
  | Some (D|I),_-> Warn.fatal "No DC CVAU or IC IVAU in X86"

  let emit_exch st _p init er ew =
    let rA,st = next_reg st in
    rA,init,
    pseudo  (emit_sta (Code.as_data er.C.loc) rA ew.C.v),
    st

  let emit_rmw () st p init er ew  =
    let rR,init,cs,st = emit_exch st p init er ew in
    Some rR,init,cs,st

  let emit_access_dep _st _p _init _e _r1 =
    Warn.fatal "Dependent access is irrelevant for X86"

  let emit_exch_dep _st =
    Warn.fatal "Dependent access is irrelevant for X86"

  let emit_rmw_dep () _n = emit_exch_dep

  let emit_fence st _ init _ f = match f with
    | MFence -> init,[X86.Instruction I_MFENCE],st

  let stronger_fence = MFence

  let emit_inc r = I_INC (Effaddr_rm32 (Rm32_reg r))

(* Check load *)
  let do_check_load p st r e =
    let ok,st = A.ok_reg st in
    (fun k ->
      Instruction (emit_cmp_int_ins r e.C.v)::
      Instruction (emit_jne_ins (Label.last p))::
      Instruction (emit_inc ok)::
      k),
    A.next_ok st

  let check_load  p r e init st =
    let cs,st = do_check_load p st r e in
    init,cs,st

(* Postlude *)

  let postlude =
    mk_postlude
      (fun st _p init loc r -> init,[Instruction (emit_store_reg loc r)],st)

  let get_xstore_results _ = []

  include NoInfo

end
