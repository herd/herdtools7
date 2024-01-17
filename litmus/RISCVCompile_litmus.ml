(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(V:Constant.S)(C:Arch_litmus.Config) =
  struct
    module A = RISCVArch_litmus.Make(C)(V)
    open A
    open A.Out
    open Printf

    let is_ret _ = false
    and is_nop = function
      | A.INop -> true
      | _ -> false

    let branch lbl = J lbl

(* No addresses in code *)
    let extract_addrs _ins = Global_litmus.Set.empty
    let stable_regs _ins = A.RegSet.empty

(************************)
(* Template compilation *)
(************************)

(* Operations *)
    let tr_reg fmt r = match r with
    | A.Ireg X0 -> "x0",[]
    | _ -> fmt,[r]

    let tr_2regs fmt1 fmt2 r1 r2 = match r1 with
    | A.Ireg X0 ->
        let fmt2,r2 = tr_reg fmt1 r2 in
        "x0",fmt2,[],r2
    | r ->
        let fmt2,r2 = tr_reg fmt2 r2 in
        fmt1,fmt2,[r],r2

    let tr_1i = tr_reg "^i0"
    let tr_1o = tr_reg "^o0"
    let tr_2i = tr_2regs "^i0" "^i1"

    open CType

    let w2type = function
      | A.Byte -> byte
      | A.Half -> half
      | A.Word -> word
      | A.Double -> quad

    let add_type t = List.map (fun x -> x,t)

    let op2regsI memo r1 r2 k =
      let fmt1,r1 = tr_1o r1
      and fmt2,r2 = tr_1i r2 in
      { empty_ins with
        memo = sprintf "%s %s,%s,%i" memo fmt1 fmt2 k ;
        inputs=r2; outputs=r1; }

    let lui r1 k =
      let fmt1,r1 = tr_1o r1 in
      { empty_ins with
        memo = sprintf "lui %s,%i" fmt1 k ;
        outputs=r1; }

    let op1regsA memo r1 lbl =
      let fmt1,r1 = tr_1o r1 in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo fmt1 lbl ;
        inputs=[]; outputs=r1; }

    let op3regs memo r1 r2 r3 =
      let fmt1,r1 = tr_1o r1
      and fmt2,fmt3,r2,r3 = tr_2i r2 r3 in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s" memo fmt1 fmt2 fmt3;
        inputs=r2@r3; outputs=r1; }

    let ext memo r1 r2 =
      let fmt1,r1 = tr_1o r1
      and fmt2,r2 = tr_1i r2 in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo fmt1 fmt2 ;
        inputs=r2; outputs=r1; }

    let emit_loop _ins = assert false

    include Handler.No(struct type ins = A.Out.ins end)


    let compile_ins tr_lab ins k = match ins with
    | A.INop -> { empty_ins with memo="nop"; }::k
    | A.Ret -> { empty_ins with memo="ret"; }::k
    | A.OpI2 (A.LUI,r1,i) ->
        lui r1 i::k
    | A.OpI (op,r1,r2,i) ->
        op2regsI (A.pp_opi op) r1 r2 i::k
    | A.OpA (op,r1,lbl) ->
        op1regsA (A.pp_opa op) r1 lbl::k
    | OpIW (op,r1,r2,i) ->
        op2regsI (A.pp_opiw op) r1 r2 i::k
    | Op (op,r1,r2,r3) ->
        op3regs (A.pp_op op) r1 r2 r3::k
    | OpW (op,r1,r2,r3) ->
        op3regs (A.pp_opw op) r1 r2 r3::k
    | Ext (_,_,r1,r2) ->
        ext "sext.w" r1 r2::k
    | J lbl ->
        { empty_ins with
          memo = sprintf "j %s" (A.Out.dump_label (tr_lab lbl));
          branch=[Branch lbl;] }::k
  | Bcc (cond,r1,r2,lbl) ->
      let fmt1,fmt2,r1,r2 = tr_2i r1 r2 in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s" (A.pp_bcc cond) fmt1 fmt2
          (A.Out.dump_label (tr_lab lbl)) ;
        inputs=r1@r2; branch=[Next;Branch lbl;]; }::k
  | Load (w,s,mo,r1,o,r2) ->
      let fmt1,r1 = tr_1o r1
      and fmt2,r2 = tr_1i r2 in
      let t = w2type w in
      { empty_ins with
        memo = sprintf "%s %s,%i(%s)" (A.pp_load w s mo) fmt1 o fmt2;
        inputs=r2; outputs=r1;
        reg_env=add_type t r1@add_type voidstar r2; }::k
  | Store (w,mo,r1,o,r2) ->
      let fmt1,fmt2,r1,r2 = tr_2i r1 r2 in
      let t = w2type w in
      { empty_ins with
        memo = sprintf "%s %s,%i(%s)" (A.pp_store w mo) fmt1 o fmt2;
        inputs=r1@r2;
        reg_env=add_type t r1@add_type voidstar r2; }::k
  | LoadReserve (w,mo,r1,r2) ->
      let fmt1,r1 = tr_1o r1
      and fmt2,r2 = tr_1i r2 in
      let t = w2type w in
      { empty_ins with
        memo = sprintf "%s %s,0(%s)" (A.pp_lr w mo) fmt1 fmt2;
        inputs=r2; outputs=r1;
        reg_env=add_type t r1@add_type voidstar r2}::k
  | StoreConditional (w,mo,r1,r2,r3) ->
      let fmt1,r1 = tr_1o r1
      and fmt2,fmt3,r2,r3 = tr_2i r2 r3 in
      { empty_ins with
        memo =  sprintf "%s %s,%s,0(%s)" (A.pp_sc w mo) fmt1 fmt2 fmt3;
        outputs=r1; inputs=r2@r3;
        reg_env=add_type word r1@add_type (w2type w) r2@add_type voidstar r3;
      }::k
  | AUIPC (_,_) -> Warn.fatal "auipc not Supported in litmus"
  | Amo (op,w,mo,r1,r2,r3) ->
      let fmt1,r1 = tr_1o r1
      and fmt2,fmt3,r2,r3 = tr_2i r2 r3 in
      let t = w2type w in
      { empty_ins with
        memo =  sprintf "%s %s,%s,(%s)" (A.pp_amo op w mo) fmt1 fmt2 fmt3;
        outputs=r1; inputs=r2@r3;
        reg_env = add_type t (r1@r2@r2);}::k
  | FenceIns f ->
      { empty_ins with memo = pp_barrier f;}::k



  end
