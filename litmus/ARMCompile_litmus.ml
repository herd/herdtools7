(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  include Arch_litmus.Config
  val morearch : MoreArch.t
end

module Make(V:Constant.S)(C:Config) =
  struct
    module A = ARMArch_litmus.Make(C)(V)
    open A
    open A.Out
    open Printf

    let is_ret _ = assert false

(* No addresses in code *)
    let extract_addrs _ins = Global_litmus.Set.empty

    let stable_regs _ins = A.RegSet.empty

(************************)
(* Template compilation *)
(************************)
    let pp_cond = function
      | AL -> ""
      | EQ -> "eq"
      | NE -> "ne"

    let is_cond = function
      | AL -> false
      |  _ -> true

    let pp_setflags = function
      | SetFlags -> "s"
      | DontSetFlags -> ""

(* Arithmetic *)
    let op3regs memo s c rD rA rB =
      let memo =
        sprintf "%s%s%s"
          memo (pp_setflags s) (pp_cond c) in
      { empty_ins with
        memo=memo^ " ^o0,^i0,^i1";
        inputs=[rA; rB];
        outputs=[rD]; cond=is_cond c; }

    let op2regsI memo s c rD rA i =
      let memo =
        sprintf "%s%s%s"
          memo (pp_setflags s) (pp_cond c) in
      { empty_ins with
        memo= sprintf "%s ^o0,^i0,#%i" memo i;
        inputs=[rA];
        outputs=[rD]; cond=is_cond c; }

(* Moves *)
    let movi c r1 i =
      let memo = sprintf "%s%s" "mov" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^o0,#%i" memo i ;
        inputs = [] ;
        outputs = [r1]; cond=is_cond c; }

    let mov c r1 r2 =
      let memo = sprintf "%s%s" "mov" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^o0,^i0" memo ;
        inputs = [r2] ;
        outputs = [r1]; cond=is_cond c; }


(* Memory *)
    let ldr2 c r1 r2 =
      let memo = sprintf "%s%s" "ldr" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^o0,[^i0]" memo ;
        inputs = [r2] ;
        outputs = [r1] ; cond=is_cond c; }

    let ldrex r1 r2 =
      let memo = "ldrex" in
      { empty_ins with
        memo = sprintf "%s ^o0,[^i0]" memo ;
        inputs = [r2] ;
        outputs = [r1]; }

    let ldr3 c r1 r2 r3 =
      let memo = sprintf "%s%s" "ldr" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^o0,[^i0,^i1]" memo ;
        inputs = [r2;r3] ;
        outputs = [r1] ; cond=is_cond c; }

    let str2 c r1 r2 =
      let memo = sprintf "%s%s" "str" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^i0,[^i1]" memo ;
        inputs = [r1;r2] ;
        outputs = [] ; cond=is_cond c; }

    let str3 c r1 r2 r3 =
      let memo = sprintf "%s%s" "str" (pp_cond c) in
      { empty_ins with
        memo = sprintf "%s ^i0,[^i1,^i2]" memo ;
        inputs = [r1;r2;r3] ;
        outputs = [] ; cond=is_cond c; }

    let strex  c r1 r2 r3 =
      let memo = sprintf "%s%s" "strex" (pp_cond c) in
       { empty_ins with
        memo = sprintf "%s ^o0,^i0,[^i1]" memo ;
        inputs = [r2;r3] ;
        outputs = [r1] ; cond=is_cond c; }

(* cmp & branches *)
    let cmpi r1 k =
      { empty_ins with
        memo = sprintf "cmp ^i0,#%i" k ;
        inputs = [r1;] ;
        outputs = [] ;
      }

    let cmp r1 r2 =
      { empty_ins with
        memo = "cmp ^i0,^i1" ;
        inputs = [r1;r2;] ;
        outputs = [] ;
      }

    let b tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

    let bcc tr_lab cond lbl =
      { empty_ins with
        memo = sprintf "b%s %s"
          (pp_cond cond) (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next; Branch lbl] ; }

    let cb tr_lab n r lbl =
      { empty_ins with
        memo = sprintf "cb%sz ^i0,%s"
          (if n then "n" else "") (A.Out.dump_label (tr_lab lbl)) ;
        inputs = [r;] ;
        branch=[Next; Branch lbl] ; }
    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }

    let decr r i =
      { empty_ins with
        memo = sprintf "subs ^o0,^i0,#%i" i ;
        inputs = [r;] ; outputs = [r;] ; }

    let no_tr lbl = lbl

    let emit_loop k =
      let lbl1 = Label.next_label "L" in
      let lbl2 = Label.next_label "L" in
      cmpi loop_idx 0::
      b no_tr lbl2::
      emit_lbl lbl1::
      k@
      [ decr loop_idx 1;
        emit_lbl lbl2 ;
        bcc no_tr NE lbl1; ]

(* Barriers *)
    let emit_opt = function
      | SY -> "sy"
      | ST -> "st"
      | ISH -> "ish"
      | ISHST -> "ishst"
      | NSH -> "nsh"
      | NSHST -> "nshst"
      | OSH -> "osh"
      | OSHST -> "oshst"

    let check_armv6k i =
      match C.morearch with
      | MoreArch.ARMv6K ->
          Warn.fatal "Non supported instruction (ARMv6K) %s"
            (A.dump_instruction i)
      | _ -> ()

    let emit_barrier memo o =
      let memo = sprintf "%s %s" memo (emit_opt o) in
      { empty_ins with memo =memo; }

    let compile_ins tr_lab ins k = match ins with
    | I_NOP -> { empty_ins with memo = "nop"; }::k
(* Arithmetic *)
    | I_ADD (s,r1, r2, i) ->  op2regsI "add" s AL r1 r2 i::k
    | I_SUB (s,r1, r2, i) ->  op2regsI "sub" s AL r1 r2 i::k
    | I_AND (s,r1, r2, i) ->  op2regsI "and" s AL r1 r2 i::k
    | I_ADD3 (s,r1, r2, r3) ->  op3regs "add" s AL r1 r2 r3::k
    | I_SADD16 (r1, r2, r3) ->  op3regs "sadd16" DontSetFlags AL r1 r2 r3::k
    | I_SEL (r1, r2, r3) ->  op3regs "sel" DontSetFlags AL r1 r2 r3::k
    | I_SUB3 (s,r1, r2, r3) ->  op3regs "sub" s AL r1 r2 r3::k
    | I_XOR (s,r1, r2, r3) -> op3regs "eor" s AL r1 r2 r3::k
(* Moves *)
    | I_MOVI (r, i, c) -> movi c r i::k
    | I_MOV (r1,r2, c) -> mov c r1 r2::k
(* Memory *)
    | I_LDR (r1, r2, c) ->  ldr2 c r1 r2::k
    | I_LDREX (r1, r2) ->  ldrex r1 r2::k
    | I_LDR3 (r1, r2, r3, c) ->  ldr3 c r1 r2 r3::k
    | I_STR (r1, r2, c) ->  str2 c r1 r2::k
    | I_STR3 (r1, r2, r3, c) ->  str3 c r1 r2 r3::k
    | I_STREX (r1, r2, r3, c) ->  strex c r1 r2 r3::k
(* Comparisons and branches *)
    | I_CMPI (r1, i) -> cmpi r1 i::k
    | I_CMP (r1, r2)-> cmp r1 r2::k
    | I_B lbl -> b tr_lab lbl::k
    | I_BNE lbl -> bcc tr_lab NE lbl::k
    | I_BEQ lbl -> bcc tr_lab EQ lbl::k
    | I_CB (n,r,lbl) -> cb tr_lab n r lbl::k
(* Misc *)
    | I_DMB o ->
        check_armv6k ins ;
        emit_barrier "dmb" o::k
    | I_DSB o ->
        check_armv6k ins ;
        emit_barrier "dsb" o::k
    | I_ISB ->
        check_armv6k ins ;
        { empty_ins with memo = "isb"; }::k

    let branch_neq r i lab k = cmpi r i::bcc no_tr NE lab::k
    let branch_eq r i lab k = cmpi r i::bcc no_tr EQ lab::k

    let signaling_write _i _k = Warn.fatal "no signaling write for ARM"

    let emit_tb_wait _ = Warn.fatal "no time base for ARM"
  end
