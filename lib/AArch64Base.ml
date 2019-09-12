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

(** Simplified AArch64, for generators *)
open Printf

let arch = Archs.aarch64
let endian = Endian.Little
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type gpr =
  | R0  | R1  | R2  | R3
  | R4  | R5  | R6  | R7
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15
  | R16 | R17 | R18 | R19
  | R20 | R21 | R22 | R23
  | R24 | R25 | R26 | R27
  | R28 | R29 | R30

type reg =
  | ZR
  | Ireg of gpr
  | Symbolic_reg of string
  | Internal of int
  | NZP
  | ResAddr

let gprs =
[
  R0 ; R1 ; R2 ; R3 ;
  R4 ; R5 ; R6 ; R7 ;
  R8 ; R9 ; R10; R11 ;
  R12; R13; R14; R15 ;
  R16; R17; R18; R19 ;
  R20; R21; R22; R23 ;
  R24; R25; R26; R27 ;
  R28; R29; R30 ;
]

let linkreg = Ireg R30

let xgprs =
[
 R0,"X0"  ; R1,"X1"  ; R2,"X2"  ; R3,"X3" ;
 R4,"X4"  ; R5,"X5"  ; R6,"X6"  ; R7,"X7" ;
 R8,"X8"  ; R9,"X9"  ; R10,"X10" ; R11,"X11" ;
 R12,"X12" ; R13,"X13" ; R14,"X14" ; R15,"X15" ;
 R16,"X16" ; R17,"X17" ; R18,"X18" ; R19,"X19" ;
 R20,"X20" ; R21,"X21" ; R22,"X22" ; R23,"X23" ;
 R24,"X24" ; R25,"X25" ; R26,"X26" ; R27,"X27" ;
 R28,"X28" ; R29,"X29" ; R30,"X30" ;
]

let xregs = (ZR,"XZR")::List.map (fun (r,s) -> Ireg r,s) xgprs

let regs = xregs

let wgprs =
[
 R0,"W0"  ; R1,"W1"  ; R2,"W2"  ; R3,"W3" ;
 R4,"W4"  ; R5,"W5"  ; R6,"W6"  ; R7,"W7" ;
 R8,"W8"  ; R9,"W9"  ; R10,"W10" ; R11,"W11" ;
 R12,"W12" ; R13,"W13" ; R14,"W14" ; R15,"W15" ;
 R16,"W16" ; R17,"W17" ; R18,"W18" ; R19,"W19" ;
 R20,"W20" ; R21,"W21" ; R22,"W22" ; R23,"W23" ;
 R24,"W24" ; R25,"W25" ; R26,"W26" ; R27,"W27" ;
 R28,"W28" ; R29,"W29" ; R30,"W30" ;
]

let wregs =
  (ZR,"WZR")::List.map (fun (r,s) -> Ireg r,s) wgprs

let parse_list =
  List.map (fun (r,s) -> s,r) regs

let parse_wlist =
  List.map (fun (r,s) -> s,r) wregs

let parse_xreg s =
  try Some (List.assoc (Misc.uppercase s) parse_list)
  with Not_found -> None

let parse_reg s = parse_xreg s

let parse_wreg s =
  try Some (List.assoc (Misc.uppercase s) parse_wlist)
  with Not_found -> None

let pp_xreg r = match r with
| Symbolic_reg r -> "X%" ^ r
| Internal i -> Printf.sprintf "i%i" i
| NZP -> "NZP"
| ResAddr -> "Res"
| _ -> try List.assoc r regs with Not_found -> assert false

let pp_reg = pp_xreg

let pp_wreg r = match r with
| Symbolic_reg r -> "W%" ^ r
| Internal i -> Printf.sprintf "i%i" i
| NZP -> "NZP"
| ResAddr -> "Res"
| _ -> try List.assoc r wregs with Not_found -> assert false


let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg r -> Some r
  | _ -> None

let symb_reg r =  Symbolic_reg r
let typeof c = assert false

(************)
(* Barriers *)
(************)

type mBReqDomain = NSH | ISH | OSH | SY

let fold_domain f k =
  let k = f SY k in
  let k = f OSH k in
  let k = f ISH k in
  let k = f NSH k in
  k

let pp_domain = function
  | NSH -> "NSH"
  | ISH -> "ISH"
  | OSH -> "OSH"
  | SY -> "SY"

type mBReqTypes = LD | ST | FULL

let fold_type f k =
  let k = f FULL k in
  let k = f ST k in
  let k = f LD k in
  k

let pp_type = function
  | LD -> "LD"
  | ST -> "ST"
  | FULL -> ""


type barrier =
  | DMB of mBReqDomain*mBReqTypes
  | DSB of mBReqDomain*mBReqTypes
  | ISB

let fold_barrier_option more f k =
  if more then
    fold_domain
      (fun d k ->
        fold_type (fun t k -> f d t k) k)
      k
  else
    fold_type (fun t k -> f SY t k) k

let do_fold_dmb_dsb more f k =
  let k =
    fold_barrier_option more
      (fun d t k -> f (DMB (d,t)) k)
      k in
  if more then
    let k =
      fold_barrier_option more
        (fun d t k -> f (DSB (d,t)) k)
        k in
    k
  else k

let fold_barrier more f k =
  let k = do_fold_dmb_dsb more f k in
  let k = f ISB k in
  k

let pp_option d t = match d,t with
| SY,FULL    -> pp_domain d
| SY,(LD|ST) -> pp_type t
| _,_ -> pp_domain d ^ pp_type t

let do_pp_barrier tag b = match b with
  | DMB (d,t) -> "DMB" ^ tag ^ pp_option d t
  | DSB (d,t) -> "DSB" ^ tag ^ pp_option d t
  | ISB -> "ISB"

let pp_barrier b = do_pp_barrier " " b
let pp_barrier_dot b = do_pp_barrier "." b

let barrier_compare = compare

(*********************)
(* Cache maintenance *)
(*********************)

module IC = struct
  type funct = I
  let pp_funct = function I -> "I"

  type typ = ALL | VA
  let pp_typ = function | ALL -> "ALL" | VA -> "VA"

  type point = U
  let pp_point = function U -> "U"

  type domain = IS | NO
  let pp_domain = function IS -> "IS" | NO -> ""

  type op = { funct:funct; typ:typ; point:point; domain:domain; }
  let ivau = { funct=I; typ=VA; point=U; domain=NO; }

  let pp_op op =
    pp_funct op.funct ^
    pp_typ op.typ ^
    pp_point op.point ^
    pp_domain op.domain
end

module DC = struct
  type funct = I | C | CI | Z
  let pp_funct = function
    | I -> "I"
    | C -> "C"
    | CI -> "CI"
    | Z -> "Z"

  type typ = VA | SW
  let pp_typ = function VA -> "VA" | SW -> "SW"

  type point = CO | U
  let pp_point = function CO -> "C" | U -> "U"

  type op = { funct:funct; typ:typ; point:point; }

  let cvau = { funct=C; typ=VA; point=U; }
  let civac = { funct=CI; typ=VA; point=CO; }

  let pp_op op =
    pp_funct op.funct ^
    pp_typ op.typ ^
    pp_point op.point
end

(********************)
(* System registers *)
(*  (Some of...)    *)
(********************)

type sysreg =
    CTR_EL0 | DCIZ_EL0 |
    MDCCSR_EL0 | DBGDTR_EL0 |
    DBGDTRRX_EL0 | DBGDTRTX_EL0

let pp_sysreg = function
  | CTR_EL0 -> "CTR_EL0"
  | DCIZ_EL0 -> "DCIZ_EL0"
  | MDCCSR_EL0 -> "MDCCSR_EL0"
  | DBGDTR_EL0 -> "DBGDTR_EL0"
  | DBGDTRRX_EL0 -> "DBGDTRRX_EL0"
  | DBGDTRTX_EL0 -> "DBGDTRTX_EL0"

(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type condition = NE | EQ

let inverse_cond = function
  | NE -> EQ
  | EQ -> NE

type op = ADD | ADDS | SUB | SUBS | AND | ANDS | ORR | EOR
type variant = V32 | V64

let tr_variant = function
  | V32 -> MachSize.Word
  | V64 -> MachSize.Quad


type 'k kr = K of 'k | RV of variant * reg
let k0 = K 0

type ld_type = AA | XX | AX | AQ

let ldr_memo = function
  | AA -> "LDAR"
  | XX -> "LDXR"
  | AX -> "LDAXR"
  | AQ -> "LDAPR"

type st_type = YY | LY

let str_memo = function
  | YY -> "STXR"
  | LY -> "STLXR"

type rmw_type = RMW_P | RMW_A | RMW_L | RMW_AL

type w_type = W_P | W_L

let w_to_rmw = function
  | W_P -> RMW_P
  | W_L -> RMW_L

let rmw_memo = function
  | RMW_P -> ""
  | RMW_A -> "A"
  | RMW_L -> "L"
  | RMW_AL -> "AL"

let w_memo = function
  | W_P -> ""
  | W_L -> "L"

let cas_memo rmw = sprintf "CAS%s" (rmw_memo rmw)
and swp_memo rmw = sprintf "SWP%s" (rmw_memo rmw)

type atomic_op = A_ADD | A_EOR | A_SET | A_CLR | A_SMAX | A_SMIN
let pp_aop = function
  | A_ADD -> "ADD"
  | A_EOR -> "EOR"
  | A_SET -> "SET"
  | A_CLR -> "CLR"
  | A_SMAX -> "SMAX"
  | A_SMIN -> "SMIN"

let ldop_memo op rmw = sprintf "LD%s%s" (pp_aop op) (rmw_memo rmw)
and stop_memo op w = sprintf "ST%s%s" (pp_aop op) (w_memo w)

type bh = B | H (* Byte or Halfword *)

let pp_bh = function
  | B -> "B"
  | H -> "H"

let bh_to_sz = function
  | B -> MachSize.Byte
  | H -> MachSize.Short

let casbh_memo bh rmw = sprintf "%s%s" (cas_memo rmw) (pp_bh bh)
and swpbh_memo bh rmw = sprintf "%s%s" (swp_memo rmw) (pp_bh bh)
and ldopbh_memo op bh rmw = sprintf "%s%s" (ldop_memo op rmw) (pp_bh bh)
and stopbh_memo op bh  rmw = sprintf "%s%s" (stop_memo op rmw) (pp_bh bh)
and ldrbh_memo bh t =  sprintf "%s%s" (ldr_memo t) (pp_bh bh)
and strbh_memo bh t =  sprintf "%s%s" (str_memo t) (pp_bh bh)

type temporal = TT | NT
type opsel = Cpy | Inc | Inv | Neg

let sel_memo = function
  | Cpy -> "CSEL"
  | Inc -> "CSINC"
  | Inv -> "CSINV"
  | Neg -> "CSNEG"

type 'k kinstruction =
  | I_NOP
(* Branches *)
  | I_B of lbl | I_BR of reg
  | I_BC of condition * lbl
  | I_CBZ of variant * reg * lbl
  | I_CBNZ of variant * reg * lbl
  | I_BL of lbl | I_BLR of reg
  | I_RET of reg option
(* Load and Store *)
  | I_LDR of variant * reg * reg * 'k kr
  | I_LDP of temporal * variant * reg * reg * reg * 'k kr
  | I_STP of temporal * variant * reg * reg * reg * 'k kr
  | I_LDAR of variant * ld_type * reg * reg
  | I_STR of variant * reg * reg * 'k kr
  | I_STLR of variant * reg * reg
  | I_STXR of variant * st_type * reg * reg * reg
(* Idem for bytes and half words *)
  | I_LDRBH of bh * reg * reg * 'k kr
  | I_LDARBH of bh * ld_type * reg * reg
  | I_STRBH of bh * reg * reg * 'k kr
  | I_STLRBH of bh * reg * reg
  | I_STXRBH of bh * st_type * reg * reg * reg
(* CAS *)
  | I_CAS of variant * rmw_type * reg * reg * reg
  | I_CASBH of bh * rmw_type  * reg * reg * reg
(* SWP *)
  | I_SWP of variant * rmw_type * reg * reg * reg
  | I_SWPBH of bh * rmw_type  * reg * reg * reg
(* Fetch and op *)
  | I_LDOP of  atomic_op * variant * rmw_type  * reg * reg * reg
  | I_LDOPBH of atomic_op * bh * rmw_type  * reg * reg * reg
  | I_STOP of  atomic_op * variant * w_type * reg * reg
  | I_STOPBH of  atomic_op * bh * w_type  * reg * reg
(* Operations *)
  | I_MOV of variant * reg * 'k kr
  | I_SXTW of reg * reg
  | I_OP3 of variant * op * reg * reg * 'k kr
  | I_ADDR of reg * lbl
  | I_RBIT of variant * reg * reg
(* Barrier *)
  | I_FENCE of barrier
(* Conditional select *)
  | I_CSEL of variant * reg *reg * reg * condition * opsel
(* Cache maintenance *)
  | I_IC of IC.op * reg
  | I_DC of DC.op * reg
(* Read system register *)
  | I_MRS of reg * sysreg
(* Memory Tagging *)
  | I_STG of reg * reg * 'k kr
  | I_LDG of reg * reg * 'k kr

type instruction = int kinstruction
type parsedInstruction = MetaConst.k kinstruction

let pp_label i = i

open PPMode

let pp_hash m = match m with
| Ascii | Dot -> "#"
| Latex -> "\\#"
| DotFig -> "\\\\#"

let pp_k m v = pp_hash m ^ string_of_int v

type 'k basic_pp = { pp_k : 'k -> string; zerop : 'k -> bool; k0 : 'k kr }


let pp_memo memo = memo

let pp_cond = function
  | NE -> "NE"
  | EQ -> "EQ"

let pp_vreg v r = match v with
| V32 -> pp_wreg r
| V64 -> pp_xreg r


let pp_op = function
  | ADD -> "ADD"
  | ADDS -> "ADDS"
  | EOR -> "EOR"
  | ORR -> "ORR"
  | SUB -> "SUBS"
  | SUBS -> "SUBS"
  | AND  -> "AND"
  | ANDS  -> "ANDS"

let do_pp_instruction m =
  let pp_rrr memo v rt rn rm =
    pp_memo memo ^ " " ^ pp_vreg v rt ^ "," ^
    pp_vreg v rn ^ "," ^ pp_vreg v  rm
  and pp_rri memo v rt rn k =
    pp_memo memo ^ " " ^ pp_vreg v rt ^ "," ^
    pp_vreg v rn ^ "," ^ m.pp_k k
  and pp_ri memo v r k =
    pp_memo memo ^ " " ^ pp_vreg v r ^ "," ^  m.pp_k k
  and pp_rr memo v r1 r2 =
    pp_memo memo ^ " " ^ pp_vreg v r1 ^ "," ^  pp_vreg v r2 in

  let pp_kr showzero kr = match kr with
  | K k when m.zerop k && not showzero -> ""
  | K k -> "," ^ m.pp_k k
  | RV (v,r) ->
      "," ^ pp_vreg v r ^
      (match v with V32 -> ",SXTW" | V64 -> "") in

  let pp_mem memo v rt ra kr =
    pp_memo memo ^ " " ^ pp_vreg v rt ^
    ",[" ^ pp_xreg ra ^ pp_kr false kr ^ "]" in

  let pp_memp memo v r1 r2 ra kr =
    pp_memo memo ^ " " ^
    pp_vreg v r1 ^ "," ^
    pp_vreg v r2 ^ ",[" ^
    pp_xreg ra ^ pp_kr false kr ^ "]" in

  let pp_rkr memo v r1 kr = match v,kr with
  | _, K k -> pp_ri memo v r1 k
  | V32, RV (V32,r2)
  | V64, RV (V64,r2)  ->
      pp_rr memo v r1 r2
  | V32,RV (V64,_)
  | V64,RV (V32,_) -> assert false in

  let pp_rrkr memo v r1 r2 kr = match v,kr with
  | _,K k -> pp_rri memo v r1 r2 k
  | V32,RV (V32,r3)
  | V64,RV (V64,r3) -> pp_rrr memo v r1 r2 r3
  | V64,RV (V32,_) ->
      pp_memo memo ^ " " ^
      pp_xreg r1  ^ "," ^
      pp_xreg r2 ^ pp_kr true kr
  | V32,RV (V64,_) -> assert false in

  let pp_stxr memo v r1 r2 r3 =
    pp_memo memo ^ " " ^
    pp_wreg r1 ^"," ^
    pp_vreg v r2 ^ ",[" ^
    pp_xreg r3 ^ "]" in

  fun i -> match i with
  | I_NOP -> "NOP"
(* Branches *)
  | I_B lbl ->
      sprintf "B %s" (pp_label lbl)
  | I_BR r ->
      sprintf "BR %s" (pp_xreg r)
  | I_BC (cond,lbl) ->
      sprintf "B.%s %s" (pp_cond cond) (pp_label lbl)
  | I_CBZ (v,r,lbl) ->
      sprintf "CBZ %s,%s" (pp_vreg v r) (pp_label lbl)
  | I_CBNZ (v,r,lbl) ->
      sprintf "CBNZ %s,%s" (pp_vreg v r) (pp_label lbl)
  | I_BL lbl ->
      sprintf "BL %s" (pp_label lbl)
  | I_BLR r ->
      sprintf "BLR %s" (pp_xreg r)
  | I_RET None->
      "RET"
  | I_RET (Some r) ->
      sprintf "RET %s" (pp_xreg r)

(* Load and Store *)
  | I_LDR (v,r1,r2,k) ->
      pp_mem "LDR" v r1 r2 k
  | I_LDP (t,v,r1,r2,r3,k) ->
      pp_memp (match t with TT -> "LDP" | NT -> "LDNP") v r1 r2 r3 k
  | I_STP (t,v,r1,r2,r3,k) ->
      pp_memp (match t with TT -> "STP" | NT -> "STNP") v r1 r2 r3 k
  | I_LDAR (v,t,r1,r2) ->
      pp_mem (ldr_memo t) v r1 r2 m.k0
  | I_LDARBH (bh,t,r1,r2) ->
      pp_mem (ldrbh_memo bh t)  V32 r1 r2 m.k0
  | I_STR (v,r1,r2,k) ->
      pp_mem "STR" v r1 r2 k
  | I_STLR (v,r1,r2) ->
      pp_mem "STLR" v r1 r2 m.k0
  | I_STXR (v,t,r1,r2,r3) ->
      pp_stxr (str_memo t) v r1 r2 r3
  | I_LDRBH (bh,r1,r2,k) ->
      pp_mem ("LDR"^pp_bh bh) V32 r1 r2 k
  | I_STRBH (bh,r1,r2,k) ->
      pp_mem ("STR"^pp_bh bh) V32 r1 r2 k
  | I_STLRBH (bh,r1,r2) ->
      pp_mem ("STLR"^pp_bh bh) V32 r1 r2 m.k0
  | I_STXRBH (bh,t,r1,r2,r3) ->
      pp_stxr (strbh_memo bh t) V32 r1 r2 r3
(* CAS *)
  | I_CAS (v,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]" (cas_memo rmw) (pp_vreg v r1) (pp_vreg v r2) (pp_xreg r3)
  | I_CASBH (bh,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]" (casbh_memo bh rmw) (pp_wreg r1) (pp_wreg r2) (pp_xreg r3)
(* SWP *)
  | I_SWP (v,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]" (swp_memo rmw) (pp_vreg v r1) (pp_vreg v r2) (pp_xreg r3)
  | I_SWPBH (bh,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]" (swpbh_memo bh rmw) (pp_wreg r1) (pp_wreg r2) (pp_xreg r3)
(* Fecth and Op *)
  | I_LDOP (op,v,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]"
        (ldop_memo op rmw) (pp_vreg v r1) (pp_vreg v r2) (pp_xreg r3)
  | I_LDOPBH (op,v,rmw,r1,r2,r3) ->
      sprintf "%s %s,%s,[%s]"
        (ldopbh_memo op v rmw) (pp_wreg r1) (pp_wreg r2) (pp_xreg r3)
  | I_STOP (op,v,rmw,r1,r2) ->
      sprintf "%s %s,[%s]"
        (stop_memo op rmw) (pp_vreg v r1) (pp_xreg r2)
  | I_STOPBH (op,v,rmw,r1,r2) ->
      sprintf "%s %s,[%s]"
        (stopbh_memo op v rmw) (pp_wreg r1) (pp_xreg r2)
(* Operations *)
  | I_MOV (v,r,kr) ->
      pp_rkr "MOV" v r kr
  | I_SXTW (r1,r2) ->
      sprintf "SXTW %s,%s" (pp_xreg r1) (pp_wreg r2)
  | I_OP3 (v,SUBS,ZR,r,K k) ->
      pp_ri "CMP" v r k
  | I_OP3 (v,SUBS,ZR,r2,RV (v3,r3)) when v=v3->
      pp_rr "CMP" v r2 r3
  | I_OP3 (v,ANDS,ZR,r,(K _ as kr)) ->
      pp_rkr "TST" v r kr
  | I_OP3 (v,op,r1,r2,K k) ->
      pp_rri (pp_op op) v r1 r2 k
  | I_OP3 (v,op,r1,r2,kr) ->
      pp_rrkr (pp_op op) v r1 r2 kr
  | I_ADDR (r,lbl) ->
      sprintf "ADDR %s,%s" (pp_xreg r) (pp_label lbl)
  | I_RBIT (v,rd,rs) ->
      sprintf "RBIT %s,%s" (pp_vreg v rd) (pp_vreg v rs)
(* Barrier *)
  | I_FENCE b ->
      pp_barrier b
(* Conditional select *)
  | I_CSEL (v,r1,ZR,ZR,c,Inc) ->
      sprintf "CSET %s,%s" (pp_vreg v r1)  (pp_cond (inverse_cond c))
  | I_CSEL (v,r1,r2,r3,c,op) ->
      pp_rrr (sel_memo op) v r1 r2 r3 ^ "," ^ pp_cond c
(* Cache maintenance *)
  | I_IC (op,r) ->
      sprintf "IC %s,%s" (IC.pp_op op) (pp_xreg r)
  | I_DC (op,r) ->
      sprintf "DC %s,%s" (DC.pp_op op) (pp_xreg r)
(* Read System register *)
  | I_MRS (r,sr) ->
      sprintf "MRS %s,%s" (pp_xreg r) (pp_sysreg sr)
(* Memory Tagging *)
  | I_STG (rt,rn,kr) ->
      pp_mem "STG" V64 rt rn kr
  | I_LDG (rt,rn,kr) ->
      pp_mem "LDG" V64 rt rn kr

let m_int = { pp_k = string_of_int ;
              zerop = (function 0 -> true | _ -> false);
              k0 = k0; }

let pp_instruction m =
  do_pp_instruction {m_int with pp_k = pp_k m; }

let dump_instruction =
  do_pp_instruction
    {m_int with pp_k = (fun v -> "#" ^ string_of_int v); }

let dump_parsedInstruction =
  do_pp_instruction
    {  pp_k = MetaConst.pp_prefix "#";
       zerop = (fun k -> MetaConst.compare MetaConst.zero k = 0);
       k0 = K MetaConst.zero; }

(****************************)
(* Symbolic registers stuff *)
(****************************)

let all_gprs =  List.map (fun r -> Ireg r) gprs

let allowed_for_symb = List.filter (fun r -> r <> linkreg) all_gprs

let fold_regs (f_regs,f_sregs) =

  let fold_reg reg (y_reg,y_sreg) = match reg with
  | Ireg _ -> f_regs reg y_reg,y_sreg
  | Symbolic_reg reg ->  y_reg,f_sregs reg y_sreg
  | Internal _ | NZP | ZR | ResAddr -> y_reg,y_sreg in

  let fold_kr kr y = match kr with
  | K _ -> y
  | RV (_,r) -> fold_reg r y in

  fun c ins -> match ins with
  | I_NOP | I_B _ | I_BC _ | I_BL _ | I_FENCE _ | I_RET None
    -> c
  | I_CBZ (_,r,_) | I_CBNZ (_,r,_) | I_BLR r | I_BR r | I_RET (Some r)
  | I_MOV (_,r,_) | I_ADDR (r,_) | I_IC (_,r) | I_DC (_,r) | I_MRS (r,_)
    -> fold_reg r c
  | I_LDAR (_,_,r1,r2) | I_STLR (_,r1,r2) | I_STLRBH (_,r1,r2)
  | I_SXTW (r1,r2) | I_LDARBH (_,_,r1,r2)
  | I_STOP (_,_,_,r1,r2) | I_STOPBH (_,_,_,r1,r2)
  | I_RBIT (_,r1,r2)
  | I_LDG (r1,r2,_) | I_STG (r1,r2,_)
    -> fold_reg r1 (fold_reg r2 c)
  | I_LDR (_,r1,r2,kr) | I_STR (_,r1,r2,kr)
  | I_OP3 (_,_,r1,r2,kr)
  | I_LDRBH (_,r1,r2,kr) | I_STRBH (_,r1,r2,kr)
    -> fold_reg r1 (fold_reg r2 (fold_kr kr c))
  | I_CSEL (_,r1,r2,r3,_,_)
  | I_STXR (_,_,r1,r2,r3) | I_STXRBH (_,_,r1,r2,r3)
    -> fold_reg r1 (fold_reg r2 (fold_reg r3 c))
  | I_LDP (_,_,r1,r2,r3,kr)
  | I_STP (_,_,r1,r2,r3,kr)
    -> fold_reg r1 (fold_reg r2 (fold_reg r3 (fold_kr kr c)))
  | I_CAS (_,_,r1,r2,r3)
  | I_CASBH (_,_,r1,r2,r3)
  | I_SWP (_,_,r1,r2,r3)
  | I_SWPBH (_,_,r1,r2,r3)
  | I_LDOP (_,_,_,r1,r2,r3)
  | I_LDOPBH (_,_,_,r1,r2,r3)
    -> fold_reg r1 (fold_reg r2 (fold_reg r3 c))


let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | Ireg _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | Internal _ | ZR | NZP | ResAddr -> reg in

  let map_kr kr = match kr with
  | K _ -> kr
  | RV (v,r) -> RV (v,map_reg r) in

  fun ins -> match ins with
  | I_NOP
(* Branches *)
  | I_B _
  | I_BC _
  | I_FENCE _
  | I_BL _
  | I_RET None
    -> ins
  | I_CBZ (v,r,lbl) ->
      I_CBZ (v,map_reg r,lbl)
  | I_CBNZ (v,r,lbl) ->
      I_CBNZ (v,map_reg r,lbl)
  | I_BR r ->
      I_BR (map_reg r)
  | I_BLR r ->
      I_BLR (map_reg r)
  | I_RET (Some r) ->
      I_RET (Some (map_reg r))
(* Load and Store *)
  | I_LDR (v,r1,r2,kr) ->
     I_LDR (v,map_reg r1,map_reg r2,map_kr kr)
  | I_LDP (t,v,r1,r2,r3,kr) ->
     I_LDP (t,v,map_reg r1,map_reg r2,map_reg r3,map_kr kr)
  | I_STP (t,v,r1,r2,r3,kr) ->
     I_STP (t,v,map_reg r1,map_reg r2,map_reg r3,map_kr kr)
  | I_LDAR (v,t,r1,r2) ->
     I_LDAR (v,t,map_reg r1,map_reg r2)
  | I_LDARBH (bh,t,r1,r2) ->
     I_LDARBH (bh,t,map_reg r1,map_reg r2)
  | I_STR (v,r1,r2,k) ->
      I_STR (v,map_reg r1,map_reg r2,k)
  | I_STLR (v,r1,r2) ->
      I_STLR (v,map_reg r1,map_reg r2)
  | I_STLRBH (v,r1,r2) ->
      I_STLRBH (v,map_reg r1,map_reg r2)
  | I_STXR (v,t,r1,r2,r3) ->
      I_STXR (v,t,map_reg r1,map_reg r2,map_reg r3)
  | I_STXRBH (bh,t,r1,r2,r3) ->
      I_STXRBH (bh,t,map_reg r1,map_reg r2,map_reg r3)
(* Byte and Half loads and stores *)
  | I_LDRBH (v,r1,r2,kr) ->
     I_LDRBH (v,map_reg r1,map_reg r2,map_kr kr)
  | I_STRBH (v,r1,r2,kr) ->
     I_STRBH (v,map_reg r1,map_reg r2,map_kr kr)
(* CAS *)
  | I_CAS (v,rmw,r1,r2,r3) ->
      I_CAS (v,rmw,map_reg r1,map_reg r2,map_reg r3)
  | I_CASBH (bh,rmw,r1,r2,r3) ->
      I_CASBH (bh,rmw,map_reg r1,map_reg r2,map_reg r3)
(* SWP *)
  | I_SWP (v,rmw,r1,r2,r3) ->
      I_SWP (v,rmw,map_reg r1,map_reg r2,map_reg r3)
  | I_SWPBH (bh,rmw,r1,r2,r3) ->
      I_SWPBH (bh,rmw,map_reg r1,map_reg r2,map_reg r3)
(* Fetch and Op *)
  | I_LDOP (op,v,rmw,r1,r2,r3) ->
      I_LDOP (op,v,rmw,map_reg r1,map_reg r2,map_reg r3)
  | I_LDOPBH (op,v,rmw,r1,r2,r3) ->
      I_LDOPBH (op,v,rmw,map_reg r1,map_reg r2,map_reg r3)
  | I_STOP (op,v,rmw,r1,r2) ->
      I_STOP (op,v,rmw,map_reg r1,map_reg r2)
  | I_STOPBH (op,v,rmw,r1,r2) ->
      I_STOPBH (op,v,rmw,map_reg r1,map_reg r2)
(* Operations *)
  | I_MOV (v,r,k) ->
      I_MOV (v,map_reg r,k)
  | I_SXTW (r1,r2) ->
      I_SXTW (map_reg r1,map_reg r2)
  | I_OP3 (v,op,r1,r2,kr) ->
      I_OP3 (v,op,map_reg r1,map_reg r2,map_kr kr)
  | I_ADDR (r,lbl) ->
      I_ADDR (map_reg r,lbl)
  | I_RBIT (v,r1,r2) ->
      I_RBIT (v,map_reg r1,map_reg r2)
(* Conditinal select *)
  | I_CSEL (v,r1,r2,r3,c,op) ->
      I_CSEL (v,map_reg r1,map_reg r2,map_reg r3,c,op)
(* Cache maintenance *)
  | I_IC (op,r) ->
      I_IC (op,map_reg r)
  | I_DC (op,r) ->
      I_DC (op,map_reg r)
(* Read system register *)
  | I_MRS (r,sr) ->
      I_MRS (map_reg r,sr)
(* Memory Tagging *)
  | I_STG (r1,r2,k) ->
      I_STG (map_reg r1,map_reg r2,k)
  | I_LDG (r1,r2,k) ->
      I_LDG (map_reg r1,map_reg r2,k)

(* No addresses burried in ARM code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

let norm_ins ins = ins

(* PLDI submission, complete later *)
let is_data _ _ = assert false

(* Instruction continuation *)
let get_next = function
  | I_B lbl -> [Label.To lbl;]
  | I_BC (_,lbl)
  | I_CBZ (_,_,lbl)
  | I_CBNZ (_,_,lbl)
  | I_BL lbl
    -> [Label.Next; Label.To lbl;]
  | I_BLR _|I_BR _|I_RET _ -> [Label.Any]
  | I_NOP
  | I_LDR _
  | I_LDP _
  | I_STP _
  | I_STR _
  | I_LDAR _
  | I_LDARBH _
  | I_STLR _
  | I_STLRBH _
  | I_STXR _
  | I_STXRBH _
  | I_LDRBH _
  | I_STRBH _
  | I_MOV _
  | I_SXTW _
  | I_OP3 _
  | I_FENCE _
  | I_CSEL _
  | I_CAS _
  | I_CASBH _
  | I_SWP _
  | I_SWPBH _
  | I_LDOP _
  | I_LDOPBH _
  | I_STOP _
  | I_STOPBH _
  | I_ADDR _
  | I_RBIT _
  | I_IC _
  | I_DC _
  | I_MRS _
  | I_STG _|I_LDG _
    -> [Label.Next;]

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let k_tr = MetaConst.as_int
      let kr_tr = function
        | K i -> K (k_tr i)
        | RV _ as kr -> kr

      let parsed_tr i = match i with
        | I_NOP
        | I_B _
        | I_BR _
        | I_BC _
        | I_CBZ _
        | I_CBNZ _
        | I_BL _
        | I_BLR _
        | I_RET _
        | I_LDAR _
        | I_LDARBH _
        | I_STLR _
        | I_STLRBH _
        | I_STXR _
        | I_STXRBH _
        | I_SXTW _
        | I_FENCE _
        | I_CSEL _
        | I_CAS _
        | I_CASBH _
        | I_SWP _
        | I_SWPBH _
        | I_LDOP _
        | I_LDOPBH _
        | I_STOP _
        | I_STOPBH _
        | I_ADDR _
        | I_RBIT _
        | I_IC _
        | I_DC _
        | I_MRS _
            as keep -> keep
        | I_LDR (v,r1,r2,kr) -> I_LDR (v,r1,r2,kr_tr kr)
        | I_LDP (t,v,r1,r2,r3,kr) -> I_LDP (t,v,r1,r2,r3,kr_tr kr)
        | I_STP (t,v,r1,r2,r3,kr) -> I_STP (t,v,r1,r2,r3,kr_tr kr)
        | I_STR (v,r1,r2,kr) -> I_STR (v,r1,r2,kr_tr kr)
        | I_STG (r1,r2,kr) -> I_STG (r1,r2,kr_tr kr)
        | I_LDG (r1,r2,kr) -> I_LDG (r1,r2,kr_tr kr)
        | I_LDRBH (v,r1,r2,kr) -> I_LDRBH (v,r1,r2,kr_tr kr)
        | I_STRBH (v,r1,r2,kr) -> I_STRBH (v,r1,r2,kr_tr kr)
        | I_MOV (v,r,k) -> I_MOV (v,r,kr_tr k)
        | I_OP3 (v,op,r1,r2,kr) -> I_OP3 (v,op,r1,r2,kr_tr kr)


      let get_naccesses = function
        | I_LDR _ | I_LDAR _ | I_LDARBH _
        | I_STR _ | I_STLR _ | I_STLRBH _ | I_STXR _
        | I_LDRBH _ | I_STRBH _ | I_STXRBH _ | I_IC _ | I_DC _
        | I_STG _ | I_LDG _
          -> 1
        | I_LDP _|I_STP _
        | I_CAS _ | I_CASBH _
        | I_SWP _ | I_SWPBH _
        | I_LDOP _ | I_LDOPBH _
        | I_STOP _ | I_STOPBH _
          -> 2
        | I_NOP
        | I_B _ | I_BR _
        | I_BL _ | I_BLR _
        | I_RET _
        | I_BC _
        | I_CBZ _
        | I_CBNZ _
        | I_MOV _
        | I_SXTW _
        | I_OP3 _
        | I_FENCE _
        | I_CSEL _
        | I_ADDR _
        | I_RBIT _
        | I_MRS _
          -> 0

      let fold_labels k f = function
        | I_B lbl
        | I_BC (_,lbl)
        | I_CBZ (_,_,lbl)
        | I_CBNZ (_,_,lbl)
        | I_BL lbl
        | I_ADDR (_,lbl)
          -> f k lbl
        | _ -> k

      let map_labels f = function
        | I_B lbl -> I_B (f lbl)
        | I_BL lbl -> I_BL (f lbl)
        | I_BC (c,lbl) -> I_BC (c,f lbl)
        | I_CBZ (v,r,lbl) -> I_CBZ (v,r,f lbl)
        | I_CBNZ (v,r,lbl) -> I_CBNZ (v,r,f lbl)
        | I_ADDR (r,lbl) -> I_ADDR (r, f lbl)
        | ins -> ins
    end)

let get_macro _name = raise Not_found

let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4
