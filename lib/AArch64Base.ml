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

type vec_reg = 
  | V0  | V1  | V2  | V3
  | V4  | V5  | V6  | V7
  | V8  | V9  | V10 | V11
  | V12 | V13 | V14 | V15
  | V16 | V17 | V18 | V19
  | V20 | V21 | V22 | V23
  | V24 | V25 | V26 | V27
  | V28 | V29 | V30 | V31

let arrange_specifier =
[
  (1,64),".1D"  ; (1,128),".1Q" ;
  (2,16),".2H"  ; (2,32),".2S" ; (2,64),".2D";
  (4,8),".4B"   ; (4,16),".4H" ; (4,32),".4S";
  (8,8),".8B"   ; (8,16),".8H" ;
  (16, 8),".16B";
  (0,8),".B"    ; (0,16),".H"  ; (0,32),".S" ; (0,64),".D";
]

type reg =
  | ZR
  | Ireg of gpr
  | Tag of gpr
  | Vreg of (vec_reg * (int * int))
  | SIMDreg of vec_reg
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

let vec_regs = 
[
  V0 ; V1 ; V2 ; V3 ;
  V4 ; V5 ; V6 ; V7 ;
  V8 ; V9 ; V10; V11;
  V12; V13; V14; V15;
  V16; V17; V18; V19;
  V20; V21; V22; V23;
  V24; V25; V26; V27;
  V28; V29; V30; V31;
]

let linkreg = Ireg R30

let cgprs =
[
 R0,"C0"  ; R1,"C1"  ; R2,"C2"  ; R3,"C3" ;
 R4,"C4"  ; R5,"C5"  ; R6,"C6"  ; R7,"C7" ;
 R8,"C8"  ; R9,"C9"  ; R10,"C10" ; R11,"C11" ;
 R12,"C12" ; R13,"C13" ; R14,"C14" ; R15,"C15" ;
 R16,"C16" ; R17,"C17" ; R18,"C18" ; R19,"C19" ;
 R20,"C20" ; R21,"C21" ; R22,"C22" ; R23,"C23" ;
 R24,"C24" ; R25,"C25" ; R26,"C26" ; R27,"C27" ;
 R28,"C28" ; R29,"C29" ; R30,"C30" ;
]

let cregs = (ZR,"CZR")::List.map (fun (r,s) -> Ireg r,s) cgprs

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

let parse_clist =
  List.map (fun (r,s) -> s,r) cregs
let vvrs =
[
 V0,"V0"  ; V1,"V1"  ; V2,"V2"  ; V3,"V3" ;
 V4,"V4"  ; V5,"V5"  ; V6,"V6"  ; V7,"V7" ;
 V8,"V8"  ; V9,"V9"  ; V10,"V10" ; V11,"V11" ;
 V12,"V12" ; V13,"V13" ; V14,"V14" ; V15,"V15" ;
 V16,"V16" ; V17,"V17" ; V18,"V18" ; V19,"V19" ;
 V20,"V20" ; V21,"V21" ; V22,"V22" ; V23,"V23" ;
 V24,"V24" ; V25,"V25" ; V26,"V26" ; V27,"V27" ;
 V28,"V28" ; V29,"V29" ; V30,"V30" ; V31, "V31";
]

let bvrs =
[
 V0,"B0"  ; V1,"B1"  ; V2,"B2"  ; V3,"B3" ;
 V4,"B4"  ; V5,"B5"  ; V6,"B6"  ; V7,"B7" ;
 V8,"B8"  ; V9,"B9"  ; V10,"B10" ; V11,"B11" ;
 V12,"B12" ; V13,"B13" ; V14,"B14" ; V15,"B15" ;
 V16,"B16" ; V17,"B17" ; V18,"B18" ; V19,"B19" ;
 V20,"B20" ; V21,"B21" ; V22,"B22" ; V23,"B23" ;
 V24,"B24" ; V25,"B25" ; V26,"B26" ; V27,"B27" ;
 V28,"B28" ; V29,"B29" ; V30,"B30" ; V31, "B31";
]

let bregs =
  List.map (fun (r,s) -> SIMDreg r,s) bvrs

let hvrs = 
[
  V0,"H0"  ; V1,"H1"  ; V2,"H2"  ; V3,"H3" ;
  V4,"H4"  ; V5,"H5"  ; V6,"H6"  ; V7,"H7" ;
  V8,"H8"  ; V9,"H9"  ; V10,"H10" ; V11,"H11" ;
  V12,"H12" ; V13,"H13" ; V14,"H14" ; V15,"H15" ;
  V16,"H16" ; V17,"H17" ; V18,"H18" ; V19,"H19" ;
  V20,"H20" ; V21,"H21" ; V22,"H22" ; V23,"H23" ;
  V24,"H24" ; V25,"H25" ; V26,"H26" ; V27,"H27" ;
  V28,"H28" ; V29,"H29" ; V30,"H30" ; V31, "H31";
]

let hregs =
  List.map (fun (r,s) -> SIMDreg r,s) hvrs

let svrs =
[
 V0,"S0"  ; V1,"S1"  ; V2,"S2"  ; V3,"S3" ;
 V4,"S4"  ; V5,"S5"  ; V6,"S6"  ; V7,"S7" ;
 V8,"S8"  ; V9,"S9"  ; V10,"S10" ; V11,"S11" ;
 V12,"S12" ; V13,"S13" ; V14,"S14" ; V15,"S15" ;
 V16,"S16" ; V17,"S17" ; V18,"S18" ; V19,"S19" ;
 V20,"S20" ; V21,"S21" ; V22,"S22" ; V23,"S23" ;
 V24,"S24" ; V25,"S25" ; V26,"S26" ; V27,"S27" ;
 V28,"S28" ; V29,"S29" ; V30,"S30" ; V31, "S31";
]

let sregs =
  List.map (fun (r,s) -> SIMDreg r,s) svrs

let dvrs =
[
 V0,"D0"  ; V1,"D1"  ; V2,"D2"  ; V3,"D3" ;
 V4,"D4"  ; V5,"D5"  ; V6,"D6"  ; V7,"D7" ;
 V8,"D8"  ; V9,"D9"  ; V10,"D10" ; V11,"D11" ;
 V12,"D12" ; V13,"D13" ; V14,"D14" ; V15,"D15" ;
 V16,"D16" ; V17,"D17" ; V18,"D18" ; V19,"D19" ;
 V20,"D20" ; V21,"D21" ; V22,"D22" ; V23,"D23" ;
 V24,"D24" ; V25,"D25" ; V26,"D26" ; V27,"D27" ;
 V28,"D28" ; V29,"D29" ; V30,"D30" ; V31, "D31";
]

let dregs =
  List.map (fun (r,s) -> SIMDreg r,s) dvrs

let qvrs =
[
 V0,"Q0"  ; V1,"Q1"  ; V2,"Q2"  ; V3,"Q3" ;
 V4,"Q4"  ; V5,"Q5"  ; V6,"Q6"  ; V7,"Q7" ;
 V8,"Q8"  ; V9,"Q9"  ; V10,"Q10" ; V11,"Q11" ;
 V12,"Q12" ; V13,"Q13" ; V14,"Q14" ; V15,"Q15" ;
 V16,"Q16" ; V17,"Q17" ; V18,"Q18" ; V19,"Q19" ;
 V20,"Q20" ; V21,"Q21" ; V22,"Q22" ; V23,"Q23" ;
 V24,"Q24" ; V25,"Q25" ; V26,"Q26" ; V27,"Q27" ;
 V28,"Q28" ; V29,"Q29" ; V30,"Q30" ; V31, "Q31";
]

let qregs =
  List.map (fun (r,s) -> SIMDreg r,s) qvrs

let parse_list rs =
  List.map (fun (r,s) -> s,r) rs

let parse_creg s =
  try Some (List.assoc (Misc.uppercase s) parse_clist)
  with Not_found -> None

let parse_xreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list regs))
  with Not_found -> None

let parse_reg s = parse_xreg s

let parse_wreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list wregs))
  with Not_found -> None

let parse_vreg s =
  try let (g1, g2) =
    ignore (Str.search_forward (Str.regexp "\\(V[0-9]+\\)\\(\\.[0-9]*[B,D,Q,H,S]\\)") (Misc.uppercase s) 0);
    (Str.matched_group 1 s, Str.matched_group 2 s);
    in Some (Vreg (List.assoc g1 (parse_list vvrs), List.assoc g2 (parse_list arrange_specifier)))
  with Not_found -> None

let parse_breg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list bregs))
  with Not_found -> None

let parse_hreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list hregs))
  with Not_found -> None

let parse_sreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list sregs))
  with Not_found -> None

let parse_dreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list dregs))
  with Not_found -> None

let parse_qreg s =
  try Some (List.assoc (Misc.uppercase s) (parse_list qregs))
  with Not_found -> None

let pp_creg r = match r with
| Symbolic_reg r -> "C%" ^ r
| Internal i -> Printf.sprintf "i%i" i
| NZP -> "NZP"
| ResAddr -> "Res"
| _ -> try List.assoc r cregs with Not_found -> assert false

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

let pp_simd_reg r = match r with
| Vreg (r',s) ->
  (try List.assoc r' vvrs with Not_found -> assert false) ^
  (try List.assoc s arrange_specifier with Not_found -> assert false)
| _ -> assert false

let pp_simd_fp_reg rl r = match r with
| SIMDreg r -> try List.assoc r rl with Not_found -> assert false
| _ -> assert false

let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg r -> Some r
  | _ -> None

let symb_reg r =  Symbolic_reg r

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
    DBGDTRRX_EL0 | DBGDTRTX_EL0 |
    FPCR | FPSR

let pp_sysreg = function
  | CTR_EL0 -> "CTR_EL0"
  | DCIZ_EL0 -> "DCIZ_EL0"
  | MDCCSR_EL0 -> "MDCCSR_EL0"
  | DBGDTR_EL0 -> "DBGDTR_EL0"
  | DBGDTRRX_EL0 -> "DBGDTRRX_EL0"
  | DBGDTRTX_EL0 -> "DBGDTRTX_EL0"
  | FPCR -> "FPCR"
  | FPSR -> "FPSR"

(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type condition = NE | EQ | GE | GT | LE | LT

let inverse_cond = function
  | NE -> EQ
  | EQ -> NE
  | LE -> GT
  | LT -> GE
  | GE -> GT
  | GT -> LE

type op = ADD | ADDS | SUB | SUBS | AND | ANDS | ORR | EOR | ASR
type gc = CFHI | GCFLGS | GCPERM | GCSEAL | GCTAG | GCTYPE | GCVALUE
type sc = CLRPERM | CTHI | SCFLGS | SCTAG | SCVALUE
type variant = V32 | V64 | V128
type simd_variant = VSIMD8 | VSIMD16 | VSIMD32 | VSIMD64 | VSIMD128

let pp_variant = function
  | V32 -> "V32"
  | V64 -> "V64"
  | V128 -> "V128"

let tr_variant = function
  | V32 -> MachSize.Word
  | V64 -> MachSize.Quad
  | V128 -> MachSize.S128


type 'k kr = K of 'k | RV of variant * reg | SIMDRV of simd_variant * reg
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

(* Inline barrel shift and extenders - need to add all variants *)
type 'k s
  = S_LSL of 'k
  | S_LSR of 'k
  | S_ASR of 'k
  | S_SXTW
  | S_UXTW
  | S_NOEXT

let pp_barrel_shift sep s pp_k = match s with
  | S_LSL(k) -> sep ^ "LSL "  ^ (pp_k k)
  | S_LSR(k) -> sep ^ "LSR "  ^ (pp_k k)
  | S_ASR(k) -> sep ^ "ASR "  ^ (pp_k k)
  | S_SXTW -> sep ^ "SXTW"
  | S_UXTW -> sep ^ "UXTW"
  | S_NOEXT  -> ""

let pp_imm n = "#" ^ string_of_int n

type 'k kinstruction =
  | I_NOP
(* Branches *)
  | I_B of lbl | I_BR of reg
  | I_BC of condition * lbl
  | I_CBZ of variant * reg * lbl
  | I_CBNZ of variant * reg * lbl
  | I_TBNZ of variant * reg * 'k * lbl
  | I_TBZ of variant * reg * 'k * lbl
  | I_BL of lbl | I_BLR of reg
  | I_RET of reg option
(* Load and Store *)
  | I_LDR of variant * reg * reg * 'k kr * 'k s
  | I_LDUR of variant * reg * reg * 'k option
(* Neon Extension Load and Store*)
  | I_LD1 of reg * int * reg * 'k kr
  | I_LD1M of reg list * reg * 'k kr
  | I_LD1R of reg * reg * 'k kr
  | I_LD2 of reg list * int * reg * 'k kr
  | I_LD2M of reg list * reg * 'k kr
  | I_LD2R of reg list * reg * 'k kr
  | I_LD3 of reg list * int * reg * 'k kr
  | I_LD3M of reg list * reg * 'k kr
  | I_LD3R of reg list * reg * 'k kr
  | I_LD4 of reg list * int * reg * 'k kr
  | I_LD4M of reg list * reg * 'k kr
  | I_LD4R of reg list * reg * 'k kr
  | I_ST1 of reg * int * reg * 'k kr
  | I_ST1M of reg list * reg * 'k kr
  | I_ST2 of reg list * int * reg * 'k kr
  | I_ST2M of reg list * reg * 'k kr
  | I_ST3 of reg list * int * reg * 'k kr
  | I_ST3M of reg list * reg * 'k kr
  | I_ST4 of reg list * int * reg * 'k kr
  | I_ST4M of reg list * reg * 'k kr
  | I_LDP_SIMD of temporal * simd_variant * reg * reg * reg * 'k kr
  | I_STP_SIMD of temporal * simd_variant * reg * reg * reg * 'k kr
(* Post-indexed load with immediate - like a writeback *)
(* sufficiently different (and semantically interesting) to need a new inst *)
  | I_LDR_P of variant * reg * reg * 'k
  | I_LDP of temporal * variant * reg * reg * reg * 'k kr
  | I_STP of temporal * variant * reg * reg * reg * 'k kr
  | I_LDAR of variant * ld_type * reg * reg
  | I_STR of variant * reg * reg * 'k kr
  | I_STLR of variant * reg * reg
  | I_STXR of variant * st_type * reg * reg * reg
(* Morello *)
  | I_ALIGND of reg * reg * 'k kr
  | I_ALIGNU of reg * reg * 'k kr
  | I_BUILD of reg * reg * reg
  | I_CHKEQ of reg * reg
  | I_CHKSLD of reg
  | I_CHKTGD of reg
  | I_CLRTAG of reg * reg
  | I_CPYTYPE of reg * reg * reg
  | I_CPYVALUE of reg * reg * reg
  | I_CSEAL of reg * reg * reg
  | I_GC of gc * reg * reg
  | I_LDCT of reg * reg
  | I_SC of sc * reg * reg * reg
  | I_SEAL of reg * reg * reg
  | I_STCT of reg * reg
  | I_UNSEAL of reg * reg * reg
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
  | I_MOVZ of variant * reg * 'k * 'k s
  | I_MOVK of variant * reg * 'k * 'k s
  | I_SXTW of reg * reg
  | I_OP3 of variant * op * reg * reg * 'k kr * 'k s
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
  | I_STZG of reg * reg * 'k kr
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
  | GT -> "GT"
  | GE -> "GE"
  | LT -> "LT"
  | LE -> "LE"

let pp_vreg v r = match v with
| V32 -> pp_wreg r
| V64 -> pp_xreg r
| V128 -> pp_creg r

let pp_vsimdreg v rs = match v with
| VSIMD8 -> List.map (pp_simd_fp_reg bvrs) rs
| VSIMD16 -> List.map (pp_simd_fp_reg hvrs) rs
| VSIMD32 -> List.map (pp_simd_fp_reg svrs) rs
| VSIMD64 -> List.map (pp_simd_fp_reg dvrs) rs
| VSIMD128 -> List.map (pp_simd_fp_reg qvrs) rs


let pp_op = function
  | ADD  -> "ADD"
  | ADDS -> "ADDS"
  | EOR  -> "EOR"
  | ORR  -> "ORR"
  | SUB  -> "SUBS"
  | SUBS -> "SUBS"
  | AND  -> "AND"
  | ANDS -> "ANDS"
  | ASR  -> "ASR"

let pp_sc = function
  | CLRPERM -> "CLRPERM"
  | CTHI -> "CTHI"
  | SCFLGS -> "SCFLGS"
  | SCTAG -> "SCTAG"
  | SCVALUE -> "SCVALUE"

let pp_gc = function
  | CFHI -> "CFHI"
  | GCFLGS -> "GCFLGS"
  | GCPERM -> "GCPERM"
  | GCSEAL -> "GCSEAL"
  | GCTAG -> "GCTAG"
  | GCTYPE -> "GCTYPE"
  | GCVALUE -> "GCVALUE"

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

  let pp_kr showsxtw showzero kr = match kr with
  | K k when m.zerop k && not showzero -> ""
  | K k -> "," ^ m.pp_k k
  | RV (v,r) ->
      "," ^ pp_vreg v r ^
      (match v with V32 when showsxtw -> ",SXTW" | V32|V64 -> "" | V128 -> assert false) in

  let pp_mem memo v rt ra kr =
    pp_memo memo ^ " " ^ pp_vreg v rt ^
    ",[" ^ pp_xreg ra ^ pp_kr true false kr ^ "]" in

  let pp_mem_shift memo v rt ra kr s =
    pp_memo memo ^ " " ^ pp_vreg v rt ^
    ",[" ^ pp_xreg ra ^ pp_kr false false kr ^
    pp_barrel_shift "," s (m.pp_k) ^ "]" in

  let pp_mem_post memo v rt ra k =
    pp_memo memo ^ " " ^ pp_vreg v rt ^
    ",[" ^ pp_xreg ra ^ "]" ^ m.pp_k k in

  let pp_memp memo v r1 r2 ra kr =
    pp_memo memo ^ " " ^
    pp_vreg v r1 ^ "," ^
    pp_vreg v r2 ^ ",[" ^
    pp_xreg ra ^ pp_kr true false kr ^ "]" in

  let pp_vmemp memo v r1 r2 ra kr = 
    pp_memo memo ^ " " ^
    String.concat ", " (pp_vsimdreg v [r1;r2]) ^ ",[" ^
    pp_xreg ra ^ pp_kr false false kr ^ "]" in

  let pp_vmem_s memo rs i r2 kr =
    pp_memo memo ^ " " ^
    "{" ^ String.concat ", " (List.map pp_simd_reg rs) ^ "}" ^
    "[" ^ string_of_int i ^ "] " ^
    pp_xreg r2 ^ pp_kr false false kr in

  let pp_vmem_r_m memo rs r2 kr =
    pp_memo memo ^ " " ^
    "{" ^ String.concat ", " (List.map pp_simd_reg rs) ^ "}" ^
    ", [" ^ pp_xreg r2 ^ "]" ^
    pp_kr false false kr in

  let pp_rkr memo v r1 kr = match v,kr with
  | _, K k -> pp_ri memo v r1 k
  | V32, RV (V32,r2)
  | V64, RV (V64,r2)
  | V128,RV (V128,r2) ->
      pp_rr memo v r1 r2
  | V32,RV ((V64|V128),_)
  | V64,RV ((V32|V128),_)
  | V128,RV ((V32|V64),_) -> assert false in

  let pp_rrkr memo v r1 r2 kr = match v,kr with
  | _,K k -> pp_rri memo v r1 r2 k
  | V32,RV (V32,r3)
  | V64,RV (V64,r3)
  | V128,RV (V128,r3) -> pp_rrr memo v r1 r2 r3
  | V64,RV (V32,_) ->
      pp_memo memo ^ " " ^
      pp_xreg r1  ^ "," ^
      pp_xreg r2 ^ pp_kr false true kr
  | V128,RV ((V32|V64),_) ->
      pp_memo memo ^ " " ^
      pp_creg r1  ^ "," ^
      pp_creg r2 ^ pp_kr false true kr
  | V32,RV (V64,_)
  | _,RV (V128,_) -> assert false in

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
  | I_TBNZ (v,r, k, lbl) ->
      sprintf "TBNZ %s,%s,%s" (pp_vreg v r) (m.pp_k k) (pp_label lbl)
  | I_TBZ (v,r, k, lbl) ->
      sprintf "TBZ %s,%s,%s" (pp_vreg v r) (m.pp_k k) (pp_label lbl)
  | I_BL lbl ->
      sprintf "BL %s" (pp_label lbl)
  | I_BLR r ->
      sprintf "BLR %s" (pp_xreg r)
  | I_RET None->
      "RET"
  | I_RET (Some r) ->
      sprintf "RET %s" (pp_xreg r)

(* Load and Store *)
  | I_LDR (v,r1,r2,k,S_NOEXT) ->
      pp_mem "LDR" v r1 r2 k
  | I_LDR (v,r1,r2,k,s) ->
      pp_mem_shift "LDR" v r1 r2 k s
  | I_LDUR (_,r1,r2,None) ->
      sprintf "LDUR %s, [%s]" (pp_reg r1) (pp_reg r2)
  | I_LDUR (_,r1,r2,Some(k)) ->
      sprintf "LDUR %s, [%s, #%s]" (pp_reg r1) (pp_reg r2) (m.pp_k k)
  | I_LDR_P (v,r1,r2,k) ->
      pp_mem_post "LDR" v r1 r2 k
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
(* Neon Extension Load and Store *)
  | I_LD1 (r1,i,r2,kr) ->
      pp_vmem_s "LD1" [r1] i r2 kr
  | I_LD1M (rs,r2,kr) ->
      pp_vmem_r_m "LD1" rs r2 kr
  | I_LD1R (r1, r2, kr) ->
      pp_vmem_r_m "LD1R" [r1] r2 kr
  | I_LD2 (rs,i,r2,kr) ->
      pp_vmem_s "LD2" rs i r2 kr
  | I_LD2M (rs,r2,kr) ->
      pp_vmem_r_m "LD2" rs r2 kr
  | I_LD2R (r1,r2,kr) ->
      pp_vmem_r_m "LD2R" r1 r2 kr
  | I_LD3 (rs,i,r2,kr) ->
      pp_vmem_s "LD3" rs i r2 kr
  | I_LD3M (rs,r2,kr) ->
      pp_vmem_r_m "LD3" rs r2 kr
  | I_LD3R (rs, r2, kr) ->
      pp_vmem_r_m "LD3R" rs r2 kr
  | I_LD4 (rs,i,r2,kr) ->
      pp_vmem_s "LD4" rs i r2 kr
  | I_LD4M (rs,r2,kr) ->
      pp_vmem_r_m "LD4" rs r2 kr
  | I_LD4R (rs,r2,kr) ->
      pp_vmem_r_m "LD4R" rs r2 kr
  | I_ST1 (r1,i,r2,kr) ->
      pp_vmem_s "ST1" [r1] i r2 kr
  | I_ST1M (rs,r2,kr) ->
      pp_vmem_r_m "ST1" rs r2 kr
  | I_ST2 (rs,i,r2,kr) ->
      pp_vmem_s "ST2" rs i r2 kr
  | I_ST2M (rs,r2,kr) ->
      pp_vmem_r_m "ST2" rs r2 kr
  | I_ST3 (rs,i,r2,kr) ->
      pp_vmem_s "ST3" rs i r2 kr
  | I_ST3M (rs,r2,kr) ->
      pp_vmem_r_m "ST3" rs r2 kr
  | I_ST4 (rs,i,r2,kr) ->
      pp_vmem_s "ST4" rs i r2 kr
  | I_ST4M (rs,r2,kr) ->
      pp_vmem_r_m "ST4" rs r2 kr
  | I_LDP_SIMD (t,v,r1,r2,r3,k) ->
      pp_vmemp (match t with TT -> "LDP" | NT -> "LDNP") v r1 r2 r3 k
  | I_STP_SIMD (t,v,r1,r2,r3,k) ->
      pp_vmemp (match t with TT -> "STP" | NT -> "STNP") v r1 r2 r3 k
(* Morello *)
  | I_ALIGND (r1,r2,k) ->
      sprintf "ALIGND %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_kr false true k)
  | I_ALIGNU (r1,r2,k) ->
      sprintf "ALIGNU %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_kr false true k)
  | I_BUILD (r1,r2,r3) ->
      sprintf "BUILD %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
  | I_CHKEQ (r1,r2) ->
      sprintf "CHKEQ %s,%s" (pp_creg r1) (pp_creg r2)
  | I_CHKSLD (r1) ->
      sprintf "CHKSLD %s" (pp_creg r1)
  | I_CHKTGD (r1) ->
      sprintf "CHKTGD %s" (pp_creg r1)
  | I_CLRTAG (r1,r2) ->
      sprintf "CLRTAG %s,%s" (pp_creg r1) (pp_creg r2)
  | I_CPYTYPE (r1,r2,r3) ->
      sprintf "CPYTYPE %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
  | I_CPYVALUE (r1,r2,r3) ->
      sprintf "CPYVALUE %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
  | I_CSEAL (r1,r2,r3) ->
      sprintf "CSEAL %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
  | I_GC (op,r1,r2) ->
      sprintf "%s %s,%s" (pp_gc op) (pp_xreg r1) (pp_creg r2)
  | I_LDCT (r1,r2) ->
      sprintf "LDCT %s,[%s]" (pp_xreg r1) (pp_xreg r2)
  | I_SC (op,r1,r2,r3) ->
      sprintf "%s %s,%s,%s" (pp_sc op) (pp_creg r1) (pp_creg r2) (pp_xreg r3)
  | I_SEAL (r1,r2,r3) ->
      sprintf "SEAL %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
  | I_STCT (r1,r2) ->
      sprintf "STCT %s,[%s]" (pp_xreg r1) (pp_xreg r2)
  | I_UNSEAL (r1,r2,r3) ->
      sprintf "UNSEAL %s,%s,%s" (pp_creg r1) (pp_creg r2) (pp_creg r3)
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
  | I_MOVZ (v,r,k,S_NOEXT) ->
      sprintf "MOVZ %s,%s" (pp_vreg v r) (m.pp_k k)
  | I_MOVZ (v,r,k,s) ->
      sprintf "MOVZ %s,%s,%s"
        (pp_vreg v r)
        (m.pp_k k)
        (pp_barrel_shift "," s (m.pp_k))
  | I_MOVK (v,r,k,S_NOEXT) ->
      sprintf "MOVK %s,%s" (pp_vreg v r) (m.pp_k k)
  | I_MOVK (v,r,k,s) ->
      sprintf "MOVK %s,%s,%s"
        (pp_vreg v r)
        (m.pp_k k)
        (pp_barrel_shift "," s (m.pp_k))
  | I_SXTW (r1,r2) ->
      sprintf "SXTW %s,%s" (pp_xreg r1) (pp_wreg r2)
  | I_OP3 (v,SUBS,ZR,r,K k, S_NOEXT) ->
      pp_ri "CMP" v r k
  | I_OP3 (v,SUBS,ZR,r2,RV (v3,r3), S_NOEXT) when v=v3->
      pp_rr "CMP" v r2 r3
  | I_OP3 (v,ANDS,ZR,r,(K _ as kr), S_NOEXT) ->
      pp_rkr "TST" v r kr
  | I_OP3 (v,op,r1,r2,K k, S_NOEXT) ->
      pp_rri (pp_op op) v r1 r2 k
  | I_OP3 (v,op,r1,r2,kr, s) ->
      pp_rrkr (pp_op op) v r1 r2 kr ^ pp_barrel_shift "," s (m.pp_k)
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
  | I_STZG (rt,rn,kr) ->
      pp_mem "STZG" V64 rt rn kr
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
  | Vreg _ -> f_regs reg y_reg,y_sreg
  | SIMDreg _ -> f_regs reg y_reg,y_sreg
  | Symbolic_reg reg ->  y_reg,f_sregs reg y_sreg
  | Internal _ | NZP | ZR | ResAddr | Tag _ -> y_reg,y_sreg in

  let fold_kr kr y = match kr with
  | K _ -> y
  | RV (_,r) -> fold_reg r y
  | SIMDRV (_,r) -> fold_reg r y in

  fun c ins -> match ins with
  | I_NOP | I_B _ | I_BC _ | I_BL _ | I_FENCE _ | I_RET None
    -> c
  | I_CBZ (_,r,_) | I_CBNZ (_,r,_) | I_BLR r | I_BR r | I_RET (Some r)
  | I_MOV (_,r,_) | I_MOVZ (_,r,_,_) | I_MOVK (_,r,_,_)
  | I_ADDR (r,_) | I_IC (_,r) | I_DC (_,r) | I_MRS (r,_)
  | I_TBNZ (_,r,_,_) | I_TBZ (_,r,_,_)
  | I_CHKSLD r | I_CHKTGD r
    -> fold_reg r c
  | I_LDAR (_,_,r1,r2) | I_STLR (_,r1,r2) | I_STLRBH (_,r1,r2)
  | I_SXTW (r1,r2) | I_LDARBH (_,_,r1,r2)
  | I_STOP (_,_,_,r1,r2) | I_STOPBH (_,_,_,r1,r2)
  | I_RBIT (_,r1,r2) | I_LDR_P (_, r1, r2, _) | I_LDUR (_, r1, r2, _)
  | I_LDG (r1,r2,_) | I_STZG (r1,r2,_) | I_STG (r1,r2,_)
  | I_CHKEQ (r1,r2) | I_CLRTAG (r1,r2) | I_GC (_,r1,r2) | I_LDCT (r1,r2)
  | I_STCT (r1,r2)
    -> fold_reg r1 (fold_reg r2 c)
  | I_LDR (_,r1,r2,kr,_) | I_STR (_,r1,r2,kr)
  | I_OP3 (_,_,r1,r2,kr,_)
  | I_LDRBH (_,r1,r2,kr) | I_STRBH (_,r1,r2,kr)
  | I_ALIGND (r1,r2,kr) | I_ALIGNU (r1,r2,kr)
  | I_LD1 (r1,_,r2,kr)
  | I_LD1R (r1,r2,kr)
    -> fold_reg r1 (fold_reg r2 (fold_kr kr c))
  | I_LD1M (rs,r2,kr)
  | I_LD2 (rs,_,r2,kr)
  | I_LD2M (rs,r2,kr)
  | I_LD2R (rs,r2,kr)
  | I_LD3 (rs,_,r2,kr)
  | I_LD3M (rs,r2,kr)
  | I_LD3R (rs,r2,kr)
  | I_LD4 (rs,_,r2,kr)
  | I_LD4M (rs,r2,kr)
  | I_LD4R (rs,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST1 (r1,_,r2,kr)
    -> fold_reg r1 (fold_reg r2 (fold_kr kr c))
  | I_ST1M (rs,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST2 (rs,_,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST2M (rs,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST3 (rs,_,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST3M (rs,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST4 (rs,_,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_ST4M (rs,r2,kr)
    -> List.fold_right fold_reg rs (fold_reg r2 (fold_kr kr c))
  | I_CSEL (_,r1,r2,r3,_,_)
  | I_STXR (_,_,r1,r2,r3) | I_STXRBH (_,_,r1,r2,r3)
  | I_BUILD (r1,r2,r3) | I_CPYTYPE (r1,r2,r3) | I_CPYVALUE (r1,r2,r3)
  | I_CSEAL (r1,r2,r3) | I_SEAL (r1,r2,r3) | I_UNSEAL (r1,r2,r3)
  | I_SC (_,r1,r2,r3)
    -> fold_reg r1 (fold_reg r2 (fold_reg r3 c))
  | I_LDP (_,_,r1,r2,r3,kr)
  | I_STP (_,_,r1,r2,r3,kr)
  | I_LDP_SIMD (_,_,r1,r2,r3,kr)
  | I_STP_SIMD (_,_,r1,r2,r3,kr)
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
  | Vreg _ -> f_reg reg
  | SIMDreg _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | Internal _ | ZR | NZP | ResAddr | Tag _-> reg in

  let map_kr kr = match kr with
  | K _ -> kr
  | RV (v,r) -> RV (v,map_reg r) 
  | SIMDRV (v,r) -> SIMDRV (v,map_reg r) in

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
  | I_TBNZ (v,r,k,lbl) ->
      I_TBNZ (v,map_reg r,k,lbl)
  | I_TBZ (v,r,k,lbl) ->
      I_TBZ (v,map_reg r,k,lbl)
  | I_BR r ->
      I_BR (map_reg r)
  | I_BLR r ->
      I_BLR (map_reg r)
  | I_RET (Some r) ->
      I_RET (Some (map_reg r))
(* Load and Store *)
  | I_LDR (v,r1,r2,kr,os) ->
     I_LDR (v,map_reg r1,map_reg r2,map_kr kr,os)
  | I_LDUR (v,r1,r2,k) ->
     I_LDUR (v,map_reg r1,map_reg r2,k)
  | I_LDR_P (v,r1,r2,k) ->
     I_LDR_P (v,map_reg r1, map_reg r2, k)
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
(* Neon Extension Loads and Stores *)
  | I_LD1 (r1,i,r2,kr) ->
      I_LD1 (map_reg r1, i, map_reg r2, map_kr kr)
  | I_LD1M (rs,r2,kr) ->
      I_LD1M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD1R (r1,r2,kr) ->
      I_LD1R (map_reg r1,map_reg r2,map_kr kr)
  | I_LD2 (rs,i,r2,kr) ->
      I_LD2 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_LD2M (rs,r2,kr) ->
      I_LD2M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD2R (rs,r2,kr) ->
      I_LD2R (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD3 (rs,i,r2,kr) ->
      I_LD3 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_LD3M (rs,r2,kr) ->
      I_LD3M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD3R (rs,r2,kr) ->
      I_LD3R (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD4 (rs,i,r2,kr) ->
      I_LD4 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_LD4M (rs,r2,kr) ->
      I_LD4M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LD4R (rs,r2,kr) ->
      I_LD4R (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_ST1 (r1,i,r2,kr) ->
      I_ST1 (map_reg r1,i,map_reg r2,map_kr kr)
  | I_ST1M (rs,r2,kr) ->
      I_ST1M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_ST2 (rs,i,r2,kr) ->
      I_ST2 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_ST2M (rs,r2,kr) ->
      I_ST2M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_ST3 (rs,i,r2,kr) ->
      I_ST3 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_ST3M (rs,r2,kr) ->
      I_ST3M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_ST4 (rs,i,r2,kr) ->
      I_ST4 (List.map map_reg rs,i,map_reg r2,map_kr kr)
  | I_ST4M (rs,r2,kr) ->
      I_ST4M (List.map map_reg rs,map_reg r2,map_kr kr)
  | I_LDP_SIMD (t,v,r1,r2,r3,kr) ->
      I_LDP_SIMD (t,v,map_reg r1,map_reg r2,map_reg r3,map_kr kr)
  | I_STP_SIMD (t,v,r1,r2,r3,kr) ->
      I_STP_SIMD (t,v,map_reg r1,map_reg r2,map_reg r3,map_kr kr)
(* Morello *)
  | I_ALIGND (r1,r2,k) ->
      I_ALIGND(map_reg r1,map_reg r2,k)
  | I_ALIGNU (r1,r2,k) ->
      I_ALIGNU(map_reg r1,map_reg r2,k)
  | I_BUILD (r1,r2,r3) ->
      I_BUILD(map_reg r1,map_reg r2,map_reg r3)
  | I_CHKEQ (r1,r2) ->
      I_CHKEQ(map_reg r1,map_reg r2)
  | I_CHKSLD (r1) ->
      I_CHKSLD(map_reg r1)
  | I_CHKTGD (r1) ->
      I_CHKTGD(map_reg r1)
  | I_CLRTAG (r1,r2) ->
      I_CLRTAG(map_reg r1,map_reg r2)
  | I_CPYTYPE (r1,r2,r3) ->
      I_CPYTYPE(map_reg r1,map_reg r2,map_reg r3)
  | I_CPYVALUE (r1,r2,r3) ->
      I_CPYVALUE(map_reg r1,map_reg r2,map_reg r3)
  | I_CSEAL (r1,r2,r3) ->
      I_CSEAL(map_reg r1,map_reg r2,map_reg r3)
  | I_GC (op,r1,r2) ->
      I_GC(op,map_reg r1,map_reg r2)
  | I_LDCT (r1,r2) ->
      I_LDCT(map_reg r1,map_reg r2)
  | I_SC (op,r1,r2,r3) ->
      I_SC(op,map_reg r1,map_reg r2,map_reg r3)
  | I_SEAL (r1,r2,r3) ->
      I_SEAL(map_reg r1,map_reg r2,map_reg r3)
  | I_STCT (r1,r2) ->
      I_STCT(map_reg r1,map_reg r2)
  | I_UNSEAL (r1,r2,r3) ->
      I_UNSEAL(map_reg r1,map_reg r2,map_reg r3)
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
  | I_MOVZ (v,r,k,s) ->
      I_MOVZ (v,map_reg r,k,s)
  | I_MOVK (v,r,k,s) ->
      I_MOVK (v,map_reg r,k,s)
  | I_SXTW (r1,r2) ->
      I_SXTW (map_reg r1,map_reg r2)
  | I_OP3 (v,op,r1,r2,kr,os) ->
      I_OP3 (v,op,map_reg r1,map_reg r2,map_kr kr,os)
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
  | I_STZG (r1,r2,k) ->
      I_STZG (map_reg r1,map_reg r2,k)
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
  | I_TBNZ (_,_,_,lbl)
  | I_TBZ (_,_,_,lbl)
  | I_BL lbl
    -> [Label.Next; Label.To lbl;]
  | I_BLR _|I_BR _|I_RET _ -> [Label.Any]
  | I_NOP
  | I_LDR _
  | I_LDUR _
  | I_LDR_P _
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
  | I_MOVZ _
  | I_MOVK _
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
  | I_STG _| I_STZG _|I_LDG _
  | I_ALIGND _| I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|I_CLRTAG _
  | I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SC _|I_SEAL _|I_STCT _
  | I_UNSEAL _
  | I_LD1 _ | I_LD1M _ | I_LD1R _
  | I_LD2 _ | I_LD2M _ | I_LD2R _
  | I_LD3 _ | I_LD3M _ | I_LD3R _
  | I_LD4 _ | I_LD4M _ | I_LD4R _
  | I_ST1 _ | I_ST1M _
  | I_ST2 _ | I_ST2M _
  | I_ST3 _ | I_ST3M _
  | I_ST4 _ | I_ST4M _
  | I_LDP_SIMD _
  | I_STP_SIMD _
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
        | SIMDRV _ as kr -> kr

      let ap_shift f s = match s with
        | S_LSL(s) -> S_LSL(f s)
        | S_LSR(s) -> S_LSR(f s)
        | S_ASR(s) -> S_ASR(f s)
        | S_SXTW -> S_SXTW
        | S_UXTW -> S_UXTW
        | S_NOEXT  -> S_NOEXT

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
        | I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|I_CLRTAG _|I_CPYTYPE _
        | I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SC _|I_SEAL _|I_STCT _
        | I_UNSEAL _
            as keep -> keep
        | I_LDR (v,r1,r2,kr,s) -> I_LDR (v,r1,r2,kr_tr kr,ap_shift k_tr s)
        | I_LDUR (v,r1,r2,None) -> I_LDUR (v,r1,r2,None)
        | I_LDUR (v,r1,r2,Some(k)) -> I_LDUR (v,r1,r2,Some(k_tr k))
        | I_LDR_P (v,r1,r2,k) -> I_LDR_P (v,r1,r2,k_tr k)
        | I_LDP (t,v,r1,r2,r3,kr) -> I_LDP (t,v,r1,r2,r3,kr_tr kr)
        | I_STP (t,v,r1,r2,r3,kr) -> I_STP (t,v,r1,r2,r3,kr_tr kr)
        | I_STR (v,r1,r2,kr) -> I_STR (v,r1,r2,kr_tr kr)
        | I_STG (r1,r2,kr) -> I_STG (r1,r2,kr_tr kr)
        | I_STZG (r1,r2,kr) -> I_STZG (r1,r2,kr_tr kr)
        | I_LDG (r1,r2,kr) -> I_LDG (r1,r2,kr_tr kr)
        | I_LDRBH (v,r1,r2,kr) -> I_LDRBH (v,r1,r2,kr_tr kr)
        | I_STRBH (v,r1,r2,kr) -> I_STRBH (v,r1,r2,kr_tr kr)
        | I_TBNZ (v,r1,k,lbl) -> I_TBNZ (v,r1,k_tr k, lbl)
        | I_TBZ (v,r1,k,lbl) -> I_TBZ (v,r1,k_tr k, lbl)
        | I_MOV (v,r,k) -> I_MOV (v,r,kr_tr k)
        | I_MOVZ (v,r,k,s) -> I_MOVZ (v,r,k_tr k,ap_shift k_tr s)
        | I_MOVK (v,r,k,s) -> I_MOVK (v,r,k_tr k,ap_shift k_tr s)
        | I_OP3 (v,op,r1,r2,kr,s) -> I_OP3 (v,op,r1,r2,kr_tr kr,ap_shift k_tr s)
        | I_ALIGND (r1,r2,k) -> I_ALIGND (r1,r2,kr_tr k)
        | I_ALIGNU (r1,r2,k) -> I_ALIGNU (r1,r2,kr_tr k)
        | I_LD1 (r1,i,r2,kr) -> I_LD1 (r1,i,r2,kr_tr kr)
        | I_LD1M (rs,r2,kr) -> I_LD1M (rs,r2,kr_tr kr)
        | I_LD1R (r1,r2,kr) -> I_LD1R (r1,r2,kr_tr kr)
        | I_LD2 (rs,i,r2,kr) -> I_LD2 (rs,i,r2,kr_tr kr)
        | I_LD2M (rs,r2,kr) -> I_LD2M (rs,r2,kr_tr kr)
        | I_LD2R (rs,r2,kr) -> I_LD2R (rs,r2,kr_tr kr)
        | I_LD3 (rs,i,r2,kr) -> I_LD3 (rs,i,r2,kr_tr kr)
        | I_LD3M (rs,r2,kr) -> I_LD3M (rs,r2,kr_tr kr)
        | I_LD3R (rs,r2,kr) -> I_LD3R (rs,r2,kr_tr kr)
        | I_LD4 (rs,i,r2,kr) -> I_LD4 (rs,i,r2,kr_tr kr)
        | I_LD4M (rs,r2,kr) -> I_LD4M (rs,r2,kr_tr kr)
        | I_LD4R (rs,r2,kr) -> I_LD4R (rs,r2,kr_tr kr)
        | I_ST1 (r1,i,r2,kr) -> I_ST1 (r1,i,r2,kr_tr kr)
        | I_ST1M (rs,r2,kr) -> I_ST1M (rs,r2,kr_tr kr)
        | I_ST2 (rs,i,r2,kr) -> I_ST2 (rs,i,r2,kr_tr kr)
        | I_ST2M (rs,r2,kr) -> I_ST2M (rs,r2,kr_tr kr)
        | I_ST3 (rs,i,r2,kr) -> I_ST3 (rs,i,r2,kr_tr kr)
        | I_ST3M (rs,r2,kr) -> I_ST3M (rs,r2,kr_tr kr)
        | I_ST4 (rs,i,r2,kr) -> I_ST4 (rs,i,r2,kr_tr kr)
        | I_ST4M (rs,r2,kr) -> I_ST4M (rs,r2,kr_tr kr)
        | I_LDP_SIMD (t,v,r1,r2,r3,kr) -> I_LDP_SIMD (t,v,r1,r2,r3,kr_tr kr)
        | I_STP_SIMD (t,v,r1,r2,r3,kr) -> I_STP_SIMD (t,v,r1,r2,r3,kr_tr kr)


      let get_naccesses = function
        | I_LDR _ | I_LDAR _ | I_LDARBH _ | I_LDUR _
        | I_STR _ | I_STLR _ | I_STLRBH _ | I_STXR _
        | I_LDRBH _ | I_STRBH _ | I_STXRBH _ | I_IC _ | I_DC _
        | I_STG _ | I_LDG _
        | I_LD1 _ | I_LD1M _ | I_LD1R _
        | I_LD2 _ | I_LD2M _ | I_LD2R _
        | I_LD3 _ | I_LD3M _ | I_LD3R _
        | I_LD4 _ | I_LD4M _ | I_LD4R _
        | I_ST1 _ | I_ST1M _
        | I_ST2 _ | I_ST2M _
        | I_ST3 _ | I_ST3M _
        | I_ST4 _ | I_ST4M _
          -> 1
        | I_LDP _|I_STP _
        | I_CAS _ | I_CASBH _
        | I_SWP _ | I_SWPBH _
        | I_LDOP _ | I_LDOPBH _
        | I_STOP _ | I_STOPBH _
        | I_STZG _
        | I_LDP_SIMD _
        | I_STP_SIMD _
          -> 2
        | I_LDR_P _ (* reads, stores, then post-index stores *)
          -> 3
        | I_LDCT _ | I_STCT _
          -> 4
        | I_NOP
        | I_B _ | I_BR _
        | I_BL _ | I_BLR _
        | I_RET _
        | I_BC _
        | I_CBZ _
        | I_CBNZ _
        | I_TBZ _
        | I_TBNZ _
        | I_MOV _
        | I_MOVZ _
        | I_MOVK _
        | I_SXTW _
        | I_OP3 _
        | I_FENCE _
        | I_CSEL _
        | I_ADDR _
        | I_RBIT _
        | I_MRS _
        | I_ALIGND _| I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _
        | I_CLRTAG _|I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_SC _|I_SEAL _
        | I_UNSEAL _
          -> 0

      let fold_labels k f = function
        | I_B lbl
        | I_BC (_,lbl)
        | I_CBZ (_,_,lbl)
        | I_CBNZ (_,_,lbl)
        | I_TBNZ (_,_,_,lbl)
        | I_TBZ (_,_,_,lbl)
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
        | I_TBNZ (v,r,k,lbl) -> I_TBNZ (v,r,k,f lbl)
        | I_TBZ (v,r,k,lbl) -> I_TBZ (v,r,k,f lbl)
        | I_ADDR (r,lbl) -> I_ADDR (r, f lbl)
        | ins -> ins
    end)

let get_macro _name = raise Not_found

let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4
