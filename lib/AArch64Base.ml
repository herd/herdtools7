(**************************************************************************)
(*                                  DIY                                   *)
(*                                                                        *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.          *)
(* Shaked Flur, Susmit Sarkar, Peter Sewell, University of Cambridge, UK. *)
(*                                                                        *)
(*  Copyright 2015 Institut National de Recherche en Informatique et en   *)
(*  Automatique and the authors. All rights reserved.                     *)
(*  This file is distributed  under the terms of the Lesser GNU General   *)
(*  Public License.                                                       *)
(**************************************************************************)

(** Simplified AArch64, for generators *)
open Printf

let arch = Archs.aarch64

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

let xregs =
  (ZR,"XZR")::List.map (fun (r,s) -> Ireg r,s) xgprs

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
  try Some (List.assoc s parse_list)
  with Not_found -> None

let parse_reg s = parse_xreg s

let parse_wreg s =
  try Some (List.assoc s parse_wlist)
  with Not_found -> None

let pp_xreg r = match r with
| Symbolic_reg r -> "X%" ^ r
| Internal i -> Printf.sprintf "i%i" i
| _ -> try List.assoc r regs with Not_found -> assert false

let pp_reg = pp_xreg

let pp_wreg r = match r with
| Symbolic_reg r -> "W%" ^ r
| Internal i -> Printf.sprintf "i%i" i
| _ -> try List.assoc r wregs with Not_found -> assert false


let reg_compare = Pervasives.compare

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

let fold_barrier_option f k =
  fold_domain
    (fun d k ->
      fold_type (fun t k -> f d t k) k)
    k

let do_fold_dmb_dsb f k =
  let k = 
    fold_barrier_option
      (fun d t k -> f (DMB (d,t)) k)
      k in
  let k = 
    fold_barrier_option
      (fun d t k -> f (DSB (d,t)) k)
      k in
  k

let fold_barrier f k =
  let k = do_fold_dmb_dsb f k in
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

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type k = int
type lbl = Label.t

type condition = NE | EQ
type op = ADD | EOR | SUBS
type variant = V32 | V64
type kr = K of k | RV of variant * reg
let k0 = K 0

type ld_type = AA | XX | AX

let ldr_memo = function
  | AA -> "LDAR"
  | XX -> "LDXR"
  | AX -> "LDAXR"

type st_type = YY | LY

let str_memo = function
  | YY -> "STXR"
  | LY -> "STLXR"


type instruction =
(* Branches *)
  | I_B of lbl
  | I_BC of condition * lbl
  | I_CBZ of variant * reg * lbl
  | I_CBNZ of variant * reg * lbl
(* Load and Store *)
  | I_LDR of variant * reg * reg * kr
  | I_LDAR of variant * ld_type * reg * reg
  | I_STR of variant * reg * reg * kr
  | I_STLR of variant * reg * reg
  | I_STXR of variant * st_type * reg * reg * reg
(* Operations *)
  | I_MOV of variant * reg * k
  | I_SXTW of reg * reg
  | I_OP3 of variant * op * reg * reg * kr
(* Barrier *)
  | I_FENCE of barrier

let pp_label i = i

open PPMode

let pp_hash m = match m with
| Ascii | Dot -> "#"
| Latex -> "\\#"
| DotFig -> "\\\\#"

let pp_k m v = pp_hash m ^ string_of_int v

type basic_pp = { pp_k : k -> string; }


let pp_memo memo = memo

let pp_cond = function
  | NE -> "NE"
  | EQ -> "EQ"

let pp_vreg v r = match v with
| V32 -> pp_wreg r
| V64 -> pp_xreg r


let pp_op = function
  | ADD -> "ADD"
  | EOR -> "EOR"
  | SUBS -> "SUBS"


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
  | K 0 when not showzero -> ""
  | K k -> "," ^ m.pp_k k
  | RV (v,r) ->
      "," ^ pp_vreg v r ^
      (match v with V32 -> ",SXTW" | V64 -> "") in

  let pp_mem memo v rt ra kr =
    pp_memo memo ^ " " ^ pp_vreg v rt ^
    ",[" ^ pp_xreg ra ^ pp_kr false kr ^ "]" in

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
(* Branches *)
  | I_B lbl ->
      sprintf "B %s" (pp_label lbl)
  | I_BC (cond,lbl) ->
      sprintf "B.%s %s" (pp_cond cond) (pp_label lbl)
  | I_CBZ (v,r,lbl) ->
      sprintf "CBZ %s,%s" (pp_vreg v r) (pp_label lbl)
  | I_CBNZ (v,r,lbl) ->
      sprintf "CBNZ %s,%s" (pp_vreg v r) (pp_label lbl)
(* Load and Store *)
  | I_LDR (v,r1,r2,k) ->
      pp_mem "LDR" v r1 r2 k
  | I_LDAR (v,t,r1,r2) ->
      pp_mem (ldr_memo t) v r1 r2 k0
  | I_STR (v,r1,r2,k) ->
      pp_mem "STR" v r1 r2 k
  | I_STLR (v,r1,r2) ->
      pp_mem "STLR" v r1 r2 k0
  | I_STXR (v,t,r1,r2,r3) ->
      pp_stxr (str_memo t) v r1 r2 r3
(* Operations *)
  | I_MOV (v,r,k) ->
      pp_ri "MOV" v r k
  | I_SXTW (r1,r2) ->
      sprintf "SXTW %s,%s" (pp_xreg r1) (pp_wreg r2)
  | I_OP3 (v,SUBS,ZR,r,K k) ->
      pp_ri "CMP" v r k
  | I_OP3 (v,SUBS,ZR,r2,RV (v3,r3)) when v=v3->
      pp_rr "CMP" v r2 r3
  | I_OP3 (v,op,r1,r2,K k) ->
      pp_rri (pp_op op) v r1 r2 k
  | I_OP3 (v,op,r1,r2,kr) ->
      pp_rrkr (pp_op op) v r1 r2 kr
(* Barrier *)
  | I_FENCE b ->
      pp_barrier b


let pp_instruction m =
  do_pp_instruction 
    {pp_k = pp_k m}

let dump_instruction =
  do_pp_instruction 
    {pp_k = (fun v -> "#" ^ string_of_int v)}

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb = List.map (fun r -> Ireg r) gprs

let fold_regs (f_regs,f_sregs) =

  let fold_reg reg (y_reg,y_sreg) = match reg with
  | Ireg _ -> f_regs reg y_reg,y_sreg
  | Symbolic_reg reg ->  y_reg,f_sregs reg y_sreg
  | Internal _|ZR -> y_reg,y_sreg in

  let fold_kr kr y = match kr with
  | K _ -> y
  | RV (_,r) -> fold_reg r y in

  fun c ins -> match ins with
  | I_B _ | I_BC _ | I_FENCE _
    -> c
  | I_CBZ (_,r,_) | I_CBNZ (_,r,_) | I_MOV (_,r,_)
    -> fold_reg r c
  | I_LDAR (_,_,r1,r2) | I_STLR (_,r1,r2)
  | I_SXTW (r1,r2)
    -> fold_reg r1 (fold_reg r2 c)
  | I_LDR (_,r1,r2,kr) | I_STR (_,r1,r2,kr)
  | I_OP3 (_,_,r1,r2,kr)
    -> fold_reg r1 (fold_reg r2 (fold_kr kr c))
  | I_STXR (_,_,r1,r2,r3)
    -> fold_reg r1 (fold_reg r2 (fold_reg r3 c))


let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | Ireg _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | Internal _|ZR -> reg in

  let map_kr kr = match kr with
  | K _ -> kr
  | RV (v,r) -> RV (v,map_reg r) in

  fun ins -> match ins with
(* Branches *)
  | I_B _
  | I_BC _
  | I_FENCE _
    -> ins
  | I_CBZ (v,r,lbl) ->
      I_CBZ (v,map_reg r,lbl)
  | I_CBNZ (v,r,lbl) ->
      I_CBNZ (v,map_reg r,lbl)
(* Load and Store *)
  | I_LDR (v,r1,r2,kr) ->
     I_LDR (v,map_reg r1,map_reg r2,map_kr kr)
  | I_LDAR (t,v,r1,r2) ->
     I_LDAR (t,v,map_reg r1,map_reg r2)
  | I_STR (v,r1,r2,k) ->
      I_STR (v,map_reg r1,map_reg r2,k)
  | I_STLR (v,r1,r2) ->
      I_STLR (v,map_reg r1,map_reg r2)
  | I_STXR (v,t,r1,r2,r3) ->
      I_STXR (v,t,map_reg r1,map_reg r2,map_reg r3)
(* Operations *)
  | I_MOV (v,r,k) ->
      I_MOV (v,map_reg r,k)
  | I_SXTW (r1,r2) ->
      I_SXTW (map_reg r1,map_reg r2)
  | I_OP3 (v,op,r1,r2,kr) ->
      I_OP3 (v,op,map_reg r1,map_reg r2,map_kr kr)

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
      -> [Label.Next; Label.To lbl;]
  | I_LDR _
  | I_STR _
  | I_LDAR _
  | I_STLR _
  | I_STXR _
  | I_MOV _
  | I_SXTW _
  | I_OP3 _
  | I_FENCE _
      -> [Label.Next;]

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg

      let get_naccesses = function
        | I_LDR _ | I_LDAR _
        | I_STR _ | I_STLR _ | I_STXR _
          -> 1
        | I_B _
        | I_BC _
        | I_CBZ _
        | I_CBNZ _
        | I_MOV _
        | I_SXTW _
        | I_OP3 _
        | I_FENCE _
          -> 0

      let fold_labels k f = function
        | I_B lbl
        | I_BC (_,lbl)
        | I_CBZ (_,_,lbl)
        | I_CBNZ (_,_,lbl)
          -> f k lbl
        | _ -> k

      let map_labels f = function
        | I_B lbl -> I_B (f lbl)
        | I_BC (c,lbl) -> I_BC (c,f lbl)
        | I_CBZ (v,r,lbl) -> I_CBZ (v,r,f lbl)
        | I_CBNZ (v,r,lbl) -> I_CBNZ (v,r,f lbl)
        | ins -> ins
    end)

let get_macro _name = raise Not_found

let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4
