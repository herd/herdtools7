(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
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

open Printf
open Sign

let arch = Archs.riscv
let endian = Endian.Little
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type gpr =
  | X0  | X1  | X2  | X3
  | X4  | X5  | X6  | X7
  | X8  | X9  | X10 | X11
  | X12 | X13 | X14 | X15
  | X16 | X17 | X18 | X19
  | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27
  | X28 | X29 | X30 | X31

type reg =
  | Ireg of gpr
  | Symbolic_reg of string
  | RESADDR

let gprs =
[
  X0 ; X1 ; X2 ; X3 ;
  X4 ; X5 ; X6 ; X7 ;
  X8 ; X9 ; X10; X11 ;
  X12; X13; X14; X15 ;
  X16; X17; X18; X19 ;
  X20; X21; X22; X23 ;
  X24; X25; X26; X27 ;
  X28; X29; X30 ; X31 ;
]

let pp_xregs =
[
 X0,["x0";"zero"]  ; X1,["x1";"ra";]  ; X2,["x2";"sp";]  ; X3,["x3";"gp";] ;
 X4,["x4";"tp";]  ; X5,["x5";"t0";]  ; X6,["x6";"t1";]  ; X7,["x7";"t2";] ;
 X8,["x8";"s0";"fp";]  ; X9,["x9";"s1";] ;
 X10,["x10";"a0";];  X11,["x11";"a1";] ;
 X12,["x12";"a2";] ; X13,["x13";"a3";] ; X14,["x14";"a4";] ; X15,["x15";"a5";] ;
 X16,["x16";"a6";] ; X17,["x17";"a7";] ;
 X18,["x18";"s2";] ; X19,["x19";"s3";] ;
 X20,["x20";"s4";] ; X21,["x21";"s5";] ; X22,["x22";"s6";] ;
 X23,["x23";"s7";] ; X24,["x24";"s8";] ; X25,["x25";"s9";] ;
 X26,["x26";"10";] ; X27,["x27";"s11";] ;
 X28,["x28";"t3";] ; X29,["x29";"t4";] ;
 X30,["x30";"t5";] ; X31,["x31";"t6";] ;
]

module RMap =  MyMap.Make
    (struct
      type t = gpr
      let compare = compare
    end)

let pp_map =
  List.fold_right
    (fun (r,pps) k -> match pps with
    | pp::_ -> RMap.add r pp k
    | [] -> assert false)
    pp_xregs RMap.empty

let parse_map =
  List.fold_right
    (fun (r,pp) k ->
      let r = Ireg r in
      List.fold_right
        (fun pp k -> StringMap.add pp r k)
        pp k)
    pp_xregs  StringMap.empty


let parse_reg s =
  try Some (StringMap.find s parse_map)
  with Not_found -> None

let pp_reg  r = match r with
| Symbolic_reg r -> "%" ^ r
| RESADDR -> "res"
| Ireg r ->
    try RMap.find r pp_map
    with Not_found -> assert false


let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg r -> Some r
  | _ -> None

let symb_reg r = Symbolic_reg r
let typeof _ = assert false

(**********)
(* Fences *)
(**********)

type access = R | W | RW

let fold_access f k =
  let k = f R k in
  let k = f W k in
  let k = f RW k in
  k

let pp_access = function
  | R -> "r"
  | W -> "w"
  | RW -> "rw"

type barrier =
  | Fence of access * access
  | FenceI
  | FenceTSO

let do_fold_fence f k =
  let k =
    fold_access
      (fun a1 k ->
        fold_access
          (fun a2 k -> f (Fence (a1,a2)) k)
          k)
      k in
  f FenceTSO k

let fold_barrier f k =
  let k = f FenceI k in
  let k = do_fold_fence f k in
  k

let do_pp_barrier sep1 sep2 = function
  | FenceI -> "fence.i"
  | FenceTSO -> "fence.tso"
  | Fence (a1,a2) ->
      sprintf "fence%s%s%s%s"
        sep1 (pp_access a1) sep2 (pp_access a2)

let pp_barrier f = do_pp_barrier " " ","     f
let pp_barrier_dot f = do_pp_barrier "." "."     f

let barrier_compare = compare


(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type opi = ADDI | SLTI | SLTIU | ANDI | ORI |  XORI | SLLI | SRLI | SRAI
let pp_opi = function
  | ADDI -> "addi"
  | SLTI -> "slti"
  | SLTIU -> "sltiu"
  | ANDI -> "andi"
  | ORI -> "ori"
  | XORI -> " xori"
  | SLLI -> "slli"
  | SRLI -> "srli"
  | SRAI -> "srai"


type opiw = ADDIW | SLLIW | SRLIW | SRAIW

let pp_opiw = function
  | ADDIW -> "addiw"
  | SLLIW -> "slliw"
  | SRLIW -> "srliw"
  | SRAIW -> "sraiw"


type op =   ADD  | SLT | SLTU | AND | OR | XOR | SLL | SRL | SUB | SRA

let pp_op = function
  | ADD -> "add"
  | SLT -> "slt"
  | SLTU -> "sltu"
  | AND -> "and"
  | OR -> "or"
  | XOR -> "xor"
  | SLL -> "sll"
  | SRL -> "srl"
  | SUB -> "sub"
  | SRA -> "sra"

type opw = ADDW | SLLW | SRLW | SUBW | SRAW


let pp_opw = function
| ADDW -> "addw"
| SLLW -> "sllw"
| SRLW -> "srlw"
| SUBW -> "subw"
| SRAW -> "sraw"

type opamo =
  | AMOSWAP
  | AMOADD
  | AMOAND
  | AMOOR
  | AMOXOR
  | AMOMAX
  | AMOMAXU
  | AMOMIN
  | AMOMINU

let pp_opamo = function
  | AMOSWAP -> "amoswap"
  | AMOADD -> "amoadd"
  | AMOAND -> "amoand"
  | AMOOR -> "amoor"
  | AMOXOR -> "amoxor"
  | AMOMAX -> "amomax"
  | AMOMAXU -> "amomaxu"
  | AMOMIN -> "amomin"
  | AMOMINU -> "amominu"

type cond = EQ | NE | LT | LTU | GE | GEU

let pp_bcc = function
  | EQ -> "beq"
  | NE -> "bne"
  | LT -> "blt"
  | LTU -> "bltu"
  | GE -> "bge"
  | GEU -> "bgeu"

type width = Byte | Half | Word | Double

let tr_width = function
  | Byte -> MachSize.Byte
  | Half -> MachSize.Short
  | Word -> MachSize.Word
  | Double -> MachSize.Quad

let pp_width = function
  | Byte -> "b"
  | Half -> "h"
  | Word -> "w"
  | Double -> "d"

let pp_signed = function
  | Signed -> ""
  | Unsigned -> "u"

type mo = Rlx | Acq | Rel | AcqRel | Sc

let pp_mo = function
  | Rlx -> ""
  | Acq -> ".aq"
  | Rel -> ".rl"
  | AcqRel -> ".aq.rl"
  | Sc -> ".sc"

let pp_load w s mo = sprintf "l%s%s%s" (pp_width w) (pp_signed s) (pp_mo mo)
let pp_store w mo = sprintf "s%s%s" (pp_width w) (pp_mo mo)


let pp_lr w mo = sprintf "lr.%s%s" (pp_width w) (pp_mo mo)
let pp_sc w mo = sprintf "sc.%s%s" (pp_width w) (pp_mo mo)

let pp_amo op w mo = sprintf "%s.%s%s" (pp_opamo op) (pp_width w) (pp_mo mo)

type signed = Sign.t

type 'k kinstruction =
  | INop
  | OpI of opi * reg * reg * 'k
  | OpIW of opiw * reg * reg * 'k
  | Op of op * reg * reg * reg
  | OpW of opw * reg * reg * reg
  | J of lbl
  | Bcc of cond * reg * reg * lbl
  | Load of width * signed * mo * reg * int * reg
  | Store of width * mo * reg * int * reg
  | LoadReserve of width * mo * reg * reg
  | StoreConditional of width * mo * reg * reg * reg
  | Amo of opamo * width * mo * reg * reg * reg
  | FenceIns of  barrier

type instruction = int kinstruction
type parsedInstruction = MetaConst.k kinstruction

let pp_label lbl = lbl

let pp_k _m v = sprintf "%i" v

type 'k basic_pp = { pp_k : 'k -> string; }

let do_pp_instruction m = function
  | INop -> "nop"
  | OpI (op,r1,r2,k) ->
      sprintf "%s %s,%s,%s"
        (pp_opi op) (pp_reg r1) (pp_reg r2) (m.pp_k k)
  | OpIW (op,r1,r2,k) ->
      sprintf "%s %s,%s,%s"
        (pp_opiw op) (pp_reg r1) (pp_reg r2) (m.pp_k k)
  | Op (op,r1,r2,r3) ->
      sprintf "%s %s,%s,%s"
        (pp_op op) (pp_reg r1) (pp_reg r2) (pp_reg r3)
  | OpW (op,r1,r2,r3) ->
      sprintf "%s %s,%s,%s"
        (pp_opw op) (pp_reg r1) (pp_reg r2) (pp_reg r3)
  | J lbl ->
      sprintf "j %s" (pp_label lbl)
  | Bcc (cond,r1,r2,lbl) ->
      sprintf  "%s %s,%s,%s"
        (pp_bcc cond) (pp_reg r1) (pp_reg r2) (pp_label lbl)
  | Load (w,s,mo,r1,o,r2) ->
      sprintf "%s %s,%i(%s)"
        (pp_load w s mo) (pp_reg r1) o (pp_reg r2)
  | Store (w,mo,r1,o,r2) ->
      sprintf "%s %s,%i(%s)"
        (pp_store w mo) (pp_reg r1) o (pp_reg r2)
  | LoadReserve (w,mo,r1,r2) ->
      sprintf "%s %s,0(%s)"
        (pp_lr w mo) (pp_reg r1) (pp_reg r2)
  | StoreConditional (w,mo,r1,r2,r3) ->
     sprintf "%s %s,%s,0(%s)"
        (pp_sc w mo) (pp_reg r1) (pp_reg r2) (pp_reg r3)
  | Amo (op,w,mo,r1,r2,r3) ->
      sprintf "%s %s,%s,(%s)"
        (pp_amo op w mo)  (pp_reg r1) (pp_reg r2) (pp_reg r3)
  | FenceIns f -> pp_barrier f

let pp_instruction m =
  do_pp_instruction
    {pp_k = pp_k m}

let dump_instruction = do_pp_instruction {pp_k = (fun v -> sprintf "%i" v)}
and dump_parsedInstruction = do_pp_instruction {pp_k = MetaConst.pp; }

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb =
  List.map (fun r -> Ireg r)
    [X5 ; X6 ; X7 ;
     X8 ; X9 ; X10; X11 ;
     X12; X13; X14; X15 ;
     X16; X17; X18; X19 ;
     X20; X21; X22; X23 ;
     X24; X25; X26; X27 ;
     X28; X29; X30 ; X31 ; ]

let fold_regs (f_reg,f_sreg) =

  let fold_reg reg (y_reg,y_sreg) = match reg with
  | Ireg _|RESADDR  -> f_reg reg y_reg,y_sreg
  | Symbolic_reg reg   -> y_reg,f_sreg reg y_sreg in

  fun c ins -> match ins with
  | INop|J _ | FenceIns _ -> c
  | OpI (_,r1,r2,_) | OpIW (_,r1,r2,_)
  | Bcc (_,r1,r2,_)
  | Load (_,_,_,r1,_,r2)
  | Store (_,_,r1,_,r2)
  | LoadReserve (_,_,r1,r2)
    -> fold_reg r1 (fold_reg r2 c)
  | Op (_,r1,r2,r3)  | OpW (_,r1,r2,r3)
  | StoreConditional (_,_,r1,r2,r3)|Amo (_,_,_,r1,r2,r3)
      -> fold_reg r1 (fold_reg r2 (fold_reg r3 c))


let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | Ireg _|RESADDR -> f_reg reg
  | Symbolic_reg reg -> f_symb reg in

  function ins -> match ins with
  | INop|J _ | FenceIns _ -> ins
  | OpI (op,r1,r2,k) ->
      OpI (op,map_reg r1,map_reg r2,k)
  | OpIW (op,r1,r2,k) ->
      OpIW (op,map_reg r1,map_reg r2,k)
  | Op (op,r1,r2,r3) ->
      Op (op,map_reg r1,map_reg r2,map_reg r3)
  | OpW (op,r1,r2,r3) ->
      OpW (op,map_reg r1,map_reg r2,map_reg r3)
  | Bcc (cc,r1,r2,lbl) ->
      Bcc (cc,map_reg r1,map_reg r2,lbl)
  | Load (w,s,mo,r1,o,r2) ->
      Load (w,s,mo,map_reg r1,o,map_reg r2)
  | Store (w,mo,r1,o,r2) ->
      Store (w,mo,map_reg r1,o,map_reg r2)
  | LoadReserve (w,mo,r1,r2) ->
      LoadReserve (w,mo,map_reg r1,map_reg r2)
  | StoreConditional (w,mo,r1,r2,r3) ->
      StoreConditional (w,mo,map_reg r1,map_reg r2,map_reg r3)
  | Amo (op,w,mo,r1,r2,r3) ->
      Amo (op,w,mo,map_reg r1,map_reg r2,map_reg r3)

(* No addresses burried in ARM code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

let norm_ins ins = ins

let get_next = function
  | J lbl -> [Label.To lbl;]
  | Bcc (_,_,_,lbl) -> [Label.Next; Label.To lbl;]
  | INop|OpI (_, _, _, _)|OpIW (_, _, _, _)|Op (_, _, _, _)|OpW (_, _, _, _)
  | Load (_,_, _, _, _, _)|Store (_,_, _, _, _)|LoadReserve (_, _, _, _)
  | StoreConditional (_, _, _, _, _)|FenceIns _|Amo _
    -> [Label.Next;]

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let k_tr = MetaConst.as_int

      let parsed_tr i = match i with
      | OpI (op,r1,r2,k) -> OpI (op,r1,r2,k_tr k)
      | OpIW (op,r1,r2,k) -> OpIW (op,r1,r2,k_tr k)
      | Op (_, _, _, _)|OpW (_, _, _, _)|J _|Bcc (_, _, _, _)
      |INop
      |Load (_, _, _, _, _, _)|Store (_, _ ,_ , _, _)
      |LoadReserve (_, _, _, _)|StoreConditional (_, _, _, _, _)|Amo _|FenceIns _
          as keep
        -> keep

      let get_naccesses = function
        | Amo (AMOSWAP,_,_,Ireg X0,_,_)
        | Amo ((AMOOR|AMOADD),_,_,_,Ireg X0,_)
        | Load _ | LoadReserve _ | Store _ | StoreConditional _ -> 1
        | Amo _ -> 2
        | INop
        | OpI (_, _, _, _)|OpIW (_, _, _, _)|Op (_, _, _, _)
        | OpW (_, _, _, _)|J _|Bcc (_, _, _, _)|FenceIns _
          -> 0

      let fold_labels k f = function
        | J lbl
        | Bcc (_,_,_,lbl)
          -> f k lbl
        | INop
        |OpI (_, _, _, _)|OpIW (_, _, _, _)|Op (_, _, _, _)
        |OpW (_, _, _, _)|Load (_, _, _, _, _, _)|Store (_, _, _, _, _)
        |LoadReserve (_, _, _, _)|StoreConditional (_, _, _, _, _)|Amo _|FenceIns _
            -> k
      let map_labels f = function
        | J lbl -> J (f lbl)
        | Bcc (cc,r1,r2,lbl) -> Bcc (cc,r1,r2,f lbl)
        |INop
        |OpI (_, _, _, _)|OpIW (_, _, _, _)|Op (_, _, _, _)
        |OpW (_, _, _, _)|Load _|Store _
        |LoadReserve (_, _, _, _)|StoreConditional (_, _, _, _, _)|Amo _|FenceIns _
            as ins
            -> ins

    end)

let get_macro _name = raise Not_found
