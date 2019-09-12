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

(** MIPS architecture, base definitions *)

open Printf

let arch = Archs.mips
let endian = Endian.Big
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type ireg =
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
  | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31

type reg =
  | IReg of ireg
  | PC
  | HI | LO
  | RESADDR
  | Symbolic_reg of string
  | Internal of int

let parse_ireg = function
  | "$0"|"$zero" -> R0
  | "$1"|"$at" -> R1
  | "$2"|"$v0" -> R2
  | "$3"|"$v1" -> R3
  | "$4"|"$a0" -> R4
  | "$5"|"$a1" -> R5
  | "$6"|"$a2" -> R6
  | "$7"|"$a3" -> R7
  | "$8"|"$t0" -> R8
  | "$9"|"$t1" -> R9
  | "$10"|"$t2" -> R10
  | "$11"|"$t3" -> R11
  | "$12"|"$t4" -> R12
  | "$13"|"$t5" -> R13
  | "$14"|"$t6" -> R14
  | "$15"|"$t7" -> R15
  | "$16"|"$s0" -> R16
  | "$17"|"$s1" -> R17
  | "$18"|"$s2" -> R18
  | "$19"|"$s3" -> R19
  | "$20"|"$s4" -> R20
  | "$21"|"$s5" -> R21
  | "$22"|"$s6" -> R22
  | "$23"|"$s7" -> R23
  | "$24"|"$t8" -> R24
  | "$25"|"$t9" -> R25
  | "$26"|"$k0" -> R26
  | "$27"|"$k1" -> R27
  | "$28"|"$gp" -> R28
  | "$29"|"$sp" -> R29
  | "$30"|"$fp" -> R30
  | "$31"|"$ra" -> R31
  | _ -> raise Exit


let parse_reg s =
  try Some (IReg (parse_ireg s))
  with Exit -> None

open PPMode

let add_dollar m s = match  m with
| Ascii | Dot -> "$" ^ s
| Latex -> "\\$" ^ s
| DotFig -> "\\\\$" ^ s

let do_pp_ireg m = function
  | R0 -> add_dollar m "0"
  | R1 -> add_dollar m "1"
  | R2 -> add_dollar m "2"
  | R3 -> add_dollar m "3"
  | R4 -> add_dollar m "4"
  | R5 -> add_dollar m "5"
  | R6 -> add_dollar m "6"
  | R7 -> add_dollar m "7"
  | R8 -> add_dollar m "8"
  | R9 -> add_dollar m "9"
  | R10 -> add_dollar m "10"
  | R11 -> add_dollar m "11"
  | R12 -> add_dollar m "12"
  | R13 -> add_dollar m "13"
  | R14 -> add_dollar m "14"
  | R15 -> add_dollar m "15"
  | R16 -> add_dollar m "16"
  | R17 -> add_dollar m "17"
  | R18 -> add_dollar m "18"
  | R19 -> add_dollar m "19"
  | R20 -> add_dollar m "20"
  | R21 -> add_dollar m "21"
  | R22 -> add_dollar m "22"
  | R23 -> add_dollar m "23"
  | R24 -> add_dollar m "24"
  | R25 -> add_dollar m "25"
  | R26 -> add_dollar m "26"
  | R27 -> add_dollar m "27"
  | R28 -> add_dollar m "28"
  | R29 -> add_dollar m "29"
  | R30 -> add_dollar m "30"
  | R31 -> add_dollar m "31"

let do_pp_reg m = function
  | IReg r -> do_pp_ireg m r
  | Symbolic_reg r -> "%" ^ r
  | Internal i -> sprintf "i%i" i
  | PC -> add_dollar m "pc"
  | HI -> add_dollar m "hi"
  | LO -> add_dollar m "lo"
  | RESADDR -> add_dollar m "res"

let r0 = IReg R0

let pp_reg = do_pp_reg Ascii

let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r
let typeof c = assert false

(************)
(* Barriers *)
(************)

type barrier = Sync

let all_kinds_of_barriers = [Sync;]

let pp_barrier = function
  | Sync -> "Sync"


let barrier_compare = compare


(****************)
(* Instructions *)
(****************)

type k = int
type lbl = Label.t

type op = ADD | ADDU | SUB | SUBU | SLT | SLTU | AND | OR | XOR | NOR
type cond = EQ | NE
type condz = LEZ | GTZ | LTZ | GEZ

type instruction =
  | NOP
  | LI of reg * k
  | OP of op * reg * reg * reg
  | OPI of op * reg * reg * k
  | B of lbl
  | BC of cond * reg * reg * lbl
  | BCZ of condz * reg * lbl
  | LW of reg * k * reg
  | SW of reg * k * reg
  | LL of reg * k * reg
  | SC of reg * k * reg
  | SYNC

type parsedInstruction = instruction

let move r1 r2 = OPI (OR,r1,r2,0)

let pp_lbl = fun i -> i

let pp_cond = function
  | EQ -> "eq"
  | NE -> "ne"

let pp_condz = function
  | LEZ -> "lez"
  | GTZ -> "gtz"
  | LTZ -> "ltz"
  | GEZ -> "gez"

let pp_op = function
  | ADD -> "add"
  | ADDU -> "addu"
  | SUB -> "sub"
  | SUBU -> "subu"
  | SLT -> "slt"
  | SLTU -> "sltu"
  | AND -> "and"
  | OR -> "or"
  | XOR -> "xor"
  | NOR -> "nor"

let pp_opi = function
  | ADD -> "addi"
  | ADDU -> "addiu"
  | SUB -> "subi"
  | SUBU -> "subiu"
  | SLT -> "slti"
  | SLTU -> "sltiu"
  | AND -> "andi"
  | OR -> "ori"
  | XOR -> "xori"
  | NOR -> "nori"


let pp_instruction m =
  let pp_reg = do_pp_reg m in
  let pp_rkr memo r1 k r2 =
    sprintf "%s %s,%i(%s)"
      memo (pp_reg r1) k (pp_reg r2) in
  fun i -> match i with
  | LI (r,k) ->
      sprintf "li %s,%i" (pp_reg r) k
  | OP (op,r1,r2,r3) ->
      sprintf "%s %s,%s,%s"
        (pp_op op)
        (pp_reg r1)
        (pp_reg r2)
        (pp_reg r3)
  | OPI (op,r1,r2,k) ->
      sprintf "%s %s,%s,%i"
        (pp_opi op)
        (pp_reg r1)
        (pp_reg r2)
        k
  | B lbl -> sprintf "b %s" (pp_lbl lbl)
  | BC (c,r1,r2,lbl) ->
      sprintf "b%s %s,%s,%s"
        (pp_cond c)
        (pp_reg r1)
        (pp_reg r2)
        (pp_lbl lbl)
  | BCZ (c,r1,lbl) ->
      sprintf "b%s %s,%s"
        (pp_condz c)
        (pp_reg r1)
        (pp_lbl lbl)
  | LW (r1,k,r2) -> pp_rkr "lw" r1 k r2
  | SW (r1,k,r2) -> pp_rkr "sw" r1 k r2
  | LL (r1,k,r2) -> pp_rkr "ll" r1 k r2
  | SC (r1,k,r2) -> pp_rkr "sc" r1 k r2
  | NOP -> "nop"
  | SYNC -> "sync"

let dump_instruction = pp_instruction Ascii


(****************************)
(* Symbolic registers stuff *)
(****************************)
let tmp1 = Symbolic_reg "T1"
let tmp2 = Symbolic_reg "T2"
let tmp3 = Symbolic_reg "T3"

let allowed_for_symb =
  List.map
    (fun r -> IReg r)
    [R2; R3; R4; R5; R6; R7; R8; R9;
     R10; R11; R12; R13; R14; R15; R16; R17; R18; R19;
     R20; R21; R22; R23; R24; R25;]

let fold_regs (f_reg,f_sreg) =
  let fold_reg reg (y_reg,y_sreg) = match reg with
  | IReg _|PC|HI|LO|RESADDR -> f_reg reg y_reg,y_sreg
  | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
  | Internal _ -> y_reg,y_sreg in

  fun c ins -> match ins with
  | LI (r,_)|BCZ (_,r,_) -> fold_reg r c
  | OP (_,r1,r2,r3) ->
      fold_reg r1 (fold_reg r2 (fold_reg r3 c))
  | OPI (_,r1,r2,_)
  | LW (r1,_,r2)
  | SW (r1,_,r2)
  | LL (r1,_,r2)
  | SC (r1,_,r2)
  | BC (_,r1,r2,_) ->
      fold_reg r1 (fold_reg r2 c)
  | NOP|B _|SYNC -> c

let map_regs f_reg f_symb =
  let map_reg reg = match reg with
  | IReg _|PC|HI|LO|RESADDR -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | Internal _ -> reg in

  fun ins -> match ins with
  | LI (r,k) -> LI (map_reg r,k)
  | OP (op,r1,r2,r3) ->
      OP (op,map_reg r1,map_reg r2,map_reg r3)
  | OPI (op,r1,r2,k) ->
      OPI (op,map_reg r1,map_reg r2,k)
  | LW (r1,k,r2) ->
      LW (map_reg r1,k,map_reg r2)
  | SW (r1,k,r2) ->
      SW (map_reg r1,k,map_reg r2)
  | LL (r1,k,r2) ->
      LL (map_reg r1,k,map_reg r2)
  | SC (r1,k,r2) ->
      SC (map_reg r1,k,map_reg r2)
  | BC (c,r1,r2,lbl) ->
      BC (c,map_reg r1,map_reg r2,lbl)
  | BCZ (c,r,lbl) ->
      BCZ (c,map_reg r,lbl)
  | NOP|B _| SYNC -> ins

(* No addresses burried in MIPS code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

(* No normalisation (yet ?) *)
let norm_ins ins = ins

(* Instruction continuation *)
let get_next = function
  | NOP
  | LI _
  | OP _
  | OPI _
  | LW _
  | SW _
  | LL _
  | SC _
  | SYNC -> [Label.Next]
  | B lbl -> [Label.To lbl]
  | BC (_,_,_,lbl)|BCZ (_,_,lbl) -> [Label.Next; Label.To lbl;]

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let parsed_tr i = i

      let get_naccesses = function
        | NOP
        | LI _
        | OP _
        | OPI _
        | SYNC
        | B _
        | BC _
        | BCZ _ -> 0
        | LW _
        | SW _
        | LL _
        | SC _ -> 1

      let fold_labels k f = function
        | B lbl
        | BC (_,_,_,lbl)
        | BCZ (_,_,lbl)
          -> f k lbl
        | _ -> k

      let map_labels f = function
        | B lbl -> B (f lbl)
        | BC (c,r1,r2,lbl) -> BC (c,r1,r2,f lbl)
        | BCZ (c,r,lbl) -> BCZ (c,r,f lbl)
        | ins -> ins

    end)

let get_macro _name = raise Not_found

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"
