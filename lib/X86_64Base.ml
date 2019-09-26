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

(** Define registers, barriers, and instructions for X86_64 *)

open Printf

(* Who am I ? *)
let arch = Archs.x86_64
let endian = Endian.Little
let base_type = CType.Base "uint64_t"

(*************)
(* Registers *)
(*************)

(* General purpose registers *)
type base_reg =
  | AX | BX | CX | DX | SI | DI | BP | SP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* B H -> 8b with B as 8 low bytes and H 8 high bytes
   W -> 16b
   L -> 32b
   Q -> 64b *)
type reg_part =
  | B | H | W | L | Q

type reg =
  | RIP
  | Ireg of base_reg * reg_part
  | Symbolic_reg of string
  | Internal of int

let loop_idx = Internal 0
let sig_cell = "sig_cell"

let pc = RIP

let gen_regs =
  [
(* 64b registers *)
    (AX, Q), "RAX";
    (BX, Q), "RBX";
    (CX, Q), "RCX";
    (DX, Q), "RDX";
    (SI, Q), "RSI";
    (DI, Q), "RDI";
    (BP, Q), "RBP";
    (SP, Q), "RSP";
    (R8, Q), "R8";
    (R9, Q), "R9";
    (R10, Q), "R10";
    (R11, Q), "R11";
    (R12, Q), "R12";
    (R13, Q), "R13";
    (R14, Q), "R14";
    (R15, Q), "R15";
(* 32b registers *)
    (AX, L), "EAX";
    (BX, L), "EBX";
    (CX, L), "ECX";
    (DX, L), "EDX";
    (SI, L), "ESI";
    (DI, L), "EDI";
    (BP, L), "EBP";
    (SP, L), "ESP";
    (R8, L), "R8D";
    (R9, L), "R9D";
    (R10, L), "R10D";
    (R11, L), "R11D";
    (R12, L), "R12D";
    (R13, L), "R13D";
    (R14, L), "R14D";
    (R15, L), "R15D";
(* 16b registers *)
    (AX, W), "AX";
    (BX, W), "BX";
    (CX, W), "CX";
    (DX, W), "DX";
    (SI, W), "SI";
    (DI, W), "DI";
    (BP, W), "BP";
    (SP, W), "SP";
    (R8, W), "R8W";
    (R9, W), "R9W";
    (R10, W), "R10W";
    (R11, W), "R11W";
    (R12, W), "R12W";
    (R13, W), "R13W";
    (R14, W), "R14W";
    (R15, W), "R15W";
(* 8 low bits registers *)
    (AX, B), "AL";
    (BX, B), "BL";
    (CX, B), "CL";
    (DX, B), "DL";
    (SI, B), "SIL";
    (DI, B), "DIL";
    (BP, B), "BPL";
    (SP, B), "SPL";
    (R8, B), "R8B";
    (R9, B), "R9B";
    (R10, B), "R10B";
    (R11, B), "R11B";
    (R12, B), "R12B";
    (R13, B), "R13B";
    (R14, B), "R14B";
    (R15, B), "R15B";
(* 8 high bits registers *)
    (AX, H), "AH";
    (BX, H), "BH";
    (CX, H), "CH";
    (DX, H), "DH";
  ]

(* Match reg size with its nae in GCC asm inline *)
let reg_size_to_string = function
  | B -> "b"
  | H -> "h"
  | W -> "w"
  | L -> "k"
  | Q -> "q"

let parse_list = List.map (fun ((r, t),s) -> s, Ireg (r, t)) gen_regs
let regs = List.map (fun ((r, t),s) -> Ireg (r, t), s) gen_regs

let reg_string r t =
  String.lowercase_ascii (List.assoc (Ireg (r, t)) regs)
let reg64_string r =
  reg_string r Q

let parse_reg s =
  try Some (List.assoc (String.uppercase_ascii s) parse_list)
  with Not_found -> None

let pp_reg r = match r with
  | Symbolic_reg r -> "%"^r
  | Internal i -> sprintf "i%i" i
  | _ -> try List.assoc r regs with Not_found -> assert false

let reg_compare r1 r2 = match r1, r2 with
  | Ireg (b1, t1), Ireg (b2, t2) -> compare b1 b2
  | _ -> compare r1 r2

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r

let reg_size_to_uint = function
  | B | H -> "uint8_t"
  | W -> "uint16_t"
  | L -> "uint32_t"
  | Q -> "uint64_t"

let typeof = function
  | Ireg (_, t) -> CType.Base (reg_size_to_uint t)
  | _ -> CType.Base "int"

let change_size_reg r sz = match r with
  | Ireg (b, t) -> Ireg (b, sz)
  | _ -> r

(************)
(* Barriers *)
(************)

type barrier =
  | Mfence

let pp_barrier b = match b with
  | Mfence -> "MFENCE"

let barrier_compare = compare

(****************)
(* Instructions *)
(****************)

type lbl = Label.t
type abs = ParsedConstant.v
type offset = int

type rm64 =
  |  Rm64_reg of reg
  |  Rm64_deref of reg * offset
  |  Rm64_abs of abs
(* Absolute memory location, we should later combine with Rm32_deref to have proper base-displacement (and later, scale-index) addressing *)

type effaddr =
  | Effaddr_rm64 of rm64

type operand =
  | Operand_effaddr of effaddr
  | Operand_immediate of int

let get_naccs_rm64 = function
  |  Rm64_reg _ -> 0
  |  Rm64_deref _ |  Rm64_abs _ -> 1

let get_naccs_eff  = function
  | Effaddr_rm64 rm -> get_naccs_rm64 rm

let get_naccs_op = function
  | Operand_immediate _ -> 0
  | Operand_effaddr e -> get_naccs_eff e

type inst_size =
  | B | W | L | Q | NO_SIZE

type inst_eff_op =
  | I_ADD
  | I_OR
  | I_XOR
  | I_MOV
  | I_CMP

type inst_eff =
  | I_DEC
  | I_INC
  | I_SETNB

type inst_eff_eff =
  | I_XCHG
  | I_XCHG_UNLOCKED

type condition =
  | C_EQ
  | C_NE
  | C_LE
  | C_LT
  | C_GT
  | C_GE
  | C_S          (* Sign *)
  | C_NS         (* Not sign *)

let pp_inst_size = function
  | B -> "B"
  | W -> "W"
  | L -> "L"
  | Q -> "Q"
  | NO_SIZE-> ""

let pp_inst_eff_op inst size =
  let inst_string = match inst with
    | I_ADD -> "ADD"
    | I_OR -> "OR"
    | I_XOR -> "XOR"
    | I_MOV -> "MOV"
    | I_CMP -> "CMP" in
  inst_string ^ pp_inst_size size

let pp_inst_eff inst size =
  let inst_string = match inst with
    | I_DEC -> "DEC"
    | I_INC -> "INC"
    | I_SETNB -> "SETNB" in
  inst_string ^ pp_inst_size size

let pp_inst_eff_eff inst size =
  let inst_string = match inst with
    | I_XCHG -> "XCHG"
    | I_XCHG_UNLOCKED -> "UXCH" in
  inst_string ^ pp_inst_size size

type instruction =
  | I_NOP
  | I_EFF_OP of inst_eff_op * inst_size * effaddr * operand
  | I_EFF of inst_eff * inst_size * effaddr
  | I_EFF_EFF of inst_eff_eff * inst_size * effaddr * effaddr
  | I_CMPXCHG of inst_size * effaddr * reg
  | I_JMP of lbl
  | I_JCC of condition * lbl
  | I_CMOVC of inst_size * reg * effaddr
  | I_LOCK of instruction
  | I_MFENCE

type parsedInstruction = instruction

let pp_abs = ParsedConstant.pp_v

open PPMode

let pp_comma m = match m with
  | Ascii |Dot -> ","
  | Latex -> "\\m "
  | DotFig -> "\\\\m "

type mm =
  {immediate : int -> string ;
   comma : string ;}

  let pp_condition = function
    | C_EQ  -> "E"
    | C_NE  -> "NE"
    | C_LE  -> "LE"
    | C_LT  -> "L"
    | C_GT  -> "G"
    | C_GE  -> "GE"
    | C_S   -> "S"
    | C_NS  -> "NS"

  let pp_offset = function
    | 0 -> ""
    | a -> string_of_int a


  let pp_rm64 rm64 =
    match rm64 with
    | Rm64_reg r -> pp_reg r
    | Rm64_deref (r,o) -> pp_offset o ^ "[" ^ pp_reg r ^ "]"
    | Rm64_abs v -> "[" ^ pp_abs v ^ "]"

let rec do_pp_instruction (m : mm) =
  let pp_effaddr ea =  match ea with
    | Effaddr_rm64 rm64 -> pp_rm64 rm64 in

  let pp_operand  op = match op with
    | Operand_effaddr(ea) -> pp_effaddr ea
    | Operand_immediate(v) -> m.immediate v in

  let ppi_inst_ea_op inst s ea op =
    pp_inst_eff_op inst s ^ " " ^ pp_effaddr ea ^ m.comma ^ pp_operand op in

  let ppi_lbl opcode lbl = opcode ^ " " ^ lbl in

  let ppi_ea inst s ea =
    pp_inst_eff inst s ^ " " ^ pp_effaddr ea in

  let ppi_ea_ea inst s ea1 ea2 =
    pp_inst_eff_eff inst s ^ " " ^ pp_effaddr ea1 ^ m.comma ^ pp_effaddr ea2 in

  let ppi_ea_r opcode s ea r =
    opcode ^ pp_inst_size s ^ " " ^ pp_effaddr ea ^ m.comma ^ pp_reg r in

  let ppi_r_ea opcode s r ea =
    opcode ^ pp_inst_size s ^ " " ^ pp_reg r ^ m.comma ^ pp_effaddr ea in


  fun i -> match i with
           | I_NOP -> "NOP"
           | I_EFF_OP(inst, s, ea, op) -> ppi_inst_ea_op inst s ea op
           | I_EFF(inst, s, ea) -> ppi_ea inst s ea
           | I_EFF_EFF(inst, s, ea1, ea2) -> ppi_ea_ea inst s ea1 ea2
           | I_CMPXCHG (s, ea, r) -> ppi_ea_r "CMPXCHG" s ea r
           | I_CMOVC (s, r, ea) ->  ppi_r_ea "CMOVC" s r ea
           | I_LOCK inst -> "LOCK; " ^ do_pp_instruction m i
           | I_JMP(lbl) -> ppi_lbl "JMP" lbl
           | I_JCC(cond, lbl) -> ppi_lbl ("J" ^ pp_condition cond) lbl
           | I_MFENCE  -> "MFENCE"

let pp_instruction m i =
  do_pp_instruction
    {immediate = string_of_int ;
     comma = pp_comma m ; } i

let dump_instruction =
  do_pp_instruction
    { immediate = (fun v -> "$" ^ string_of_int v) ;
      comma = "," ; }

(****************************)
(* Symbolic registers stuff *)
(****************************)

let reg_size_p size = function
  | Ireg (r,t) -> t = size
  | _ -> false

let allowed_for_symb_size size = List.filter (reg_size_p size)
                         (List.map (fun ((r, t),s) -> Ireg (r, t)) gen_regs)

let allowed_for_symb = allowed_for_symb_size Q

let rec fold_regs (f_reg,f_sreg) =

  let fold_reg (y_reg,y_sreg) reg = match reg with
    | RIP | Ireg _ -> f_reg reg y_reg,y_sreg
    | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
    | Internal _ -> y_reg,y_sreg in

  let fold_rm64 c = function
    | Rm64_reg reg | Rm64_deref (reg, _) -> fold_reg c reg
    | Rm64_abs _ -> c in

  let fold_effaddr c = function
    | Effaddr_rm64 rm -> fold_rm64 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins -> match ins with
               | I_EFF_OP (_, _, ea, op) ->
                  let c = fold_effaddr c ea in
                  fold_operand c op
               | I_NOP | I_JMP _ | I_JCC _ | I_MFENCE -> c
               | I_EFF (_, _, eff) -> fold_effaddr c eff
               | I_EFF_EFF (_, _, ea1, ea2) ->
                  let c = fold_effaddr c ea1 in
                  fold_effaddr c ea2
               | I_CMPXCHG (_, ea,r) ->
                  let c = fold_effaddr c ea in
                  fold_reg c r
               | I_CMOVC (_, reg,eff) ->
                  let c = fold_reg c reg in
                  fold_effaddr c eff
               | I_LOCK ins -> fold_regs (f_reg,f_sreg) c ins

let rec map_regs f_reg f_symb =

  let map_reg reg = match reg with
    | RIP | Ireg _ | Internal _ -> f_reg reg
    | Symbolic_reg reg -> f_symb reg in

  let map_rm64 = function
    | Rm64_reg reg -> Rm64_reg (map_reg reg)
    | Rm64_deref (reg, o) -> Rm64_deref (map_reg reg,o)
    | Rm64_abs _ as rm -> rm in

  let map_effaddr = function
    | Effaddr_rm64 rm -> Effaddr_rm64 (map_rm64 rm) in

  let map_operand op = match op with
    | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
    | Operand_immediate _ -> op in

  fun ins -> match ins with
             | I_EFF_OP(inst, s, ea, op) ->
                I_EFF_OP (inst, s, map_effaddr ea, map_operand op)
             | I_NOP | I_JMP _ | I_JCC _| I_MFENCE -> ins
             | I_EFF (inst, s, ea) ->
                I_EFF (inst, s, map_effaddr ea)
             | I_EFF_EFF (inst, s, ea1, ea2) ->
                I_EFF_EFF (inst, s, map_effaddr ea1, map_effaddr ea2)
             | I_CMPXCHG (s, ea,r) ->
                I_CMPXCHG (s, map_effaddr ea,map_reg r)
             | I_CMOVC (s, reg,eff) ->
                I_CMOVC (s, map_reg reg, map_effaddr eff)
             | I_LOCK ins ->
                I_LOCK (map_regs f_reg f_symb ins)

let rec fold_addrs f =

  let fold_rm64 c = function
    | Rm64_reg _ | Rm64_deref _ -> c
    | Rm64_abs v -> f v c in

  let fold_effaddr c = function
    | Effaddr_rm64 rm -> fold_rm64 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins -> match ins with
               | I_EFF_OP (_, _, ea, op) ->
                  let c = fold_effaddr c ea in
                  fold_operand c op
               | I_NOP | I_JMP _ | I_JCC _ | I_MFENCE -> c
               | I_EFF (_, _, eff) -> fold_effaddr c eff
               | I_EFF_EFF (_, _, ea1, ea2) ->
                  let c = fold_effaddr c ea1 in
                  fold_effaddr c ea2
               | I_CMPXCHG (_, ea,_) ->
                  fold_effaddr c ea
               | I_CMOVC (_, _,eff) ->
                  fold_effaddr c eff
               | I_LOCK ins -> fold_addrs f c ins

let rec map_addrs f =

  let map_rm64 x = match x with
    | Rm64_reg _| Rm64_deref _ -> x
    | Rm64_abs v -> Rm64_abs (f v) in

  let map_effaddr = function
    | Effaddr_rm64 rm -> Effaddr_rm64 (map_rm64 rm) in

  let map_operand op = match op with
    | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
    | Operand_immediate _ -> op in

  fun ins -> match ins with
             | I_EFF_OP(inst, s, ea, op) ->
                I_EFF_OP (inst, s, map_effaddr ea, map_operand op)
             | I_NOP | I_JMP _ | I_JCC _| I_MFENCE -> ins
             | I_EFF (inst, s, ea) ->
                I_EFF (inst, s, map_effaddr ea)
             | I_EFF_EFF (inst, s, ea1, ea2) ->
                I_EFF_EFF (inst, s, map_effaddr ea1, map_effaddr ea2)
             | I_CMPXCHG (s, ea,r) ->
                I_CMPXCHG (s, map_effaddr ea,r)
             | I_CMOVC (s, reg,eff) ->
                I_CMOVC (s, reg, map_effaddr eff)
             | I_LOCK ins ->
                I_LOCK (map_addrs f ins)

let norm_ins ins = ins

let rec get_next = function
  | I_NOP | I_EFF_OP _ | I_MFENCE
    | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
    | I_CMOVC _ -> [Label.Next]
  | I_JMP lbl-> [Label.To lbl]
  | I_JCC (_,lbl) -> [Label.Next; Label.To lbl]
  | I_LOCK ins -> get_next ins

include Pseudo.Make
          (struct
            type ins = instruction
            type pins = parsedInstruction
            type reg_arg = reg

            let parsed_tr i = i

            let rec get_naccesses = function
              | I_EFF_OP (_, _, ea, op)
                -> get_naccs_eff ea + get_naccs_op op
              | I_NOP | I_MFENCE | I_JMP _ | I_JCC _ -> 0
              | I_EFF (I_SETNB, _, e) -> get_naccs_eff e
              | I_EFF (_, _, e) | I_CMPXCHG (_, e, _) -> 2 * get_naccs_eff e
              | I_EFF_EFF (_, _, e1, e2) ->
                 2 * (get_naccs_eff e1 + get_naccs_eff e2)
              | I_CMOVC (_, _, e)
                -> get_naccs_eff e
              | I_LOCK i -> get_naccesses i

            let rec fold_labels k f = function
              | I_LOCK ins -> fold_labels k f ins
              | I_JMP lbl | I_JCC (_, lbl)-> f k lbl
              | I_NOP | I_EFF_OP _ | I_MFENCE
                | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
                | I_CMOVC _ -> k

            let rec map_labels f ins = match ins with
              | I_LOCK ins -> I_LOCK (map_labels f ins)
              | I_JMP lbl | I_JCC (_, lbl) -> I_JMP (f lbl)
              | I_NOP | I_EFF_OP _ | I_MFENCE
                | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
                | I_CMOVC _ -> ins

          end)

let get_macro _name = raise Not_found

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"
