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
let base_type = CType.Base "int"

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
  | R8bL | R8bH | R16b | R32b | R64b

(* Flags *)
type flag =
  | ZF | SF | CF

(* xmm registers *)
type xmm =
  | XMM0 | XMM1 | XMM2 | XMM3
  | XMM4 | XMM5 | XMM6 | XMM7

type reg =
  | RIP
  | CS
  | Ireg of base_reg * reg_part
  | XMM of xmm
  | Symbolic_reg of string
  | Internal of int
  | Flag of flag

let copy_part r1 r2 = match r1,r2 with
| Ireg (_,p),Ireg (r,_) -> Ireg (r,p)
| _,_ -> r2

let loop_idx = Internal 0
let sig_cell = "sig_cell"

let pc = RIP
let cs = CS
let gen_regs64 =
    [
(* 64b registers *)
    (AX, R64b), "rax";
    (BX, R64b), "rbx";
    (CX, R64b), "rcx";
    (DX, R64b), "rdx";
    (SI, R64b), "rsi";
    (DI, R64b), "rdi";
    (BP, R64b), "rbp";
    (SP, R64b), "rsp";
    (R8, R64b), "r8";
    (R9, R64b), "r9";
    (R10, R64b), "r10";
    (R11, R64b), "r11";
    (R12, R64b), "r12";
    (R13, R64b), "r13";
    (R14, R64b), "r14";
    (R15, R64b), "r15";
   ]
let gen_regs =
  gen_regs64 @
  [
(* 32b registers *)
    (AX, R32b), "eax";
    (BX, R32b), "ebx";
    (CX, R32b), "ecx";
    (DX, R32b), "edx";
    (SI, R32b), "esi";
    (DI, R32b), "edi";
    (BP, R32b), "ebp";
    (SP, R32b), "esp";
    (R8, R32b), "r8d";
    (R9, R32b), "r9d";
    (R10, R32b), "r10d";
    (R11, R32b), "r11d";
    (R12, R32b), "r12d";
    (R13, R32b), "r13d";
    (R14, R32b), "r14d";
    (R15, R32b), "r15d";
(* 16b registers *)
    (AX, R16b), "ax";
    (BX, R16b), "bx";
    (CX, R16b), "cx";
    (DX, R16b), "dx";
    (SI, R16b), "si";
    (DI, R16b), "di";
    (BP, R16b), "bp";
    (SP, R16b), "sp";
    (R8, R16b), "r8w";
    (R9, R16b), "r9w";
    (R10, R16b), "r10w";
    (R11, R16b), "r11w";
    (R12, R16b), "r12w";
    (R13, R16b), "r13w";
    (R14, R16b), "r14w";
    (R15, R16b), "r15w";
(* 8 low bits registers *)
    (AX, R8bL), "al";
    (BX, R8bL), "bl";
    (CX, R8bL), "cl";
    (DX, R8bL), "dl";
    (SI, R8bL), "sil";
    (DI, R8bL), "dil";
    (BP, R8bL), "bpl";
    (SP, R8bL), "spl";
    (R8, R8bL), "r8b";
    (R9, R8bL), "r9b";
    (R10, R8bL), "r10b";
    (R11, R8bL), "r11b";
    (R12, R8bL), "r12b";
    (R13, R8bL), "r13b";
    (R14, R8bL), "r14b";
    (R15, R8bL), "r15b";
(* 8 high bits registers *)
    (AX, R8bH), "ah";
    (BX, R8bH), "bh";
    (CX, R8bH), "ch";
    (DX, R8bH), "dh";
  ]

let flag_string =
  [
    Flag ZF, "zf";
    Flag SF, "sf";
    Flag CF, "cf";
  ]

let xmm_regs =
  [
   XMM0, "xmm0";
   XMM1, "xmm1";
   XMM2, "xmm2";
   XMM3, "xmm3";
   XMM4, "xmm4";
   XMM5, "xmm5";
   XMM7, "xmm6";
   XMM7, "xmm7";
 ]

let xmms = List.map fst xmm_regs

(* Match reg size with its nae in GCC asm inline *)
let reg_size_to_string = function
  | R8bL -> "b"
  | R8bH -> "h"
  | R16b -> "w"
  | R32b -> "k"
  | R64b -> "q"

let parse_list = [("cs",cs);("rip",pc)]@(List.map (fun ((r, t),s) -> s, Ireg (r, t)) gen_regs)

let parse_list64 = [("cs",cs); ("rip",pc)]@List.map (fun ((r, t),s) -> s, Ireg (r, t)) gen_regs64

let parse_list_xmm =  List.map (fun (xmm,s) -> s,xmm) xmm_regs

let regs = [(cs,"cs");(pc,"rip")]@List.map (fun ((r, t),s) -> Ireg (r, t), s) gen_regs

let reg_string r t = List.assoc (Ireg (r, t)) regs

let reg64_string r = reg_string r R64b

let xmm_string xmm = List.assoc xmm xmm_regs

let parse_reg s =
  try Some (List.assoc (Misc.lowercase s) parse_list64)
  with Not_found -> None

let parse_any_reg s =
  try Some (List.assoc (Misc.lowercase s) parse_list)
  with Not_found -> None

let parse_xmm_reg s =
   try Some (List.assoc (Misc.lowercase s) parse_list_xmm)
   with Not_found -> None


let reg_compare r1 r2 = match r1, r2 with
  | Ireg (b1, _), Ireg (b2, _) -> compare b1 b2
  | _ -> compare r1 r2

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r

let reg_size_to_uint = function
  | R8bL | R8bH -> "uint8_t"
  | R16b -> "uint16_t"
  | R32b -> "uint32_t"
  | R64b -> "uint64_t"

let type_reg = function
  | Ireg (_, t) -> CType.Base (reg_size_to_uint t)
  | _ -> CType.Base "int"

let change_size_reg r sz = match r with
  | Ireg (b, _) -> Ireg (b, sz)
  | _ -> r

let get_reg_size = function
  | Ireg (_, t) -> t
  | _ -> R64b

(************)
(* Barriers *)
(************)

type barrier =
  | MFENCE
  | SFENCE
  | LFENCE

let pp_barrier = function
  | MFENCE -> "mfence"
  | SFENCE -> "sfence"
  | LFENCE -> "lfence"

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
  |  Rm64_scaled of offset * reg * reg * offset

(* Absolute memory location, we should later combine with Rm64_deref to have proper base-displacement (and later, scale-index) addressing *)

type effaddr =
  | Effaddr_rm64 of rm64

type operand =
  | Operand_effaddr of effaddr
  | Operand_immediate of int

let get_naccs_rm64 = function
  |  Rm64_reg _ -> 0
  |  Rm64_deref _ |  Rm64_abs _
  |  Rm64_scaled _ -> 1

let get_naccs_eff  = function
  | Effaddr_rm64 rm -> get_naccs_rm64 rm

let get_naccs_op = function
  | Operand_immediate _ -> 0
  | Operand_effaddr e -> get_naccs_eff e

type inst_size =
  | I8b | I16b | I32b | I64b | INSb

type inst_eff_op =
  | I_ADD
  | I_OR
  | I_AND
  | I_XOR
  | I_MOV
  | I_CMP
  | I_SHL

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
  | I8b -> "b"
  | I16b -> "w"
  | I32b -> "l"
  | I64b -> "q"
  | INSb -> ""

let pp_inst_eff_op inst size =
  let inst_string = match inst with
    | I_ADD -> "add"
    | I_OR -> "or"
    | I_AND -> "and"
    | I_XOR -> "xor"
    | I_MOV -> "mov"
    | I_CMP -> "cmp"
    | I_SHL -> "shl" in
  inst_string ^ pp_inst_size size

let pp_inst_eff inst size =
  let inst_string = match inst with
    | I_DEC -> "dec"
    | I_INC -> "inc"
    | I_SETNB -> "setnb" in
  inst_string ^ pp_inst_size size

let pp_inst_eff_eff inst size =
  let inst_string = match inst with
    | I_XCHG -> "xchg"
    | I_XCHG_UNLOCKED -> "uxch" in
  inst_string ^ pp_inst_size size

let pp_movnti sz = sprintf "movnti%s" (pp_inst_size sz)

let  pp_movd sz =
  sprintf "mov%s"
    (match sz with
    | I32b -> "d"
    | I64b -> "q"
    | I8b|I16b|INSb -> assert false)

let pp_movntdqa = "movntdqa"

type opt = NoOpt|Opt

let pp_clflush opt = match opt with
  | NoOpt -> "clflush"
  | Opt -> "clflushopt"

type instruction =
  | I_NOP
  | I_EFF_OP of inst_eff_op * inst_size * effaddr * operand
  | I_EFF of inst_eff * inst_size * effaddr
  | I_EFF_EFF of inst_eff_eff * inst_size * effaddr * effaddr
  | I_CMPXCHG of inst_size * effaddr * reg
  | I_JMP of lbl
  | I_RET
  | I_JCC of condition * lbl
  | I_CMOVC of inst_size * reg * effaddr
  | I_LOCK of instruction
  | I_FENCE of barrier
(* Extra "Non Temporal" instructions *)
  | I_MOVNTI of inst_size * effaddr * reg
  | I_MOVNTDQA of xmm * effaddr
  | I_MOVD of inst_size * reg * xmm
(* Cache flush *)
  | I_CLFLUSH of opt * effaddr

type parsedInstruction = instruction

let pp_abs = ParsedConstant.pp_v

open PPMode

let pp_comma m = match m with
  | Ascii |Dot -> ","
  | Latex -> "\\m "
  | DotFig -> "\\\\m "

let pp_dollar m = match m with
  | Ascii |Dot -> "$"
  | Latex -> "\\$ "
  | DotFig -> "\\\\$ "

let pp_amper m = match m with
  | Ascii |Dot -> "%"
  | Latex -> "\\% "
  | DotFig -> "\\\\% "

type mm =
  {immediate : int -> string ;
   comma : string ;
   amper : string ; }


  let pp_condition = function
    | C_EQ  -> "e"
    | C_NE  -> "ne"
    | C_LE  -> "le"
    | C_LT  -> "l"
    | C_GT  -> "g"
    | C_GE  -> "ge"
    | C_S   -> "s"
    | C_NS  -> "ns"

  let pp_offset = function
    | 0 -> ""
    | a -> string_of_int a


let ascii_m =
 { immediate = (fun v -> "$" ^ string_of_int v) ;
   comma = "," ; amper = "%"; }

let pp_xmm r = try xmm_string r with Not_found -> assert false

let do_pp_reg amper r = match r with
  | Symbolic_reg r -> amper ^ r
  | Internal i -> sprintf "i%i" i
  | Flag _ -> (try List.assoc r flag_string with Not_found -> assert false)
  | XMM r -> pp_xmm r
  | _ -> try List.assoc r regs with Not_found -> assert false

let pp_reg r = do_pp_reg "%" r


let rec do_pp_instruction (m : mm) =

  let pp_reg r = match r with
  | Symbolic_reg r -> m.amper ^ r
  | Internal i -> sprintf "i%i" i
  | Flag _ -> (try List.assoc r flag_string with Not_found -> assert false)
  | _ -> sprintf "%s%s" m.amper (pp_reg r) in

  let pp_rm64 rm64  =
    match rm64 with
    | Rm64_reg r -> pp_reg r
    | Rm64_deref (r,o) -> pp_offset o ^ "(" ^ pp_reg r ^ ")"
    | Rm64_abs v -> "(" ^ pp_abs v ^ ")"
    | Rm64_scaled (o1,r1,r2,o2) -> pp_offset o1 ^ "(" ^ pp_reg r1
      ^ ", " ^ pp_reg r2 ^ "," ^ pp_offset o2 ^ ")" in

  let pp_effaddr ea =  match ea with
    | Effaddr_rm64 rm64 -> pp_rm64 rm64 in

  let pp_operand  op = match op with
    | Operand_effaddr(ea) -> pp_effaddr ea
    | Operand_immediate(v) -> m.immediate v in

  let ppi_inst_ea_op inst s ea op =
    pp_inst_eff_op inst s ^ " " ^  pp_operand op ^ m.comma ^ pp_effaddr ea in

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
           | I_RET -> "ret"
           | I_EFF_OP(inst, s, ea, op) -> ppi_inst_ea_op inst s ea op
           | I_EFF(inst, s, ea) -> ppi_ea inst s ea
           | I_EFF_EFF(inst, s, ea1, ea2) -> ppi_ea_ea inst s ea1 ea2
           | I_CMPXCHG (s, ea, r) -> ppi_ea_r "cmpxchg" s ea r
           | I_CMOVC (s, r, ea) ->  ppi_r_ea "cmovc" s r ea
           | I_LOCK inst -> "lock; " ^ do_pp_instruction m inst
           | I_JMP(lbl) -> ppi_lbl "jmp" lbl
           | I_JCC(cond, lbl) -> ppi_lbl ("j" ^ pp_condition cond) lbl
           | I_FENCE f -> pp_barrier f
           | I_MOVNTI (sz,ea,r) ->
               sprintf "%s %s,%s"
                 (pp_movnti sz)
                 (pp_reg r) (pp_effaddr ea)
           | I_MOVD (sz,r,xmm) ->
               sprintf "%s %s,%s" (pp_movd sz) (pp_xmm xmm) (pp_reg r)
           | I_MOVNTDQA (xmm,ea) ->
               sprintf "%s %s,%s" pp_movntdqa (pp_effaddr ea) (pp_xmm xmm)
           | I_CLFLUSH (opt,ea) ->
               sprintf "%s %s" (pp_clflush opt) (pp_effaddr ea)
let pp_instruction m i =
  do_pp_instruction
    {immediate = (fun x -> pp_dollar m ^ string_of_int x) ;
     comma = pp_comma m ;
     amper = pp_amper m ; } i

let dump_instruction = do_pp_instruction ascii_m

let dump_instruction_hash = dump_instruction

(****************************)
(* Symbolic registers stuff *)
(****************************)

let reg_size_p size = function
  | Ireg (_,t) -> t = size
  | _ -> false

let allowed_for_symb_size size = List.filter (reg_size_p size)
                         (List.map (fun ((r, t),_) -> Ireg (r, t)) gen_regs)

let allowed_for_symb = allowed_for_symb_size R64b

let rec fold_regs (f_reg,f_sreg) =

  let fold_reg (y_reg,y_sreg) reg = match reg with
    | RIP | CS | Ireg _ | Flag _ | XMM _ -> f_reg reg y_reg,y_sreg
    | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
    | Internal _ -> y_reg,y_sreg in

  let fold_xmm c xmm = fold_reg c (XMM xmm) in

  let fold_rm64 c = function
    | Rm64_reg reg | Rm64_deref (reg, _) -> fold_reg c reg
    | Rm64_scaled (_,r1,r2,_) -> fold_reg (fold_reg c r1) r2
    | Rm64_abs _ -> c in

  let fold_effaddr c = function
    | Effaddr_rm64 rm -> fold_rm64 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins ->
    match ins with
    | I_EFF_OP (_, _, ea, op) ->
        let c = fold_effaddr c ea in
        fold_operand c op
    | I_NOP | I_JMP _ | I_RET | I_JCC _ | I_FENCE _ -> c
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
    | I_MOVNTI (_,ea,r) ->
        fold_reg (fold_effaddr c ea) r
    | I_MOVD (_,r,xmm) -> fold_reg (fold_xmm c xmm) r
    | I_MOVNTDQA (xmm,effaddr) -> fold_effaddr (fold_xmm c xmm) effaddr
    | I_CLFLUSH (_,effaddr) -> fold_effaddr c effaddr

let rec map_regs f_reg f_symb =

  let f_reg r = copy_part r (f_reg r) in

  let map_reg reg = match reg with
  | RIP | CS | Ireg _ | Flag _ | Internal _ | XMM _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg in

  let map_xmm xmm = match map_reg (XMM xmm) with
  | XMM xmm -> xmm
  | _ -> Warn.fatal "xmm registers must be mapped to xmm registers" in

  let map_rm64 = function
    | Rm64_reg reg -> Rm64_reg (map_reg reg)
    | Rm64_deref (reg, o) -> Rm64_deref (map_reg reg,o)
    | Rm64_scaled (o1,r1,r2,o2) -> Rm64_scaled
      (o1,map_reg r1,map_reg r2, o2)
    | Rm64_abs _ as rm -> rm in

  let map_effaddr = function
    | Effaddr_rm64 rm -> Effaddr_rm64 (map_rm64 rm) in

  let map_operand op = match op with
    | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
    | Operand_immediate _ -> op in

  fun ins ->
  match ins with
  | I_EFF_OP(inst, s, ea, op) ->
      I_EFF_OP (inst, s, map_effaddr ea, map_operand op)
  | I_NOP | I_JMP _ | I_RET | I_JCC _| I_FENCE _ -> ins
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
  | I_MOVNTI (sz,ea,r) ->
      I_MOVNTI (sz,map_effaddr ea,map_reg r)
  | I_MOVD (sz,r,xmm) ->
      I_MOVD (sz,map_reg r,map_xmm xmm)
  | I_MOVNTDQA (xmm,ea) ->
      I_MOVNTDQA (map_xmm xmm,map_effaddr ea)
  | I_CLFLUSH (opt,ea) ->
      I_CLFLUSH (opt,map_effaddr ea)

let rec fold_addrs f =

  let fold_rm64 c = function
    | Rm64_reg _ | Rm64_deref _ | Rm64_scaled _ -> c
    | Rm64_abs v -> f v c in

  let fold_effaddr c = function
    | Effaddr_rm64 rm -> fold_rm64 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins ->
  match ins with
  | I_EFF_OP (_, _, ea, op) ->
      let c = fold_effaddr c ea in
      fold_operand c op
  | I_NOP | I_JMP _ | I_RET | I_JCC _ | I_FENCE _ |I_MOVD _ -> c
  | I_EFF (_, _, eff) -> fold_effaddr c eff
  | I_EFF_EFF (_, _, ea1, ea2) ->
      let c = fold_effaddr c ea1 in
      fold_effaddr c ea2
  | I_CMPXCHG (_, ea,_) ->
      fold_effaddr c ea
  | I_CMOVC (_, _,eff)|I_MOVNTI (_,eff,_)|I_MOVNTDQA (_,eff)
  | I_CLFLUSH (_,eff)
    ->
      fold_effaddr c eff
  | I_LOCK ins -> fold_addrs f c ins

let rec map_addrs f =

  let map_rm64 x = match x with
    | Rm64_reg _| Rm64_deref _ | Rm64_scaled _ -> x
    | Rm64_abs v -> Rm64_abs (f v) in

  let map_effaddr = function
    | Effaddr_rm64 rm -> Effaddr_rm64 (map_rm64 rm) in

  let map_operand op = match op with
    | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
    | Operand_immediate _ -> op in

  fun ins ->
  match ins with
  | I_EFF_OP(inst, s, ea, op) ->
      I_EFF_OP (inst, s, map_effaddr ea, map_operand op)
  | I_NOP | I_JMP _ | I_RET | I_JCC _| I_FENCE _| I_MOVD _ -> ins
  | I_EFF (inst, s, ea) ->
      I_EFF (inst, s, map_effaddr ea)
  | I_EFF_EFF (inst, s, ea1, ea2) ->
      I_EFF_EFF (inst, s, map_effaddr ea1, map_effaddr ea2)
  | I_CMPXCHG (s, ea,r) ->
      I_CMPXCHG (s, map_effaddr ea,r)
  | I_CMOVC (s, reg,eff) ->
      I_CMOVC (s, reg, map_effaddr eff)
  | I_MOVNTI (sz,ea,r) ->
      I_MOVNTI (sz,map_effaddr ea,r)
  | I_MOVNTDQA (xmm,ea) ->
      I_MOVNTDQA (xmm,map_effaddr ea)
  | I_CLFLUSH (opt,ea) ->
      I_CLFLUSH (opt,map_effaddr ea)
  | I_LOCK ins ->
      I_LOCK (map_addrs f ins)


let rec get_next = function
  | I_NOP | I_EFF_OP _ | I_FENCE _
    | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
    | I_CMOVC _|I_MOVNTI _ | I_MOVD _ | I_MOVNTDQA _
    | I_CLFLUSH _ | I_RET
      -> [Label.Next]
    | I_JMP lbl-> [Label.To lbl]
    | I_JCC (_,lbl) -> [Label.Next; Label.To lbl]
    | I_LOCK ins -> get_next ins

include InstrUtils.No(struct type instr = instruction end)

include Pseudo.Make
          (struct
            type ins = instruction
            type pins = parsedInstruction
            type reg_arg = reg

            let parsed_tr i = i

            let rec get_naccesses = function
              | I_EFF_OP (_, _, ea, op)
                -> get_naccs_eff ea + get_naccs_op op
              | I_NOP | I_FENCE _ | I_JMP _ | I_RET | I_JCC _| I_MOVD _ -> 0
              | I_EFF (I_SETNB, _, e) -> get_naccs_eff e
              | I_EFF (_, _, e) | I_CMPXCHG (_, e, _) -> 2 * get_naccs_eff e
              | I_EFF_EFF (_, _, e1, e2) ->
                 2 * (get_naccs_eff e1 + get_naccs_eff e2)
              | I_CMOVC (_, _, e)|I_MOVNTI (_,e,_)|I_MOVNTDQA (_,e)
              | I_CLFLUSH (_,e)
                -> get_naccs_eff e
              | I_LOCK i -> get_naccesses i

(* This is incorrect as the size of instructions varies.
 * However a wrong value should generally be harmless, except
 * for litmus with option `-variant self` and for initial label
 * values
 *)
            let size_of_ins _ = 4

            let rec fold_labels k f = function
              | I_LOCK ins -> fold_labels k f ins
              | I_JMP lbl | I_JCC (_, lbl)-> f k lbl
              | I_NOP | I_RET | I_EFF_OP _ | I_FENCE _
              | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
              | I_CMOVC _|I_MOVNTI _|I_MOVD _|I_MOVNTDQA _
              | I_CLFLUSH _
                -> k

            let rec map_labels f ins = match ins with
              | I_LOCK ins ->
                 I_LOCK (map_labels f ins)
              | I_JMP lbl ->
                 I_JMP (BranchTarget.as_string_fun f lbl)
              | I_JCC (cc,lbl) ->
                 I_JCC (cc,BranchTarget.as_string_fun f lbl)
              | I_NOP | I_RET | I_EFF_OP _ | I_FENCE _
              | I_EFF_EFF _ | I_EFF _ | I_CMPXCHG _
              | I_CMOVC _|I_MOVNTI _|I_MOVD _|I_MOVNTDQA _
              | I_CLFLUSH _ -> ins

          end)

let get_macro _name = raise Not_found

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"

let hash_pteval _ = assert false

module Instr = Instr.No(struct type instr = instruction end)
