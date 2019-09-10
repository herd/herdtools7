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

(** Define registers, barriers, and instructions for X86 *)

open Printf


(* Who am I ? *)
let arch = Archs.x86
let endian = Endian.Little

(*************)
(* Registers *)
(*************)

type reg =
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
(* we do 32-bit protected mode for now*)
  | ZF | SF | CF
(* Wastefully putting 31 bits instead of 1 bit *)
  | Symbolic_reg of string
  | Internal of int

let loop_idx = Internal 0
let sig_cell = "sig_cell"

let pc = EIP

let regs =
  [
   EAX, "EAX" ;
   EBX, "EBX" ;
   ECX, "ECX" ;
   EDX, "EDX" ;
   ESI, "ESI" ;
   EDI, "EDI" ;
   EBP, "EBP" ;
   ESP, "ESP" ;
   EIP, "EIP" ;
   ZF, "ZF" ;
   SF, "SF" ;
   CF, "CF" ;
 ]

let parse_list = List.map (fun (r,s) -> s,r) regs

let parse_reg s =
  try Some (List.assoc s parse_list)
  with Not_found -> None

let pp_reg r = match r with
| Symbolic_reg r -> "%"^r
| Internal i -> sprintf "i%i" i
| _ -> try List.assoc r regs with Not_found -> assert false

let reg_compare = compare (* Will do, no doubt *)

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r
let typeof c = assert false

(************)
(* Barriers *)
(************)

type barrier =
  | Lfence | Sfence | Mfence

let all_kinds_of_barriers = [ Lfence ; Sfence ; Mfence ; ]

let pp_barrier b = match b with
| Lfence -> "LFENCE"
| Sfence -> "SFENCE"
| Mfence -> "MFENCE"

let barrier_compare = compare


(****************)
(* Instructions *)
(****************)

type abs = ParsedConstant.v

type rm32 =
  |  Rm32_reg of reg
  |  Rm32_deref of reg
  |  Rm32_abs of abs
(* Absolute memory location, we should later combine with Rm32_deref to have proper base-displacement (and later, scale-index) addressing *)

type effaddr =
  | Effaddr_rm32 of rm32

type operand =
  | Operand_effaddr of effaddr
  | Operand_immediate of int

let get_naccs_rm32 = function
  |  Rm32_reg _ -> 0
  |  Rm32_deref _
  |  Rm32_abs _ -> 1

let get_naccs_eff  = function
  | Effaddr_rm32 rm -> get_naccs_rm32 rm

let get_naccs_op = function
  | Operand_immediate _ -> 0
  | Operand_effaddr e -> get_naccs_eff e

type condition =
  | C_EQ
  | C_NE
  | C_LE
  | C_LT
  | C_GT
  | C_GE
  | C_S          (* Sign *)
  | C_NS         (* Not sign *)

type lbl = Label.t

type instruction =
  | I_NOP
  | I_ADD of effaddr * operand
  | I_XOR of effaddr * operand
  | I_OR of effaddr * operand
  | I_MOV of effaddr * operand
  | I_DEC of effaddr
  | I_CMP of effaddr * operand
  | I_CMOVC of reg * effaddr
  | I_INC of effaddr
  | I_JMP of lbl
  | I_JCC of condition * lbl
  | I_LOCK of instruction
  | I_XCHG of effaddr * effaddr
  | I_XCHG_UNLOCKED of effaddr * effaddr
  | I_CMPXCHG of effaddr * reg
  | I_READ of operand  (* pseudo-instruction that just does a read *)
  | I_SETNB of effaddr
  | I_LFENCE
  | I_SFENCE
  | I_MFENCE
(* various sizes of move: 1, 2, 4, 8, 10 bytes respectively *)
  | I_MOVB of effaddr * operand
  | I_MOVW of effaddr * operand
  | I_MOVL of effaddr * operand
  | I_MOVQ of effaddr * operand
  | I_MOVT of effaddr * operand
(* 64 bits mem-mem "string" move (operands in esi and edi) *)
  | I_MOVSD

type parsedInstruction = instruction


let pp_abs = ParsedConstant.pp_v

let pp_rm32 rm32 =
  match rm32 with
  | Rm32_reg r -> pp_reg r
  | Rm32_deref r -> "[" ^ pp_reg r ^ "]"
  | Rm32_abs v -> "[" ^ pp_abs v ^ "]"


let pp_effaddr ea =  match ea with
| Effaddr_rm32 rm32 -> pp_rm32 rm32

      (* Those may change *)
type basic_pp =
    { immediate : int -> string ;
      comma : string ; }

(* Old, mode dependant,  printing of constants *)

open PPMode

let pp_dollar m = match m with
| Ascii|Dot -> "$"
| Latex -> "\\$"
| DotFig -> "\\\\$"

(* As pointed out by Scott, X86 should not have dollar in Intel syntax *)
let pp_immediate _m v = (* pp_dollar m ^ *) string_of_int v

    (* Old, mode dependant,  printing of a comma *)
let x86_comma m= match m with
| Ascii|Dot -> ","
| Latex -> "\\m "
| DotFig -> "\\\\m "

let pp_condition c =  match c with
| C_EQ  -> "E"
| C_NE  -> "NE"
| C_LE  -> "LE"
| C_LT  -> "L"
| C_GT  -> "G"
| C_GE  -> "GE"
| C_S   -> "S"
| C_NS  -> "NS"




let rec do_pp_instruction m =

  let pp_operand  op = match op with
  | Operand_effaddr(ea) -> pp_effaddr ea
  | Operand_immediate(v) -> m.immediate v in


  let ppi_ea_r opcode ea r =
    opcode^" "^pp_effaddr ea ^ m.comma ^pp_reg r in
  let ppi_ea_op opcode ea op =
    opcode^" "^pp_effaddr ea ^ m.comma ^pp_operand op in
  let ppi_ea_ea opcode ea1 ea2 =
    opcode^" "^pp_effaddr ea1 ^ m.comma ^pp_effaddr ea2 in
  let ppi_ea opcode ea = opcode^" "^pp_effaddr ea in
  let ppi_op  opcode op = opcode^" " ^ pp_operand op in
  let ppi_lbl opcode lbl = opcode^" " ^ lbl in
  let ppi_c_lbl opcode c lbl =
    opcode ^ pp_condition c ^ " " ^ lbl in


  fun i -> match i with
  | I_NOP -> "NOP"
  | I_ADD(ea,op) -> ppi_ea_op "ADD" ea op
  | I_XOR(ea,op) -> ppi_ea_op "XOR" ea op
  | I_OR(ea,op) -> ppi_ea_op "OR" ea op
  | I_MOV(ea,op) -> ppi_ea_op "MOV" ea op
  | I_MOVB(ea,op) -> ppi_ea_op "MOVB" ea op
  | I_MOVW(ea,op) -> ppi_ea_op "MOVW" ea op
  | I_MOVL(ea,op) -> ppi_ea_op "MOVL" ea op
  | I_MOVQ(ea,op) -> ppi_ea_op "MOVQ" ea op
  | I_MOVT(ea,op) -> ppi_ea_op "MOVT" ea op
  | I_MOVSD   -> "MOVSD"
  | I_DEC(ea) -> ppi_ea "DEC " ea
  | I_CMP(ea,op) -> ppi_ea_op "CMP " ea op
  | I_CMOVC(r,ea) -> "CMOVC " ^pp_reg r ^ m.comma ^pp_effaddr ea
  | I_INC(ea) -> ppi_ea "INC " ea
  | I_JMP(lbl) -> ppi_lbl "JMP" lbl
  | I_JCC(c,lbl) -> ppi_c_lbl "J" c lbl
  | I_LOCK(i) -> "LOCK; " ^ do_pp_instruction m i
  | I_XCHG(ea1,ea2) -> ppi_ea_ea "XCHG" ea1 ea2
  | I_XCHG_UNLOCKED(ea1,ea2) -> ppi_ea_ea "UNLOCKED XCHG" ea1 ea2
  | I_CMPXCHG (ea,r) -> ppi_ea_r "CMPXCHG" ea r
  | I_READ(op) -> ppi_op "READ" op
  | I_SETNB(ea) -> ppi_ea "SETNB" ea
  | I_LFENCE  -> "LFENCE"
  | I_SFENCE  -> "SFENCE"
  | I_MFENCE  -> "MFENCE"


let pp_instruction m i =
  do_pp_instruction
    {immediate = pp_immediate m ;
     comma = x86_comma m ; } i

let dump_instruction =
  do_pp_instruction
    { immediate = (fun v -> "$" ^ string_of_int v) ;
      comma = "," ; }

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb =
  [ EAX ; EBX ;  ECX ;
    EDX ;  ESI ;  EDI ; ]



let rec fold_regs (f_reg,f_sreg) =

  let fold_reg (y_reg,y_sreg) reg = match reg with
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | ZF | SF | CF ->  f_reg reg y_reg,y_sreg
  | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
  | Internal _ -> y_reg,y_sreg in

  let fold_rm32 c = function
    | Rm32_reg reg | Rm32_deref reg -> fold_reg c reg
    | Rm32_abs _ -> c in

  let fold_effaddr c = function
    | Effaddr_rm32 rm -> fold_rm32 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins -> match ins with
  | I_XOR (eff,op)
  | I_OR (eff,op)
  | I_ADD (eff,op)
  | I_MOV (eff,op) | I_MOVB (eff,op) | I_MOVW (eff,op) | I_MOVL (eff,op) | I_MOVQ (eff,op) | I_MOVT (eff,op)
  | I_CMP (eff,op) ->
      let c = fold_effaddr c eff in
      fold_operand c op
  | I_DEC eff
  | I_INC eff
  | I_SETNB eff ->
      fold_effaddr c eff
  | I_CMOVC (reg,eff) ->
      let c = fold_reg c reg in
      fold_effaddr c eff
  | I_READ op ->
      fold_operand c op
  | I_XCHG_UNLOCKED  (ea1, ea2)|I_XCHG (ea1, ea2) ->
      let c = fold_effaddr c ea1 in
      fold_effaddr c ea2
  | I_CMPXCHG (ea,r) ->
      let c = fold_effaddr c ea in
      fold_reg c r
  | I_LOCK ins -> fold_regs (f_reg,f_sreg) c ins
  | I_NOP
  | I_JCC _
  | I_JMP _
  | I_MFENCE|I_SFENCE|I_LFENCE
  | I_MOVSD
    -> c


let rec map_regs f_reg f_symb =

  let map_reg  reg = match reg with
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | ZF | SF | CF | Internal _ ->  f_reg reg
  | Symbolic_reg reg -> f_symb reg in

  let map_rm32 = function
    | Rm32_reg reg -> Rm32_reg (map_reg reg)
    | Rm32_deref reg -> Rm32_deref (map_reg reg)
    | Rm32_abs _ as rm -> rm in

  let map_effaddr = function
    | Effaddr_rm32 rm -> Effaddr_rm32 (map_rm32 rm) in

  let map_operand op = match op with
  | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
  | Operand_immediate _ -> op in

  fun ins -> match ins with
  | I_XOR (eff,op) ->
      I_XOR (map_effaddr eff, map_operand op)
  | I_OR (eff,op) ->
      I_OR (map_effaddr eff, map_operand op)
  | I_ADD (eff,op) ->
      I_ADD (map_effaddr eff, map_operand op)
  | I_MOV (eff,op) ->
      I_MOV (map_effaddr eff, map_operand op)
  | I_MOVB (eff,op) ->
      I_MOVB (map_effaddr eff, map_operand op)
  | I_MOVW (eff,op) ->
      I_MOVW (map_effaddr eff, map_operand op)
  | I_MOVL (eff,op) ->
      I_MOVL (map_effaddr eff, map_operand op)
  | I_MOVQ (eff,op) ->
      I_MOVQ (map_effaddr eff, map_operand op)
  | I_MOVT (eff,op) ->
      I_MOVT (map_effaddr eff, map_operand op)
  | I_CMP (eff,op) ->
      I_CMP (map_effaddr eff, map_operand op)
  | I_DEC eff ->
      I_DEC (map_effaddr eff)
  | I_INC eff ->
      I_INC  (map_effaddr eff)
  | I_SETNB eff ->
      I_SETNB (map_effaddr eff)
  | I_CMOVC (reg,eff) ->
      I_CMOVC (map_reg reg, map_effaddr eff)
  | I_READ op ->
      I_READ (map_operand op)
  | I_XCHG_UNLOCKED  (ea1, ea2) ->
      I_XCHG_UNLOCKED (map_effaddr ea1,map_effaddr ea2)
  |I_XCHG (ea1, ea2) ->
      I_XCHG (map_effaddr ea1,map_effaddr ea2)
  | I_CMPXCHG (ea,r) ->
      I_CMPXCHG (map_effaddr ea,map_reg r)
  | I_LOCK ins ->
      I_LOCK (map_regs f_reg f_symb ins)
  | I_NOP
  | I_JCC _| I_JMP _
  | I_MFENCE|I_SFENCE|I_LFENCE
  | I_MOVSD
    -> ins


let rec fold_addrs f =

  let fold_rm32 c = function
    | Rm32_reg _ | Rm32_deref _ -> c
    | Rm32_abs v -> f v c in

  let fold_effaddr c = function
    | Effaddr_rm32 rm -> fold_rm32 c rm in

  let fold_operand c = function
    | Operand_effaddr e -> fold_effaddr c e
    | Operand_immediate _ -> c in

  fun c ins -> match ins with
  | I_XOR (eff,op)
  | I_OR (eff,op)
  | I_ADD (eff,op)
  | I_MOV (eff,op)
  | I_MOVB (eff,op)
  | I_MOVW (eff,op)
  | I_MOVL (eff,op)
  | I_MOVQ (eff,op)
  | I_MOVT (eff,op)
  | I_CMP (eff,op) ->
      let c = fold_effaddr c eff in
      fold_operand c op
  | I_DEC eff
  | I_INC eff
  | I_SETNB eff ->
      fold_effaddr c eff
  | I_CMOVC (_,eff) ->
      fold_effaddr c eff
  | I_READ op ->
      fold_operand c op
  | I_XCHG_UNLOCKED  (ea1, ea2)|I_XCHG (ea1, ea2) ->
      let c = fold_effaddr c ea1 in
      fold_effaddr c ea2
  | I_CMPXCHG (ea,_) ->
      fold_effaddr c ea
  | I_LOCK ins -> fold_addrs f c ins
  | I_NOP
  | I_JCC _
  | I_JMP _
  | I_MFENCE|I_SFENCE|I_LFENCE
  | I_MOVSD
    -> c

let rec map_addrs f =

  let map_rm32 x = match x with
    | Rm32_reg _| Rm32_deref _ -> x
    | Rm32_abs v -> Rm32_abs (f v) in

  let map_effaddr = function
    | Effaddr_rm32 rm -> Effaddr_rm32 (map_rm32 rm) in

  let map_operand op = match op with
  | Operand_effaddr ea ->  Operand_effaddr (map_effaddr ea)
  | Operand_immediate _ -> op in

  fun ins -> match ins with
  | I_XOR (eff,op) ->
      I_XOR (map_effaddr eff, map_operand op)
  | I_OR (eff,op) ->
      I_OR (map_effaddr eff, map_operand op)
  | I_ADD (eff,op) ->
      I_ADD (map_effaddr eff, map_operand op)
  | I_MOV (eff,op) ->
      I_MOV (map_effaddr eff, map_operand op)
  | I_MOVB (eff,op) ->
      I_MOVB (map_effaddr eff, map_operand op)
  | I_MOVW (eff,op) ->
      I_MOVW (map_effaddr eff, map_operand op)
  | I_MOVL (eff,op) ->
      I_MOVL (map_effaddr eff, map_operand op)
  | I_MOVQ (eff,op) ->
      I_MOVQ (map_effaddr eff, map_operand op)
  | I_MOVT (eff,op) ->
      I_MOVT (map_effaddr eff, map_operand op)
  | I_CMP (eff,op) ->
      I_CMP (map_effaddr eff, map_operand op)
  | I_DEC eff ->
      I_DEC (map_effaddr eff)
  | I_INC eff ->
      I_INC  (map_effaddr eff)
  | I_SETNB eff ->
      I_SETNB (map_effaddr eff)
  | I_CMOVC (reg,eff) ->
      I_CMOVC (reg, map_effaddr eff)
  | I_READ op ->
      I_READ (map_operand op)
  | I_XCHG_UNLOCKED  (ea1, ea2) ->
      I_XCHG_UNLOCKED (map_effaddr ea1,map_effaddr ea2)
  |I_XCHG (ea1, ea2) ->
      I_XCHG (map_effaddr ea1,map_effaddr ea2)
  | I_CMPXCHG (ea,r) ->
      I_CMPXCHG (map_effaddr ea,r)
  | I_LOCK ins ->
      I_LOCK (map_addrs f ins)
  | I_NOP
  | I_JCC _| I_JMP _
  | I_MFENCE|I_SFENCE|I_LFENCE
  | I_MOVSD
    -> ins


let norm_ins ins = match ins with
| I_MOVB (eff,op) -> I_MOV (eff,op)
| I_MOVW (eff,op) -> I_MOV (eff,op)
| I_MOVL (eff,op) -> I_MOV (eff,op)
| I_MOVQ (eff,op) -> I_MOV (eff,op)
| I_MOVT (eff,op) -> I_MOV (eff,op)
| _ -> ins

(* PLDI submission, complete later ? *)
let is_data _ _ = assert false

(* Instruction continuation *)
let rec get_next = function
  | I_LOCK ins -> get_next ins
  | I_NOP
  | I_ADD _
  | I_XOR _ | I_OR _
  | I_MOV _ | I_MOVB _ | I_MOVW _ | I_MOVL _ | I_MOVQ _ | I_MOVT _
  | I_MOVSD
  | I_DEC _
  | I_CMP _
  | I_CMOVC _
  | I_INC _
  | I_XCHG _
  | I_XCHG_UNLOCKED _
  | I_CMPXCHG _
  | I_READ _
  | I_SETNB _
  | I_LFENCE
  | I_SFENCE -> [Label.Next]
  | I_MFENCE -> [Label.Next]
  | I_JMP lbl-> [Label.To lbl]
  | I_JCC (_,lbl) -> [Label.Next; Label.To lbl]

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let parsed_tr i = i

      let rec get_naccesses = function
        | I_LOCK i -> get_naccesses i
        | I_ADD (e,o)
        | I_XOR (e,o)
        | I_OR (e,o)
        | I_MOV (e,o)
        | I_CMP (e,o)
        | I_MOVB (e,o)
        | I_MOVW (e,o)
        | I_MOVL (e,o)
        | I_MOVQ (e,o)
        | I_MOVT (e,o)
          -> get_naccs_eff e + get_naccs_op o
        | I_DEC e
        | I_INC e
        | I_CMPXCHG (e,_)
            -> 2 * get_naccs_eff e

        | I_CMOVC (_,e)
              -> get_naccs_eff e
        | I_NOP
        | I_LFENCE
        | I_SFENCE
        | I_MFENCE
        | I_JMP _
        | I_JCC _ -> 0
        | I_XCHG (e1,e2)
        | I_XCHG_UNLOCKED (e1,e2)
          ->
            2 * (get_naccs_eff e1 + get_naccs_eff e2)
        | I_READ o
            -> get_naccs_op o
        | I_SETNB e
            -> get_naccs_eff e
        | I_MOVSD -> 2

      let rec fold_labels k f = function
        | I_LOCK ins -> fold_labels k f ins
        | I_JMP lbl
        | I_JCC (_,lbl)
          -> f k lbl
        | I_NOP
        | I_SETNB _|I_READ _|I_XCHG_UNLOCKED (_, _)|I_XCHG (_, _)|I_INC _
        | I_CMOVC (_, _)|I_CMP (_, _)|I_DEC _
        | I_MOV (_, _)|I_MOVB (_,_)|I_MOVW (_,_)
        |I_MOVL (_,_)|I_MOVQ (_,_)|I_MOVT (_,_)
        | I_MOVSD
        | I_XOR (_, _)|I_OR _|I_ADD (_, _)
        | I_MFENCE|I_SFENCE|I_LFENCE
        | I_CMPXCHG (_,_)
          -> k

      let rec map_labels f ins = match ins with
        | I_LOCK ins -> I_LOCK (map_labels f ins)
        | I_JMP lbl -> I_JMP (f lbl)
        | I_JCC (cc,lbl) -> I_JCC (cc,f lbl)
        | I_NOP
        | I_SETNB _|I_READ _|I_XCHG_UNLOCKED (_, _)|I_XCHG (_, _)|I_INC _
        | I_CMOVC (_, _)|I_CMP (_, _)|I_DEC _
        | I_MOV (_, _)|I_MOVB (_,_)|I_MOVW (_,_)|I_MOVL (_,_)
        |I_MOVQ (_,_)|I_MOVT (_,_)
        | I_MOVSD
        | I_XOR (_, _)|I_OR _|I_ADD (_, _)
        | I_MFENCE|I_SFENCE|I_LFENCE
        | I_CMPXCHG (_,_)
            -> ins

    end)

let get_macro _name = raise Not_found

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"
