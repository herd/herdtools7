(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Copyright (c) 2024 Puranjay Mohan <puranjay@kernel.org>                  *)
(*                                                                          *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** BPF architecture, base definitions *)

open Printf
open Sign

let arch = Archs.bpf
let endian = Endian.Little
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type ireg =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10

type reg =
  | IReg of ireg
  | PC
  | Symbolic_reg of string
  | Internal of int

let parse_ireg = function
  | "R0" | "r0" -> R0
  | "R1" | "r1" -> R1
  | "R2" | "r2" -> R2
  | "R3" | "r3" -> R3
  | "R4" | "r4" -> R4
  | "R5" | "r5" -> R5
  | "R6" | "r6" -> R6
  | "R7" | "r7" -> R7
  | "R8" | "r8" -> R8
  | "R9" | "r9" -> R9
  | "R10" | "r10" | "FP" | "fp" -> R10
  | _ -> raise Exit
;;

let parse_reg s =
  try Some (IReg (parse_ireg s)) with
  | Exit -> None
;;

open PPMode

let do_pp_ireg = function
  | R0 -> "r0"
  | R1 -> "r1"
  | R2 -> "r2"
  | R3 -> "r3"
  | R4 -> "r4"
  | R5 -> "r5"
  | R6 -> "r6"
  | R7 -> "r7"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "fp"
;;

let pp_reg = function
  | IReg r -> do_pp_ireg r
  | Symbolic_reg r -> r
  | Internal i -> sprintf "ir%i" i
  | PC -> "pc"
;;

let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None
;;

let symb_reg r = Symbolic_reg r
let type_reg _ = base_type

(************)
(* Barriers *)
(************)

(* Artificial sync instruction for testing *)
type barrier = Sync

let all_kinds_of_barriers = [ Sync ]

let pp_barrier = function
  | Sync -> "Sync"
;;

let barrier_compare = compare

(***************)
(* Annotations *)
(***************)

(* X  -> Atomic without any ordering *)
(* SC -> Atomic with full ordering *)
(* N  -> Plain: non-atomic and no ordering *)
(* A  -> Non-atomic and acquire ordering *)
(* R  -> Non-atomic and release ordering *)
type lannot =
  | X
  | SC
  | N
  | A
  | R

(****************)
(* Instructions *)
(****************)

type k = int
type lbl = Label.t

type op =
  | ADD
  | SUB
  | MUL
  | DIV
  | REM
  | AND
  | OR
  | XOR
  | LSL
  | LSR
  | ASR
  | AMOXCHG
  | AMOCMPXCHG

type width =
  | Byte
  | Half
  | Word
  | Double

let tr_width = function
  | Byte -> MachSize.Byte
  | Half -> MachSize.Short
  | Word -> MachSize.Word
  | Double -> MachSize.Quad
;;

type signed = Sign.t

type cond =
  | EQ
  | NE
  | LT
  | GE

type instruction =
  | OP of op * reg * reg
  | OPI of op * reg * k
  | LOAD of width * signed * reg * reg * k
  | LDAQ of width * reg * reg * k
  | STORE of width * reg * k * reg
  | STOREI of width * reg * k * k
  | STRL of width * reg * k * reg
  | MOV of reg * reg
  | MOVI of reg * k
  | AMO of op * width * reg * k * reg * lannot * bool
  | GOTO of lbl
  | JCOND of cond * reg * reg * lbl
  | JCONDI of cond * reg * k * lbl

type parsedInstruction = instruction

let pp_lbl i = i

let pp_op = function
  | ADD -> "+="
  | SUB -> "-="
  | MUL -> "*="
  | DIV -> "/="
  | REM -> "%="
  | AND -> "&="
  | OR -> "|="
  | XOR -> "^="
  | LSL -> "<<="
  | LSR -> ">>="
  | ASR -> "s>>="
  | AMOXCHG | AMOCMPXCHG -> assert false
;;

let pp_width = function
  | Byte -> "8"
  | Half -> "16"
  | Word -> "32"
  | Double -> "64"
;;

let pp_sign = function
  | Unsigned -> "u"
  | Signed -> "s"
;;

let pp_k k = if k < 0 then "- " ^ string_of_int (-k) else "+ " ^ string_of_int k

let pp_amo_xchg_s sz =
  match sz with
  | Word -> "32_32"
  | Double -> "_64"
  | _ -> assert false
;;

let pp_amo_op_f op sz r1 k r2 =
  match op with
  | AMOXCHG ->
    sprintf
      "%s = xchg%s(%s %s, %s)"
      (pp_reg r2)
      (pp_amo_xchg_s sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | AMOCMPXCHG ->
    sprintf
      "r0 = cmpxchg%s(%s %s, r0, %s)"
      (pp_amo_xchg_s sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | ADD ->
    sprintf
      "%s = atomic_fetch_add((u%s *)(%s %s), %s)"
      (pp_reg r2)
      (pp_width sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | OR ->
    sprintf
      "%s = atomic_fetch_or((u%s *)(%s %s), %s)"
      (pp_reg r2)
      (pp_width sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | AND ->
    sprintf
      "%s = atomic_fetch_and((u%s *)(%s %s), %s)"
      (pp_reg r2)
      (pp_width sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | XOR ->
    sprintf
      "%s = atomic_fetch_xor((u%s *)(%s %s), %s)"
      (pp_reg r2)
      (pp_width sz)
      (pp_reg r1)
      (pp_k k)
      (pp_reg r2)
  | _ -> assert false
;;

let pp_cond = function
  | EQ -> "=="
  | NE -> "!="
  | LT -> "<"
  | GE -> ">="
;;

let pp_instruction _m i =
  match i with
  | MOV (r1, r2) -> sprintf "%s = %s" (pp_reg r1) (pp_reg r2)
  | MOVI (r1, k) -> sprintf "%s = %i" (pp_reg r1) k
  | OP (op, r1, r2) -> sprintf "%s %s %s" (pp_reg r1) (pp_op op) (pp_reg r2)
  | OPI (op, r1, k) -> sprintf "%s %s %i" (pp_reg r1) (pp_op op) k
  | LOAD (w, s, r1, r2, k) ->
    sprintf
      "%s = *(%s%s *) (%s %s)"
      (pp_reg r1)
      (pp_sign s)
      (pp_width w)
      (pp_reg r2)
      (pp_k k)
  | LDAQ (w, r1, r2, k) ->
    sprintf
      "%s = load_acquire((u%s *) (%s %s))"
      (pp_reg r1)
      (pp_width w)
      (pp_reg r2)
      (pp_k k)
  | STORE (w, r1, k, r2) ->
    sprintf "*(u%s *)(%s %s) = %s" (pp_width w) (pp_reg r1) (pp_k k) (pp_reg r2)
  | STOREI (w, r1, k1, k2) ->
    sprintf "*(u%s *)(%s %s) = %i" (pp_width w) (pp_reg r1) (pp_k k1) k2
  | STRL (w, r1, k, r2) ->
    sprintf "store_release ((u%s *)(%s %s), %s)" (pp_width w) (pp_reg r1) (pp_k k) (pp_reg r2)
  | AMO (op, sz, r1, k, r2, _an, f) ->
    if f
    then pp_amo_op_f op sz r1 k r2
    else
      sprintf
        "lock *(u%s *)(%s %s) %s %s"
        (pp_width sz)
        (pp_reg r1)
        (pp_k k)
        (pp_op op)
        (pp_reg r2)
  | GOTO lbl -> sprintf "goto %s" (pp_lbl lbl)
  | JCOND (cond, r1, r2, lbl) ->
    sprintf "if %s %s %s goto %s" (pp_reg r1) (pp_cond cond) (pp_reg r2) (pp_lbl lbl)
  | JCONDI (cond, r1, k, lbl) ->
    sprintf "if %s %s %i goto %s" (pp_reg r1) (pp_cond cond) k (pp_lbl lbl)
;;

let dump_instruction = pp_instruction Ascii
let dump_instruction_hash = dump_instruction

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb =
  List.map (fun r -> IReg r) [ R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10 ]
;;

let fold_regs (f_reg, f_sreg) =
  let fold_reg reg (y_reg, y_sreg) =
    match reg with
    | IReg _ | PC -> f_reg reg y_reg, y_sreg
    | Symbolic_reg reg -> y_reg, f_sreg reg y_sreg
    | Internal _ -> y_reg, y_sreg
  in
  fun c ins ->
    match ins with
    | OP (_, r1, r2)
    | LOAD (_, _, r1, r2, _)
    | LDAQ (_, r1, r2, _)
    | MOV (r1, r2)
    | AMO (_, _, r1, _, r2, _, _)
    | JCOND (_, r1, r2, _)
    | STRL (_, r1, _, r2)
    | STORE (_, r1, _, r2) -> fold_reg r1 (fold_reg r2 c)
    | MOVI (r1, _) | STOREI (_, r1, _, _) | JCONDI (_, r1, _, _) | OPI (_, r1, _) ->
      fold_reg r1 c
    | GOTO _ -> c
;;

let map_regs f_reg f_symb =
  let map_reg reg =
    match reg with
    | IReg _ | PC -> f_reg reg
    | Symbolic_reg reg -> f_symb reg
    | Internal _ -> reg
  in
  fun ins ->
    match ins with
    | OP (op, r1, r2) -> OP (op, map_reg r1, map_reg r2)
    | OPI (op, r1, k) -> OPI (op, map_reg r1, k)
    | LOAD (w, s, r1, r2, k) -> LOAD (w, s, map_reg r1, map_reg r2, k)
    | LDAQ (w, r1, r2, k) -> LDAQ (w, map_reg r1, map_reg r2, k)
    | STORE (w, r1, k, r2) -> STORE (w, map_reg r1, k, map_reg r2)
    | STRL (w, r1, k, r2) -> STRL (w, map_reg r1, k, map_reg r2)
    | STOREI (w, r1, k1, k2) -> STOREI (w, map_reg r1, k1, k2)
    | MOV (r1, r2) -> MOV (map_reg r1, map_reg r2)
    | MOVI (r1, k) -> MOVI (map_reg r1, k)
    | AMO (op, w, r1, k, r2, s, f) -> AMO (op, w, map_reg r1, k, map_reg r2, s, f)
    | JCOND (c, r1, r2, l) -> JCOND (c, map_reg r1, map_reg r2, l)
    | JCONDI (c, r1, k, l) -> JCONDI (c, map_reg r1, k, l)
    | GOTO _ -> ins
;;

(* No addresses burried in BPF code *)
let fold_addrs _f c _ins = c
let map_addrs _f ins = ins

(* Instruction continuation *)
let get_next = function
  | OP _ | OPI _ | LOAD _ | LDAQ _ | STORE _ | STRL _ | STOREI _ | MOV _ | AMO _ | MOVI _ -> [ Label.Next ]
  | GOTO lbl -> [ Label.To lbl ]
  | JCONDI (_, _, _, lbl) | JCOND (_, _, _, lbl) -> [ Label.Next; Label.To lbl ]

include InstrUtils.No(struct type instr = instruction end)

include Pseudo.Make (struct
    type ins = instruction
    type pins = parsedInstruction
    type reg_arg = reg

    let parsed_tr i = i

    let get_naccesses = function
      | OP _ | GOTO _ | OPI _ | JCOND _ | JCONDI _ | MOV _ | MOVI _ -> 0
      | STORE _ | STRL _ | STOREI _ | LOAD _ | LDAQ _ -> 1
      | AMO _ -> 2
    ;;

    let size_of_ins _ = 4

    let fold_labels k _f = function
      | JCOND (_, _, _, lbl) | JCONDI (_, _, _, lbl) | GOTO lbl -> _f k lbl
      | _ -> k
    ;;

    let map_labels _f =
      let open BranchTarget in
      function
      | GOTO lbl -> GOTO (as_string_fun _f lbl)
      | JCOND (c, r1, r2, lbl) -> JCOND (c, r1, r2, as_string_fun _f lbl)
      | JCONDI (c, r1, k, lbl) -> JCONDI (c, r1, k, as_string_fun _f lbl)
      | ins -> ins
    ;;
  end)

let get_macro _name = raise Not_found
let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"
let hash_pteval _ = assert false

module Instr = Instr.No (struct
    type instr = instruction
  end)
