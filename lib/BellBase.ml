(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

(* Should be put in a bell library *)
let string_of_annot_list a = String.concat "," a
let string_of_labels a = Label.Set.pp_str "," Misc.identity a

(* Who am i ? *)
let arch = Archs.lisa
let endian = Endian.Little
let base_type = CType.Base "int"

type gpr_reg = int

type reg =
  | GPRreg of gpr_reg
  | Symbolic_reg of string

let pp_reg r = match r with
| GPRreg ir -> sprintf "r%i" ir
| Symbolic_reg s -> sprintf "%%%s" s

let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r

let parse_reg s =
  let len = String.length s in
  assert (len > 0) ;
  match s.[0] with
  | 'r' ->
      begin try
        let rem = String.sub s 1 (len-1) in
        Some (GPRreg (int_of_string rem))
      with _ -> None end
  | _ -> None
let typeof c = assert false

(****************)
(* Barriers     *)
(****************)

type barrier =
 | Fence of string list * (Label.Set.t * Label.Set.t) option
 (* list of annotations, optional sets of labels*)

(* jade: i'm guessing this is to give one possible example fence? picking this one *)
let all_kinds_of_barriers =  [ Fence ([], None);]

let pp_barrier b = match b with
  | Fence(s, None) ->
      sprintf "Fence(%s)" (string_of_annot_list s)
  | Fence(s, Some(s1,s2)) ->
      sprintf "Fence(%s)(%s X %s)"
        (string_of_annot_list s)
        (string_of_labels s1)
        (string_of_labels s2)


let barrier_compare = compare

(* For barrier instructions, MUST be the same as parsed *)
let pp_fence_ins = function
  | Fence (s,None) ->
      sprintf "f[%s]" (string_of_annot_list s)
  | Fence(s, Some(s1,s2)) ->
      sprintf "f[%s] {%s} {%s}"
        (string_of_annot_list s)
        (string_of_labels s1)
        (string_of_labels s2)


(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type 'k reg_or_imm =
| Regi of reg
| Imm of 'k

let reg_or_imm_tr f = function
  | Imm k -> Imm (f k)
  | Regi _ as keep -> keep

let string_of_reg_or_imm pk r = match r with
  | Regi r -> pp_reg r
  | Imm r -> pk r

type reg_or_addr =
  | Rega of reg  (* address given in register *)
  | Abs of ParsedConstant.v (* address given as a constant *)


let pp_abs = ParsedConstant.pp_v

let string_of_reg_or_addr r = match r with
  | Rega r -> pp_reg r
  | Abs r -> pp_abs r

type 'k imm_or_addr_or_reg =
  | IAR_roa of reg_or_addr
  | IAR_imm of 'k

let imm_or_addr_or_reg_tr f = function
  | IAR_roa _ as keep -> keep
  | IAR_imm k -> IAR_imm (f k)

let pp_iar pk iar = match iar with
  | IAR_roa roa -> string_of_reg_or_addr roa
  | IAR_imm i -> pk i

type 'k addr_op =
| Addr_op_atom of reg_or_addr
| Addr_op_add of reg_or_addr * 'k reg_or_imm

let addr_op_tr f = function
  | Addr_op_add (r,k) -> Addr_op_add (r,reg_or_imm_tr f k)
  | Addr_op_atom _ as keep -> keep

let pp_addr_op pk a = match a with
  | Addr_op_atom roa -> string_of_reg_or_addr roa
  | Addr_op_add(roa,roi) -> sprintf "%s+%s" (string_of_reg_or_addr roa)
    (string_of_reg_or_imm pk roi)

type op_t =
  | Add | Xor | And | Eq | Neq

type 'k op =
  | RAI of 'k imm_or_addr_or_reg
  | OP of op_t * 'k imm_or_addr_or_reg * 'k imm_or_addr_or_reg

let r_in_op =
  let in_addr r = function
    | IAR_roa (Rega ra) -> reg_compare r ra = 0
    | IAR_roa (Abs _)
    | IAR_imm _ -> false in
  fun r x -> match x with
  | RAI a -> in_addr r a
  | OP (_,x,y) ->
      in_addr r x || in_addr r y

let op_tr f = function
  | RAI (iar) ->
      RAI (imm_or_addr_or_reg_tr f iar)
  | OP (op,iar1,iar2) ->
      OP (op,imm_or_addr_or_reg_tr f iar1,imm_or_addr_or_reg_tr f iar2)

let pp_op pk = function
  | RAI(iar) -> sprintf "%s" (pp_iar pk iar)
  | OP(Add,x,i) -> sprintf "(add %s %s)" (pp_iar pk x) (pp_iar pk i)
  | OP(Xor,x,i) -> sprintf "(xor %s %s)" (pp_iar pk x) (pp_iar pk i)
  | OP(And,x,i) -> sprintf "(and %s %s)" (pp_iar pk x) (pp_iar pk i)
  | OP(Eq,x,y) -> sprintf "(eq %s %s)" (pp_iar pk x) (pp_iar pk y)
  | OP(Neq,x,y) -> sprintf "(neq %s %s)" (pp_iar pk x) (pp_iar pk y)

type 'k kinstruction =
| Pnop
| Pld of reg * 'k addr_op * string list
| Pst of 'k addr_op * 'k reg_or_imm * string list
| Pfence of barrier
| Pcall of string
| Prmw of reg * 'k op * 'k addr_op * string list
| Pbranch of reg option * lbl * string list
| Pmov of reg * 'k op


let instruction_tr f = function
  | Pnop -> Pnop
  | Pld (r,x,s) -> Pld (r,addr_op_tr f x,s)
  | Pst (x,ri,s) -> Pst (addr_op_tr f x,reg_or_imm_tr f ri,s)
  | Pfence _ as i -> i
  | Prmw (r,op,x,s) -> Prmw (r,op_tr f op,addr_op_tr f x,s)
  | Pbranch _|Pcall _ as i -> i
  | Pmov (r,op) -> Pmov (r,op_tr f op)

type instruction = int kinstruction
type parsedInstruction = MetaConst.k kinstruction

(* from GPU_PTXBase *)

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let parsed_tr i = instruction_tr MetaConst.as_int i

      let get_naccesses = function
        | Pld _
        | Pst _  -> 1
        | _ -> 0

      (* We do have instructions with labels... *)
      let fold_labels k f = function
        | Pbranch (_,lbl,_) -> f k lbl
        | _ -> k

      let map_labels f = function
        | Pbranch(c,lbl,s) -> Pbranch(c,f lbl,s)
        | ins -> ins

     end)

let do_dump_instruction pk i = match i with
| Pnop -> "nop"

| Pld(r, addr_op, s) -> sprintf "r[%s] %s %s"
      (string_of_annot_list s)
      (pp_reg r)
      (pp_addr_op pk addr_op)

| Pst(addr_op,roi,s) -> sprintf "w[%s] %s %s"
      (string_of_annot_list s)
      (pp_addr_op pk addr_op)
      (string_of_reg_or_imm pk roi)

| Prmw(r,op,x,s) -> sprintf "rmw[%s] %s %s %s"
      (string_of_annot_list s)
      (pp_reg r)
      (pp_op pk op)
      (pp_addr_op pk x)

| Pfence f -> pp_fence_ins f

| Pcall s -> sprintf "call[%s]" s

| Pbranch(Some r,l,s) -> sprintf "b[%s] %s %s"
      (string_of_annot_list s)
      (pp_reg r)
      l

| Pbranch(None,l,s) -> sprintf "b[%s] %s"
      (string_of_annot_list s)
      l

| Pmov(r,op) -> sprintf "mov %s %s"
      (pp_reg r)
      (pp_op pk op)


let fold_regs (f_reg,f_sreg) =
  let fold_reg reg (y_reg,y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg,y_sreg
    | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
  in
  let fold_roa roa c = match roa with
    | Rega r -> fold_reg r c
    | Abs _ -> c
  in
  let fold_roi roi c = match roi with
    | Imm _ -> c
    | Regi r -> fold_reg r c
  in
  let fold_iar iar c = match iar with
    | IAR_roa roa -> fold_roa roa c
    | IAR_imm _ -> c
  in
  let fold_addr_op ao c = match ao with
    | Addr_op_atom roa -> fold_roa roa c
    | Addr_op_add(roa,roi) -> fold_roa roa (fold_roi roi c)
  in
  let fold_op op c = match op with
    | RAI(i) -> fold_iar i c
    | OP(_,x,i) -> fold_iar x (fold_iar i c)
  in
  let fold_ins (_y_reg,_y_sreg as c) ins =
    begin match ins with
    | Pnop -> c
    | Pld(r, addr_op, _) -> fold_reg r (fold_addr_op addr_op c)
    | Pst(addr_op,roi,_) -> fold_addr_op addr_op (fold_roi roi c)
    | Pfence _|Pcall _|Pbranch (None,_,_) -> c
    | Prmw(r,op,_,_) -> fold_reg r (fold_op op c)
    | Pbranch(Some r,_,_) -> fold_reg r c
    | Pmov(r,op) -> fold_reg r (fold_op op c)
    end
  in fold_ins

let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | GPRreg _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg in

  let map_roa roa = match roa with
    | Abs _ -> roa
    | Rega r -> Rega(map_reg r)
  in
  let map_roi roi = match roi with
    | Imm _ -> roi
    | Regi r -> Regi(map_reg r)
  in
  let map_iar iar = match iar with
    | IAR_imm _ -> iar
    | IAR_roa roa -> IAR_roa(map_roa roa)
  in
  let map_addr_op ao = match ao with
    | Addr_op_atom roa -> Addr_op_atom(map_roa roa)
    | Addr_op_add(roa,roi) -> Addr_op_add(map_roa roa,map_roi roi)
  in
  let map_op op = match op with
    | RAI(i) -> RAI(map_iar i)
    | OP(op,x,i) -> OP(op,map_iar x, map_iar i)
  in
  let map_ins ins = begin match ins with
    | Pnop -> Pnop
    | Pld(r,addr_op,s) -> Pld(map_reg r, map_addr_op addr_op, s)
    | Pst(addr_op,roi,s) -> Pst(map_addr_op addr_op, map_roi roi, s)
    | Prmw(r,op,x,s) -> Prmw(map_reg r,map_op op, map_addr_op x, s)
    | Pbranch(Some r,lbl,s) -> Pbranch(Some (map_reg r),lbl,s)
    | Pfence _|Pcall _|Pbranch (None,_,_) -> ins
    | Pmov(r,op) -> Pmov(map_reg r,map_op op)
  end in
  map_ins

(* Seems to work for other architectures *)

let norm_ins ins = ins


let fold_addrs f =
 let fold_roa roa c = match roa with
  | Rega _ -> c
  | Abs a -> f a c
 in
 let fold_iar iar c = match iar with
  | IAR_roa roa -> fold_roa roa c
  | IAR_imm _ -> c
 in
 let fold_ao ao c = match ao with
   | Addr_op_atom roa
   | Addr_op_add (roa,_) ->
       fold_roa roa c
  in
  let fold_op op c = match op with
    | RAI(i) -> fold_iar i c
    | OP(_,x,i) -> fold_iar x (fold_iar i c)
  in
  fun c ins -> match ins with
  | Pnop|Pbranch _ | Pfence _|Pcall _ -> c
  | Pld (_,ao,_) | Pst (ao,_,_) -> fold_ao ao c
  | Prmw (_,op,x,_) -> fold_op op (fold_ao x c)
  | Pmov (_,op) -> fold_op op c

let dump_instruction i = do_dump_instruction (sprintf "%i") i
let dump_parsedInstruction i = do_dump_instruction MetaConst.pp i

let pp_instruction _m = dump_instruction

(* 100 registers are probably enough *)
let allowed_for_symb = List.map (fun r ->  GPRreg r) (Misc.interval 0 100)


let _get_reg_list _ins = ([], [])


(* unimplemented so far, will implement if needed*)
let get_macro _name = Warn.fatal "Bell get_macro has not been implemented"

let is_data _reg _ins = Warn.fatal "Bell is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "Bell map_addrs has not been implemented"

let get_next _ins = Warn.fatal "Bell get_next not implemented"

let set_shared _i = Warn.fatal "Bell set_shared has not been implemented"

let set_global _i = Warn.fatal "Bell set_global has not been implemented"

let get_reg_list _i = Warn.fatal "Bell get_reg_list has not been implemented"

(* Annotations *)
let get_id_and_list i = match i with
| Pld(_,_,s) -> (BellName.r,s)
| Pst(_,_,s) -> (BellName.w,s)
| Pfence (Fence (s, _)) -> (BellName.f,s)
| Prmw(_,_,_,s) -> (BellName.rmw,s)
| Pbranch(_,_,s) -> (BellName.b,s)
| Pcall s -> (BellName.call,[s])
| Pnop|Pmov _ -> raise Not_found

let get_from_and_to_labels b = match b with
| Fence (_, a) -> a

let set_list i al = match i with
| Pld (a1,a2,_) -> Pld (a1,a2,al)
| Pst (a1,a2,_) -> Pst (a1,a2,al)
| Pfence (Fence (_,a2)) -> Pfence (Fence (al,a2))
| Prmw(a1,a2,a3,_) -> Prmw(a1,a2,a3,al)
| Pbranch(a1,a2,_) -> Pbranch(a1,a2,al)
| Pcall _ ->
    begin match al with
    | [] -> i
    | s::_ -> Pcall s
    end
| Pnop|Pmov _ as i -> i

let tr_compat = function
  | Pcall "sync" -> Pfence (Fence (["sync";],None))
  | i -> i
