(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(* Should be put in a bell library *)
let string_of_annot_list a = String.concat "," a  

(* Who am i ? *)
let arch = Archs.bell

(* Registers mostly taken from GPU_PTX *)
type gpr_reg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 

let gpr_regs =
  [
   GPR0,"r0";  GPR1,"r1";
   GPR2,"r2";  GPR3,"r3";
   GPR4,"r4";  GPR5,"r5";
   GPR6,"r6";  GPR7,"r7";
   GPR8,"r8";  GPR9,"r9";
  ]

type reg =
  | GPRreg of gpr_reg
  | Symbolic_reg of string
  | PC (* program counter, not really sure why it's required... *)

let pp_reg r =
  match r with
  | GPRreg ir -> begin 
      try List.assoc ir gpr_regs with
      | Not_found -> assert false 
    end
  | Symbolic_reg s -> sprintf "%%%s" s
  | PC -> "pc"

let reg_compare = Pervasives.compare

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs
(* Awful hack to handle register names, s0, s1 etc.
@ 
  List.map
    (fun (r,s) -> "s" ^ String.sub s 1 (String.length s-1), GPRreg r) gpr_regs
*)

let parse_reg s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None

let pc = PC

(****************)
(* Barriers     *)
(****************)

type barrier =
 | Fence of string list * (string list * string list) option
 (* list of annotations, optional sets of labels*)

(* jade: i'm guessing this is to give one possible example fence? picking this one *)
let all_kinds_of_barriers =  [ Fence ([], None);]

let pp_barrier b = match b with
  | Fence(s, None) ->
      sprintf "Fence(%s)" (string_of_annot_list s)
  | Fence(s, Some(s1,s2)) ->
      sprintf "Fence(%s)(%s,%s)"
        (string_of_annot_list s)
        (string_of_annot_list s1)
        (string_of_annot_list s2)

let barrier_compare = Pervasives.compare

(* For barrier instructions, MUST be the same as parsed *)
let pp_fence_ins = function
  | Fence (s,None) ->
      sprintf "f[%s]" (string_of_annot_list s)
  | Fence(s, Some(s1,s2)) ->
      sprintf "f[%s]([%s],[%s])"
        (string_of_annot_list s)
        (string_of_annot_list s1)
        (string_of_annot_list s2)

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

let string_of_reg_or_imm r = match r with
  | Regi r -> pp_reg r
  | Imm r -> sprintf "%d" r

open Constant

type reg_or_addr = 
  | Rega of reg  (* address given in register *)
  | Abs of SymbConstant.v (* address given as a constant *)

let pp_abs = function
  | Symbolic s -> s
  | Concrete i -> string_of_int i

let string_of_reg_or_addr r = match r with 
  | Rega r -> pp_reg r
  | Abs r -> pp_abs r

type 'k imm_or_addr_or_reg = 
  | IAR_roa of reg_or_addr
  | IAR_imm of 'k

let imm_or_addr_or_reg_tr f = function
  | IAR_roa _ as keep -> keep
  | IAR_imm k -> IAR_imm (f k)

let pp_iar iar = match iar with
  | IAR_roa roa -> string_of_reg_or_addr roa
  | IAR_imm i -> sprintf "%d" i

type 'k addr_op = 
| Addr_op_atom of reg_or_addr
| Addr_op_add of reg_or_addr * 'k reg_or_imm

let addr_op_tr f = function
  | Addr_op_add (r,k) -> Addr_op_add (r,reg_or_imm_tr f k)
  | Addr_op_atom _ as keep -> keep

let pp_addr_op a = match a with
  | Addr_op_atom roa -> string_of_reg_or_addr roa
  | Addr_op_add(roa,roi) -> sprintf "%s+%s" (string_of_reg_or_addr roa) 
    (string_of_reg_or_imm roi)

type 'k cond = 
  | Eq of reg * 'k reg_or_imm
  | Ne of reg * 'k reg_or_imm
  | Bal 

let cond_tr f = function
  | Eq (r,ri) -> Eq (r,reg_or_imm_tr f ri)
  | Ne (r,ri) -> Ne (r,reg_or_imm_tr f ri)
  | Bal as keep -> keep

type 'k op = 
  | Add of reg * 'k imm_or_addr_or_reg * 'k imm_or_addr_or_reg 
  | And of reg * 'k imm_or_addr_or_reg * 'k imm_or_addr_or_reg
  | Mov of reg * 'k imm_or_addr_or_reg 

let op_tr f = function
  | Add (r,iar1,iar2) ->
      Add (r,imm_or_addr_or_reg_tr f iar1,imm_or_addr_or_reg_tr f iar2)
  | And (r,iar1,iar2) ->
      And (r,imm_or_addr_or_reg_tr f iar1,imm_or_addr_or_reg_tr f iar2)
  | Mov (r,iar) -> Mov (r,imm_or_addr_or_reg_tr f iar)

let pp_op = function
  | Add(r,x,i) -> sprintf "add %s %s %s" (pp_reg r) (pp_iar x) (pp_iar i)
  | And(r,x,i) -> sprintf "and %s %s %s" (pp_reg r) (pp_iar x) (pp_iar i) 
  | Mov(r,i) -> sprintf "mov %s %s" (pp_reg r) (pp_iar i)

let pp_cond = function
  | Eq(r,x) -> sprintf "eq %s %s" (pp_reg r) (string_of_reg_or_imm x) 
  | Ne(r,x) -> sprintf "ne %s %s" (pp_reg r) (string_of_reg_or_imm x)  
  | Bal -> ""

type 'k kinstruction = 
| Pld of reg * 'k addr_op * string list
| Pst of 'k addr_op * 'k reg_or_imm * string list
| Prmw of 'k op * string list
| Pfence of barrier
| Pbranch of 'k cond * lbl * string list

let instruction_tr f = function
  | Pld (r,ao,s) -> Pld (r,addr_op_tr f ao,s)
  | Pst (ao,ri,s) -> Pst (addr_op_tr f ao,reg_or_imm_tr f ri,s)
  | Prmw (op,s) -> Prmw (op_tr f op,s)
  | Pfence _ as i -> i
  | Pbranch (cond,lbl,s) -> Pbranch (cond_tr f cond,lbl,s)

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

let dump_instruction i = match i with
| Pld(r, addr_op, s) -> sprintf "r[%s] %s %s"
      (string_of_annot_list s)
      (pp_reg r)
      (pp_addr_op addr_op)

| Pst(addr_op,roi,s) -> sprintf "w[%s] %s %s" 
      (string_of_annot_list s)
      (pp_addr_op addr_op)
      (string_of_reg_or_imm roi)

| Prmw(op,s) -> sprintf "rmw[%s](%s)"
      (string_of_annot_list s)
      (pp_op op)

| Pfence f -> pp_fence_ins f

| Pbranch(c,l,s) -> sprintf "b[%s](%s) %s" 
      (string_of_annot_list s)
      (pp_cond c) 
      l 
 
let fold_regs (f_reg,f_sreg) = 
  let fold_reg reg (y_reg,y_sreg as k) = match reg with
    | GPRreg _ -> f_reg reg y_reg,y_sreg
    | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
    | PC -> k
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
  let fold_cond cond c = match cond with
    | Eq(r,i) -> fold_reg r (fold_roi i c) 
    | Ne(r,i) -> fold_reg r (fold_roi i c)  
    | Bal -> c 
  in
  let fold_op op c = match op with
    | Add(r,x,i) -> fold_reg r (fold_iar x (fold_iar i c)) 
    | And(r,x,i) -> fold_reg r (fold_iar x (fold_iar i c)) 
    | Mov(r,i) -> fold_reg r (fold_iar i c)
  in
  let fold_ins (_y_reg,_y_sreg as c) ins = 
    begin match ins with      
    | Pld(r, addr_op, _) -> fold_reg r (fold_addr_op addr_op c)
    | Pst(addr_op,roi,_) -> fold_addr_op addr_op (fold_roi roi c)
    | Prmw(op,_) -> fold_op op c
    | Pbranch(cond,_,_) -> fold_cond cond c 
    | Pfence _ -> c
    end 
  in fold_ins
  
let map_regs f_reg f_symb = 

  let map_reg reg = match reg with
  | GPRreg _ -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | PC -> reg in

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
  let map_cond cond = match cond with
    | Eq(r,i) -> Eq(map_reg r, map_roi i)
    | Ne(r,i) -> Ne(map_reg r, map_roi i)  
    | Bal -> Bal
  in
  let map_op op = match op with
    | Add(r,x,i) -> Add(map_reg r, map_iar x, map_iar i) 
    | And(r,x,i) -> And(map_reg r, map_iar x, map_iar i) 
    | Mov(r,i) -> Mov(map_reg r, map_iar i) 
  in
  let map_ins ins = begin match ins with
    | Pld(r,addr_op,s) -> Pld(map_reg r, map_addr_op addr_op, s)
    | Pst(addr_op,roi,s) -> Pst(map_addr_op addr_op, map_roi roi, s)
    | Prmw(op,s) -> Prmw(map_op op, s)
    | Pbranch(cond,lbl,s) -> Pbranch(map_cond cond,lbl,s)
    | Pfence _ -> ins
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
    | Add(_,x,i) -> fold_iar x (fold_iar i c) 
    | And(_,x,i) -> fold_iar x (fold_iar i c) 
    | Mov(_,i) -> fold_iar i c
  in
  fun c ins -> match ins with
  | Pbranch _ | Pfence _ -> c
  | Pld (_,ao,_) | Pst (ao,_,_) -> fold_ao ao c
  | Prmw (op,_) -> fold_op op c

let pp_instruction _m ins = dump_instruction ins

let allowed_for_symb =
  List.map
    (fun (r,_) ->  GPRreg r)
    gpr_regs


let _get_reg_list _ins = ([], [])


(* unimplemented so far, will implement if needed*)
let get_macro _name = Warn.fatal "Bell get_macro has not been implemented"

let is_data _reg _ins = Warn.fatal "Bell is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "Bell map_addrs has not been implemented"

let get_next _ins = Warn.fatal "Bell get_next not implemented"

let set_shared _i = Warn.fatal "Bell set_shared has not been implemented"

let set_global _i = Warn.fatal "Bell set_global has not been implmeneted"

let get_reg_list _i = Warn.fatal "Bell get_reg_list has not been implemented"

(* Annotations *)
let get_id_and_list i = match i with
| Pld(_,_,s) -> (BellName.r,s)
| Pst(_,_,s) -> (BellName.w,s)
| Pfence (Fence (s, _)) -> (BellName.f,s)      
| Prmw(_,s) -> (BellName.rmw,s)
| Pbranch(_,_,s) -> (BellName.b,s)


let get_from_and_to_labels b = match b with
| Fence (_, a) -> a

let set_list i al = match i with
| Pld (a1,a2,_) -> Pld (a1,a2,al)
| Pst (a1,a2,_) -> Pst (a1,a2,al)
| Pfence (Fence (_,a2)) -> Pfence (Fence (al,a2))
| Prmw(a1,_) -> Prmw(a1,al)
| Pbranch(a1,a2,_) -> Pbranch(a1,a2,al)

