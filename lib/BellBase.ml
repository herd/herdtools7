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
let rec string_of_annot_list = function
  | [] -> ""
  | [a] -> a   
  | a::e -> a^", "^string_of_annot_list e
  
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
  | PC (* program counter, not really sure why it's required... *)

let pp_reg r =
  match r with
  | GPRreg ir -> begin 
      try List.assoc ir gpr_regs with
      | Not_found -> assert false 
    end
  | PC -> "pc"

let reg_compare = Pervasives.compare

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs @
  List.map
    (fun (r,s) -> "s" ^ String.sub s 1 (String.length s-1), GPRreg r) gpr_regs


let parse_reg s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None

let pc = PC

(****************)
(* Barriers     *)
(****************)

type barrier =
 | Fence of string list (* list of annotations *)

(* I'm not sure what this is for either. We should
   be okay *)
let all_kinds_of_barriers =  [ Fence [""] ;]

let pp_barrier b = match b with
  | Fence s -> sprintf "Fence(%s)" (string_of_annot_list s)

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type reg_or_imm =
| Regi of reg
| Imm of int

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

type imm_or_addr_or_reg = 
  | IAR_roa of reg_or_addr
  | IAR_imm of int

let pp_iar iar = match iar with
  | IAR_roa roa -> sprintf "[%s]" (string_of_reg_or_addr roa)
  | IAR_imm i -> sprintf "%d" i

type rmw2_op =
| RMWExch
| RMWAdd

let pp_rmw2_op op = match op with
  | RMWExch -> "exch"
  | RMWAdd -> "add"

type rmw3_op = 
| RMWCAS

let pp_rmw3_op op = match op with
  | RMWCAS -> "cas"

type addr_op = 
| Addr_op_atom of reg_or_addr
| Addr_op_add of reg_or_addr * reg_or_imm

let pp_addr_op a = match a with
  | Addr_op_atom roa -> string_of_reg_or_addr roa
  | Addr_op_add(roa,roi) -> sprintf "%s + %s" (string_of_reg_or_addr roa) 
    (string_of_reg_or_imm roi)


type instruction = 
| Pld  of reg * addr_op * string list
| Pst  of addr_op * reg_or_imm * string list
| Pmov of reg * imm_or_addr_or_reg
| Pand of reg * imm_or_addr_or_reg * imm_or_addr_or_reg
| Padd of reg * imm_or_addr_or_reg * imm_or_addr_or_reg
| Pxor of reg * imm_or_addr_or_reg * imm_or_addr_or_reg
| Pbeq of reg_or_imm * reg_or_imm * lbl
| Prmw2_op of reg * reg_or_addr * reg_or_imm * rmw2_op * string list
| Prmw3_op of reg * reg_or_addr * reg_or_imm * reg_or_imm * rmw3_op * string list
| Pfence of barrier

(* from GPU_PTXBase *)

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| Pld _
	| Pst _  -> 1
	| _ -> 0

      (* We don't have instructions with labels yet 
         and I'm not sure what these are used for anyways.*)
      let fold_labels k _f = function 
	| _ -> k

      let map_labels _f = function 
	| ins -> ins

     end)

let dump_instruction i = match i with
  | Pld(r, addr_op, s) -> sprintf "r(%s) %s, [%s]" 
                     (string_of_annot_list s)
                     (pp_reg r)
                     (pp_addr_op addr_op)

  | Pst(addr_op,roi,s) -> sprintf "w(%s) [%s] %s" 
                     (string_of_annot_list s)
                     (pp_addr_op addr_op)
                     (string_of_reg_or_imm roi)

  | Pmov(r,roia) -> sprintf "mov %s, %s"
		     (pp_reg r)
		     (pp_iar roia)

  | Pand(r,roia1,roia2) -> sprintf "and %s, %s, %s"
		     (pp_reg r)
		     (pp_iar roia1)
		     (pp_iar roia2)

  | Padd(r,roia1,roia2) -> sprintf "add %s, %s, %s"
		     (pp_reg r)
		     (pp_iar roia1)
		     (pp_iar roia2)

  | Pxor(r,roia1,roia2) -> sprintf "add %s, %s, %s"
		     (pp_reg r)
		     (pp_iar roia1)
		     (pp_iar roia2)


  | Pbeq(roi1,roi2,lbl) -> sprintf "beq %s, %s, %s"
    		     (string_of_reg_or_imm roi1)
		     (string_of_reg_or_imm roi2)
                     (lbl)

  | Prmw2_op(r,roa,roi,op,s) ->
                         sprintf "rmw.%s(%s) %s, [%s], %s"
			   (pp_rmw2_op op)
			   (string_of_annot_list s)
			   (pp_reg r)
			   (string_of_reg_or_addr roa)
			   (string_of_reg_or_imm roi)

  | Prmw3_op(r,roa,roi1,roi2,op,s) ->
                         sprintf "rmw.%s(%s) %s, [%s], %s, %s"
			   (pp_rmw3_op op)
			   (string_of_annot_list s)
			   (pp_reg r)
			   (string_of_reg_or_addr roa)
			   (string_of_reg_or_imm roi1)
			   (string_of_reg_or_imm roi2)


  | Pfence f -> pp_barrier f

let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg,y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg,y_sreg
    | _ -> y_reg, y_sreg 
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
  let fold_ins (_y_reg,_y_sreg as c) ins = 
    begin match ins with      
    | Pld(r, addr_op, _) -> fold_reg r (fold_addr_op addr_op c)
    | Pst(addr_op,roi,_) -> fold_addr_op addr_op (fold_roi roi c)
    | Pmov(r,roi) -> fold_reg r (fold_iar roi c)
    | Padd(r,roi1,roi2) -> fold_reg r (fold_iar roi1 (fold_iar roi2 c))
    | Pand(r,roi1,roi2) -> fold_reg r (fold_iar roi1 (fold_iar roi2 c))
    | Pxor(r,roi1,roi2) -> fold_reg r (fold_iar roi1 (fold_iar roi2 c))
    | Pbeq(roi1, roi2, _) -> fold_roi roi1 (fold_roi roi2 c)
    | Prmw2_op(r,roa,roi,_,_) -> fold_reg r (fold_roa roa (fold_roi roi c))
    | Prmw3_op(r,roa,roi1,roi2,_,_) -> fold_reg r (fold_roa roa (fold_roi roi1 (fold_roi roi2 c)))
    | Pfence _ -> c
    end 
  in fold_ins
  
let map_regs f_reg _f_symb = 
  let map_roa roa = match roa with
    | Abs _ -> roa
    | Rega r -> Rega(f_reg r)
  in
  let map_roi roi = match roi with
    | Imm _ -> roi
    | Regi r -> Regi(f_reg r)
  in
  let map_iar iar = match iar with
    | IAR_imm _ -> iar
    | IAR_roa roa -> IAR_roa(map_roa roa)
  in
  let map_addr_op ao = match ao with
    | Addr_op_atom roa -> Addr_op_atom(map_roa roa)      
    | Addr_op_add(roa,roi) -> Addr_op_add(map_roa roa,map_roi roi)
  in
  let map_ins ins = begin match ins with
    | Pld(r, addr_op, s) -> Pld(f_reg r, map_addr_op addr_op, s)
    | Pst(addr_op,roi,s) -> Pst(map_addr_op addr_op, map_roi roi, s)
    | Pmov(r,roi) -> Pmov(f_reg r, map_iar roi)
    | Padd(r,roi1,roi2) -> Padd(f_reg r, map_iar roi1, map_iar roi2)
    | Pand(r,roi1,roi2) -> Pand(f_reg r, map_iar roi1, map_iar roi2)
    | Pxor(r,roi1,roi2) -> Pxor(f_reg r, map_iar roi1, map_iar roi2)
    | Pbeq(roi1,roi2,lbl) -> Pbeq(map_roi roi1, map_roi roi2, lbl)
    | Prmw2_op(r,roa,roi,op,s) -> Prmw2_op(f_reg r, map_roa roa, map_roi roi, op, s)
    | Prmw3_op(r,roa,roi1,roi2,op,s) -> Prmw3_op(f_reg r, map_roa roa, map_roi roi1, map_roi roi2, op, s)
    | Pfence _ -> ins
  end in
  map_ins

(* Seems to work for other architectures *)
let norm_ins ins = ins

let fold_addrs _f c _ins = c

let pp_instruction _m ins = dump_instruction ins

let allowed_for_symb = []

let _get_reg_list _ins = ([], [])


(* unimplemented so far, will implement if needed*)
let get_macro _name = Warn.fatal "Bell get_macro has not been implemented"

let is_data _reg _ins = Warn.fatal "Bell is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "Bell map_addrs has not been implemented"

let get_next _ins = Warn.fatal "Bell get_next not implemented"

let set_shared _i = Warn.fatal "Bell set_shared has not been implemented"

let set_global _i = Warn.fatal "Bell set_global has not been implmeneted"

let get_reg_list _i = Warn.fatal "Bell get_reg_list has not been implemented"
