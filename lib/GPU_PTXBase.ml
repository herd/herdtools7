(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define registers, barriers, and instructions for PTX *) 

open Printf

(* Who am i ? *)
let arch = Archs.gpu_ptx

(*************)
(* Registers *)
(*************)

type gpr_reg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 | PR0  | PR1 
  | PR2  | PR3  | PR4  | PR5 
  | PR6  | PR7  | PR8  | PR9

let gpr_regs =
  [
   GPR0,"r0";  GPR1,"r1";
   GPR2,"r2";  GPR3,"r3";
   GPR4,"r4";  GPR5,"r5";
   GPR6,"r6";  GPR7,"r7";
   GPR8,"r8";  GPR9,"r9";
   PR0,"p0";  PR1,"p1";
   PR2,"p2";  PR3,"p3";
   PR4,"p4";  PR5,"p5";
   PR6,"p6";  PR7,"p7";
   PR8,"p8";  PR9,"p9";
  ]

type reg =
  | GPRreg of gpr_reg (* integer and boolean registers *)
  | PC (* program counter *)

let pp_reg r =
  match r with
  | GPRreg ir -> begin 
      try List.assoc ir gpr_regs with
      | Not_found -> assert false 
    end
  | PC -> "pc"

let reg_compare = Pervasives.compare

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs 
    
let parse_reg s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs 
    
let parse_cop s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None


let pc = PC

(****************)
(* Barriers     *)
(****************)

type bar_scope = 
| CTA_bar
| GL_bar
| SYS_bar

let pp_bar_scope s = match s with
  | CTA_bar -> "cta"
  | GL_bar -> "gl"
  | SYS_bar -> "sys"

type barrier =
 | Membar of bar_scope

let all_kinds_of_barriers =  [ Membar GL_bar ;]
(* Shouldn't the list above include Membar CTA and Membar SYS too? *)

let pp_barrier b = match b with
  | Membar s -> sprintf "membar.%s" (pp_bar_scope s)
  

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type cache_op = 
  | CA
  | CG
  | CV
  | WB
  | WT
  | NCOP


type state_space = 
  | Shared
  | Global
  | NOMP

type op_type = 
  | S32
  | B64
  | B32
  | U64
  | S64
  | U32
  | PRED

type ptx_atom_op =
| Atom_and
| Atom_or
| Atom_xor
| Atom_add
| Atom_inc
| Atom_dec
| Atom_exch
| Atom_cas

let pp_op_type o = match o with
  | S32 -> ".s32"
  | B64 -> ".b64"
  | B32 -> ".b32"
  | U64 -> ".u64"
  | S64 -> ".s64"
  | U32 -> ".u32"
  | PRED -> ".pred"

let pp_state_space m = match m with
  | Shared -> ".shared"
  | Global -> ".global"
  | NOMP -> ""

let pp_cache_op c = match c with
  | CA -> ".ca"
  | CG -> ".cg"
  | CV -> ".cv"
  | WB -> ".wb"
  | WT -> ".wt"
  | NCOP -> ""

let pp_ptx_atom_op o = match o with
| Atom_and -> ".and"
| Atom_or -> ".or"
| Atom_xor -> ".xor"
| Atom_add -> ".add"
| Atom_inc -> ".inc"
| Atom_dec -> ".dec"
| Atom_exch -> ".exch"
| Atom_cas -> ".cas"
    
type ins_op = 
| Reg of reg
| Im of int

let pp_ins_op op = match op with
  | Reg r -> (pp_reg r)
  | Im i -> sprintf "%d" i

let mk_b64_reg r = match r with
  | GPRreg(r) -> r
  | _ -> assert false

let mk_s32_reg r = match r with
  | GPRreg(r) -> r
  | _ -> assert false

type lbl = Label.t

type instruction = 
| Pld of reg*reg*state_space*cache_op*op_type
| Pst of reg*reg*state_space*cache_op*op_type
| Pldvol of reg*reg*state_space*op_type
| Pstvol of reg*reg*state_space*op_type
| Padd of reg*ins_op*ins_op*op_type
| Pand of reg*ins_op*ins_op*op_type
| Pmov of reg*ins_op*op_type
| Pcvt of reg*reg*op_type*op_type
| Pmembar of bar_scope
| Pguard of reg*instruction
| Pguardnot of reg*instruction
| Psetp of Op.op*reg*ins_op*ins_op*op_type
| Pjmp of lbl
| Patom2op of reg*reg*ins_op*state_space*ptx_atom_op*op_type
| Patom3op of reg*reg*ins_op*ins_op*state_space*ptx_atom_op*op_type

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| Pld _
	| Pst _ 
	| Pldvol _ 
	| Pstvol _ -> 1
	| _ -> 0


      (* I don't think we have labels yet... *)
      let fold_labels k _f = function 
	| _ -> k

      let map_labels _f = function 
	| ins -> ins

     end)
    
let rec dump_instruction i = match i with
  | Pld(r1,r2,m,cop,t) -> sprintf "ld%s%s%s %s, [%s]" (pp_state_space m)
                                                      (pp_cache_op cop)
                                                      (pp_op_type t)
                                                      (pp_reg r1) (pp_reg r2)

  | Pst(r1,r2,m,cop,t) -> sprintf "st%s%s%s [%s], %s" (pp_state_space m)
                                                      (pp_cache_op cop)
                                                      (pp_op_type t)
                                                      (pp_reg r1) (pp_reg r2)

  | Pldvol(r1,r2,m,t) -> sprintf "ld.volatile%s%s %s, [%s]" (pp_state_space m)
                                                            (pp_op_type t)
                                                            (pp_reg r1) (pp_reg r2)

  | Pstvol(r1,r2,m,t) -> sprintf "st.volatile%s%s [%s], %s" (pp_state_space m)
                                                            (pp_op_type t)
                                                            (pp_reg r1) (pp_reg r2)


  | Pmov(r1,op,t) -> sprintf "mov%s %s, %s" (pp_op_type t)
                                            (pp_reg r1)
                                            (pp_ins_op op)

  | Padd(r1,op1,op2,t) -> sprintf "add%s %s, %s %s" (pp_op_type t) 
                                                     (pp_reg r1) 
                                                     (pp_ins_op op1) 
                                                     (pp_ins_op op2)

  | Pand(r1,op1,op2,t) -> sprintf "and%s %s, %s %s" (pp_op_type t) 
                                                     (pp_reg r1) 
                                                     (pp_ins_op op1) 
                                                     (pp_ins_op op2)

  | Pcvt(r1,r2,t1,t2) -> sprintf "cvt%s%s %s, %s"    (pp_op_type t1)
                                                       (pp_op_type t2)
                                                       (pp_reg r1) 
                                                       (pp_reg r2)

  | Pmembar s -> sprintf "membar.%s" (pp_bar_scope s)
  | Pguard(r,ins) -> sprintf "@%s %s" (pp_reg r) (dump_instruction ins)
  | Pguardnot(r,ins) -> sprintf "@!%s %s" (pp_reg r) (dump_instruction ins)
  | Psetp(cmp_op,r,op1,op2,t) -> sprintf "setp %s %s %s, %s, %s" (Op.pp_ptx_cmp_op cmp_op) (pp_op_type t) (pp_reg r) (pp_ins_op op1) (pp_ins_op op2)
  | Pjmp (lbl) -> sprintf "bra %s" lbl
  | Patom2op(d,a,b,ss,o,t) -> sprintf "atom%s%s%s %s, [%s], %s" (pp_state_space ss) (pp_ptx_atom_op o)(pp_op_type t) (pp_reg d) (pp_reg a) (pp_ins_op b)
  | Patom3op(d,a,b,c,ss,o,t) -> sprintf "atom%s%s%s %s, [%s], %s, %s" (pp_state_space ss) (pp_ptx_atom_op o)(pp_op_type t) (pp_reg d) (pp_reg a) (pp_ins_op b) (pp_ins_op c)

(* Required by archBase.mli   *)

(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg,y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg,y_sreg
    | _ -> y_reg, y_sreg in
  let rec fold_ins (_y_reg,_y_sreg as c) ins = begin match ins with

    (*two registers*)
  | Pld(r1,r2,_,_,_) | Pst(r1,r2,_,_,_) | Pcvt(r1,r2,_,_) | Pldvol(r1,r2,_,_) | Pstvol(r1,r2,_,_)
      -> fold_reg (r2 )(fold_reg (r1) c)

    (*one register or 2 registers for mov*)
  | Pmov (r1,op,_) ->
    begin match op with
    | Reg r2  -> fold_reg (r1 )(fold_reg (r2) c)
    | _          -> fold_reg (r1) c
    end

      (* possibly 3 registers for 2 op atomics *)
  | Patom2op(r1,r2,op,_,_,_) -> 
    begin match op with
    | Reg r3 -> fold_reg (r1) (fold_reg (r2) (fold_reg (r3) c))
    | _ -> fold_reg (r1 )(fold_reg (r2) c)
    end

  (* possibly 4 (phew!) registers for 3 op atomics *)
  | Patom3op(r1,r2,op1,op2,_,_,_) -> 
    begin match op1,op2 with
    | Reg r3, Reg r4-> fold_reg (r1) (fold_reg (r2) (fold_reg (r3) (fold_reg (r4) c)))
    | Reg r3, _ | _, Reg r3 -> fold_reg (r1) (fold_reg (r2) (fold_reg (r3) c))
    | _, _ -> fold_reg (r1 )(fold_reg (r2) c)
    end
      
  | Padd(r1, op1, op2, _) 
  | Psetp(_, r1, op1, op2, _) 
  | Pand(r1, op1, op2, _) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> fold_reg (r1 )(fold_reg (r2) (fold_reg (r3) c))
    | _, Reg r2      -> fold_reg (r1 )(fold_reg (r2) c)
    | Reg r2, _      -> fold_reg (r1 )(fold_reg (r2) c)
    | _, _           -> fold_reg (r1) c
    end

  (* guarded instructions -- the following is a bit of a guess *) 
  | Pguard(r,ins) -> fold_reg (r) (fold_ins c ins)

  | Pguardnot(r,ins) -> fold_reg (r) (fold_ins c ins)

  (*zero registers*)
  | Pmembar _  -> c
  | Pjmp _  -> c
  end 
  in fold_ins
    
let map_regs f_reg _f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  let map2 ins r1 r2 = ins (map_reg r1,map_reg r2) in
  let map3 ins r1 r2 r3 =ins (map_reg r1,map_reg r2,map_reg r3) in
  let map4 ins r1 r2 r3 r4 =ins (map_reg r1,map_reg r2,map_reg r3, map_reg r4) in

  let rec map_ins ins = begin match ins with
  (*Two registers*)
  | Pld(r1,r2,m,cop,t) ->
    map2 (fun (r1,r2) -> Pld(r1,r2,m,cop,t)) r1 r2
  | Pst(r1,r2,m,cop,t) -> 
    map2 (fun (r1,r2) -> Pst(r1,r2,m,cop,t)) r1 r2

  | Pldvol(r1,r2,m,t) ->
    map2 (fun (r1,r2) -> Pldvol(r1,r2,m,t)) r1 r2
  | Pstvol(r1,r2,m,t) -> 
    map2 (fun (r1,r2) -> Pstvol(r1,r2,m,t)) r1 r2

  | Pcvt(r1,r2,t1,t2) ->
    map2 (fun (r1,r2) -> Pcvt(r1,r2,t1,t2)) r1 r2

  (*mov instruction takes 1 or 2 registers*)
  | Pmov(r1,op,t) ->
    begin match op with
    | Reg r2  -> map2 (fun (r1,r2) -> Pmov(r1,Reg r2,t)) r1 r2
    | _       -> Pmov(map_reg r1,op,t)
    end

  | Patom2op(r1,r2,op,a,b,c) -> 
    begin match op with 
    | Reg r3 -> map3 (fun (rr1,rr2,rr3) -> Patom2op(rr1,rr2,Reg(rr3),a,b,c)) r1 r2 r3
    | _ -> map2 (fun (rr1,rr2) -> Patom2op(rr1,rr2,op,a,b,c)) r1 r2
    end

  | Patom3op(r1,r2,op1,op2,a,b,c) -> 
    begin match op1,op2 with 
    | Reg r3, Reg r4 -> map4 (fun (rr1,rr2,rr3,rr4) -> Patom3op(rr1,rr2,Reg(rr3),Reg(rr4),a,b,c)) r1 r2 r3 r4
    | Reg r3,_ -> map3 (fun (rr1,rr2,rr3) -> Patom3op(rr1,rr2,Reg(rr3),op2,a,b,c)) r1 r2 r3 
    | _, Reg r3 -> map3 (fun (rr1,rr2,rr3) -> Patom3op(rr1,rr2,op1,Reg(rr3),a,b,c)) r1 r2 r3 
    | _,_ -> map2 (fun (rr1,rr2) -> Patom3op(rr1,rr2,op1,op2,a,b,c)) r1 r2
    end       		 

  | Padd (r1,op1,op2,t) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> map3 (fun (r1,r2,r3) -> Padd(r1, Reg r2, Reg r3,t)) r1 r2 r3
    | Reg r2, _ ->
      map2 (fun (r1,r2) -> Padd(r1, Reg r2, op2,t)) r1 r2
    | _, Reg r2 ->
      map2 (fun (r1,r2) -> Padd(r1, op1, Reg r2,t)) r1 r2
    | _,_ -> Padd(map_reg r1, op1, op2, t)
    end   

  | Psetp (cmp_op,r1,op1,op2,t) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> 
      map3 (fun (r1,r2,r3) -> Psetp(cmp_op,r1, Reg r2, Reg r3,t)) r1 r2 r3
    | Reg r2, _ ->
      map2 (fun (r1,r2) -> Psetp(cmp_op,r1, Reg r2, op2,t)) r1 r2
    | _, Reg r2 ->
      map2 (fun (r1,r2) -> Psetp(cmp_op,r1, op1, Reg r2,t)) r1 r2
    | _,_ -> Psetp(cmp_op,map_reg r1, op1, op2, t)
    end  

  | Pand (r1,op1,op2,t) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> map3 (fun (r1,r2,r3) -> Pand(r1, Reg r2, Reg r3,t)) r1 r2 r3
    | Reg r2, _ ->
      map2 (fun (r1,r2) -> Pand(r1, Reg r2, op2,t)) r1 r2
    | _, Reg r2 ->
      map2 (fun (r1,r2) -> Pand(r1, op1, Reg r2,t)) r1 r2
    | _,_ -> Pand(map_reg r1, op1, op2, t)
    end    
  | Pguard (r,ins) -> Pguard (map_reg r, map_ins ins)
  | Pguardnot (r,ins) -> Pguardnot (map_reg r, map_ins ins)

      (*Zero registers*)
  | Pmembar _ 
  | Pjmp _  -> ins
  end in
  map_ins
  

(* GPU operation to change memory into shared or global locations *)
let to_shared s1 s2 = sprintf "cvta.to.shared %s, %s;" s1 s2
let to_global _s1 _s2 = ""

let gpu_macros = ""

(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let _get_reg_list _ins = ([], [])

let get_macro _name = Warn.fatal "GPU_PTX get_macro has not been implemented"
let is_data _reg _ins = Warn.fatal "GPU_PTX is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "GPU_PTX map_addrs has not been implemented"

"ARM and PPC do the same..."
let fold_addrs _f c _ins = c

let pp_instruction _m ins = dump_instruction ins

let get_next _ins = Warn.fatal "GPU_PTX get_next not implemented"

let allowed_for_symb = []

let set_shared _i = Warn.fatal "GPU_PTX set_shared has not been implemented"
let set_global _i = Warn.fatal "GPU_PTX set_global has not been implmeneted"

let get_reg_list _i = Warn.fatal "Litmus GPU_PTX does not implement get_reg_list"

include ScopeTree
include MemSpaceMap
