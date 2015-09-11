(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
include AArch64Base

exception Error of string 

type mcst = MetaConst.k

type substitution = 
  | Reg of string * reg
  | Cst of string * int
  | Lab of string * string
  | Addr of string * string
  | Code of string * pseudo list

let sr_name = function
  | Symbolic_reg s -> s
  | _ -> raise (Error "Not a symbolic register.")

let cv_name = function
  | MetaConst.Meta s -> s
  | _ -> raise (Error "Not a constant variable.")

let rec dump_pseudos = function
    | [] -> ""
    | Nop::is -> "*Nop*\n" ^dump_pseudos is
    | Label(s,i)::is -> s^": "^(dump_pseudos (i::is))
    | Instruction i::is -> dump_instruction i ^" ;\n"^
			     (dump_pseudos is)
    | _ -> assert false

let rec add_subs s s' = match s with
  | [] -> s'
  | s::ss -> 
     if List.mem s s'
     then add_subs ss s'
     else add_subs ss (s::s')

let match_kr subs kr kr' = match kr,kr' with
  | K(MetaConst.Meta m),K i ->
     Some(add_subs [Cst(m, i)] subs)
  | RV(_,r),RV(_,r') -> Some(add_subs [Reg(sr_name r,r')] subs)
  | K(MetaConst.Int i),K(j) when i=j -> Some subs
  | _ -> None

let match_instr subs pattern instr = match pattern,instr with
  | I_FENCE fp,I_FENCE fi when fp = fi
    -> Some subs

  | I_B lp, I_B li
    -> Some(add_subs [Lab(lp,li)] subs)

  | I_BC(cp,lp), I_BC(ci,li) when cp = ci
    -> Some(add_subs [Lab(lp,li)] subs)

  | I_CBZ(_,r,lp),I_CBZ(_,r',li)
  | I_CBNZ(_,r,lp),I_CBNZ(_,r',li)
    -> Some(add_subs [Reg(sr_name r,r');
		      Lab(lp,li)] subs)
	   
  | I_MOV(_,r,MetaConst.Meta m),I_MOV(_,r',i)
    -> Some(add_subs [Reg(sr_name r,r');
		      Cst(m,i)] subs)

  | I_LDAR(_,tp,r1,r2),I_LDAR(_,ti,r1',r2') when tp = ti
    -> Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)
	   
  | I_STLR(_,r1,r2),I_STLR(_,r1',r2')
  | I_SXTW(r1,r2),I_SXTW(r1',r2')
    -> Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)

  | I_STXR(_,tp,r1,r2,r3),I_STXR(_,ti,r1',r2',r3') when tp = ti 
    -> Some(add_subs [Reg(sr_name r1,r1');
		      Reg(sr_name r2,r2');
		      Reg(sr_name r3,r3')]
		     subs)
     
  | I_LDR(_,r1,r2,kr),I_LDR(_,r1',r2',kr')
  | I_STR(_,r1,r2,kr),I_STR(_,r1',r2',kr')
    -> begin match match_kr subs kr kr' with 
	     | Some subs 
	       -> Some(add_subs [Reg(sr_name r1,r1');
				 Reg(sr_name r2,r2')]
				subs)
	     | None -> None
       end

  | I_OP3(_,opp,r1,r2,kr),I_OP3(_,opi,r1',r2',kr') when opp=opi
    -> begin match match_kr subs kr kr' with 
	     | Some subs 
	       -> Some(add_subs [Reg(sr_name r1,r1');
				 Reg(sr_name r2,r2')]
				subs)
	     | None -> None
       end

  | _,_ -> None

let rec match_instruction subs pattern instr = match pattern,instr with
  | Label(lp,insp),Label(li,insi) 
    -> match_instruction (add_subs [Lab(lp,li)] subs) insp insi
  | Label _, _ -> None
  | pattern, Label(_,instr)
    -> match_instruction subs pattern instr
  | Instruction ip, Instruction ii 
    -> match_instr subs ip ii
  | _,_ -> assert false
	
let instanciate_with subs free instrs =
  let get_register =
    let env,free = ref [],ref free in
    fun s -> try List.assoc s !env with
       | Not_found ->
	  let r = List.hd !free in
	  env := (s,r)::!env;
	  free := List.tl !free;
	  r in
  let get_label = 
    let fresh_lbl = 
      let i = ref 0 in 
      fun () -> incr i;"lbl"^(string_of_int !i) in
    let env = ref [] in
    fun s -> try List.assoc s !env with
       | Not_found ->
	  let l = fresh_lbl () in
	  env := (s,l)::!env;
	  l in
  let find_cst s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 -> MetaConst.Int i
      | _::subs -> aux subs
    in aux subs in
  let find_code s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for code "^s))
      | Code(n,c)::_ when String.compare n s = 0 -> c
      | _::subs -> aux subs
    in aux subs in
  let find_lab l =
    let rec aux = function
      | [] -> get_label l
      | Lab(n,lbl)::_ when String.compare n l = 0 -> lbl
      | _::subs -> aux subs
    in aux subs in
  let conv_reg = function
    | Symbolic_reg s ->
       let rec aux = function
	 | [] -> get_register s
	 | Reg(n,r)::_ when String.compare n s = 0 -> r
	 | Addr(n,r)::_ when String.compare n s = 0 -> get_register r
	 | _::subs -> aux subs
       in aux subs
    | r -> r in

  let expl =
    let expl_kr = function
      | RV(a,r) -> RV(a,conv_reg r)
      | K(MetaConst.Meta v) -> K(find_cst v)
      | kr -> kr in
    function
    | I_FENCE b -> I_FENCE b
    | I_B l -> I_B(find_lab l)
    | I_BC(a,l) -> I_BC(a,find_lab l)
    | I_CBZ(a,r,l) -> I_CBZ(a,conv_reg r,find_lab l)
    | I_CBNZ(a,r,l) -> I_CBNZ(a,conv_reg r,find_lab l)
    | I_MOV(a,r,MetaConst.Meta v) -> I_MOV(a,conv_reg r,find_cst v)
    | I_MOV(a,r,c) -> I_MOV(a,conv_reg r,c)
    | I_LDAR(a,b,r1,r2) -> I_LDAR(a,b,conv_reg r1,conv_reg r2)
    | I_STLR(a,r1,r2) -> I_STLR(a,conv_reg r1,conv_reg r2)
    | I_SXTW(r1,r2) -> I_SXTW(conv_reg r1,conv_reg r2)
    | I_STXR(a,b,r1,r2,r3) -> I_STXR(a,b,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_LDR(a,r1,r2,kr) -> I_LDR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_STR(a,r1,r2,kr) -> I_STR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_OP3(a,b,r1,r2,kr) -> I_OP3(a,b,conv_reg r1,conv_reg r2,expl_kr kr)
  in 
  let rec expl_pseudos = 
    let rec aux = function
      | Nop -> []
      | Instruction ins -> [pseudo_parsed_tr (Instruction (expl ins))]
      | Label (lbl,ins) ->  begin
	 match aux ins with
	 | [] -> [pseudo_parsed_tr (Label (find_lab lbl, Nop))]
	 | h::t -> Label(find_lab lbl,h)::t
	end
      | Symbolic s -> find_code s
      | Macro (_,_) -> assert false
    in function
    | [] -> []
    | i::is -> (aux i)@(expl_pseudos is)
  
  in expl_pseudos instrs

