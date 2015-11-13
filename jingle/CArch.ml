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
include Arch.MakeArch(struct
  open CBase

  include Arch.MakeCommon(CBase)

  let rec wrap_pseudo = function 
    | [] -> []
    | i::is -> (Instruction i)::(wrap_pseudo is)
       
  let rec unwrap_pseudo = function 
    | [] -> []
    | (Instruction i)::is -> i::(unwrap_pseudo is)
    | (Label(_,p))::is -> (unwrap_pseudo [p])@(unwrap_pseudo is)
    | Nop:: is -> unwrap_pseudo is
    | _ -> assert false

  let match_location subs pat instr = match pat,instr with
    | CBase.Reg s,CBase.Reg s'
    | Mem s,Mem s' ->
       Some(add_subs [Reg(s,s')] subs)
    | _ -> None
       
  let rec match_expr subs pat instr = match pat,instr with
    | Const(Constant.Symbolic s),Const(Constant.Concrete c) ->
     Some(add_subs [Cst(s, c)] subs)
    | Const(Constant.Concrete s),Const(Constant.Concrete c) 
      when c=s ->
       Some subs
    | Load(l,mo),Load(l',mo') when mo=mo' ->
       match_location subs l l'
    | Op(op,ex1,ex2),Op(op',ex1',ex2') when op=op' ->
       begin match match_expr subs ex1 ex1' with
       | None -> None
       | Some subs ->
	  match_expr subs ex2 ex2'
       end
    | Exchange(l,ex,mo),Exchange(l',ex',mo') when mo=mo' ->
       begin match match_location subs l l' with
       | None -> None
       | Some subs -> match_expr subs ex ex'
       end
    | Fetch(l,op,ex,mo),Fetch(l',op',ex',mo') when mo=mo' && op=op' ->
       begin match match_location subs l l' with
       | None -> None
       | Some subs -> match_expr subs ex ex'
       end
    | _ -> None
       
  let rec match_instr subs pattern instr = match pattern,instr with
    | Fence b,Fence b' when b = b'->
       Some subs
    | Seq l, Seq l' -> 
       let rec aux subs ips iis = match subs,ips,iis with
	 | None,_,_ -> None
	 | Some _ as subs,[],[] -> subs
	 | Some subs,ip::ips,ii::iis ->
	    aux (match_instr subs ip ii) ips iis
	 | _ -> None
       in aux (Some subs) l l'
    | If(c,t,e),If(c',t',e') -> begin 
      match match_expr subs c c' with
      | None -> None
      | Some subs ->
	 match match_instr subs t t' with
	 | None -> None
	 | Some subs ->
	    match e,e' with
	    | None,None -> Some subs
	    | Some e,Some e' -> match_instr subs e e'
	    | _ -> None
    end
    | Store(l,ex,mo),Store(l',ex',mo') when mo=mo' ->
       begin match match_location subs l l' with
       | None -> None
       | Some subs -> match_expr subs ex ex'
       end
    | Lock l,Lock l' -> match_location subs l l'
    | Unlock l,Unlock l' -> match_location subs l l'
    | Symb s,Seq l -> 
       Some(add_subs [Code(s,wrap_pseudo l)] subs)
    | Symb s,ins -> 
       Some(add_subs [Code(s,wrap_pseudo [ins])] subs)
    | _ -> None


  let rec expl_instr subs free =
    let conv_reg = conv_reg subs free in
    let find_code s =
      let rec aux = function
	| [] -> raise (Error("No conversion found for code "^s))
	| Code(n,c)::_ when String.compare n s = 0 ->
	   Seq(unwrap_pseudo c)
	| _::subs -> aux subs
      in aux subs
    in
    let find_cst s =
      let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 -> Constant.Concrete i
      | _::subs -> aux subs
      in aux subs
    in
    let expl_loc = function
      | CBase.Reg s -> CBase.Reg(conv_reg s)
      | Mem s -> Mem(conv_reg s)
    in
    let rec expl_expr = function
      | Const(Constant.Symbolic s) -> Const(find_cst s)
      | Load(l,mo) -> Load(expl_loc l,mo)
      | Op(op,e1,e2) -> Op(op,expl_expr e1,expl_expr e2)
      | Exchange(l,e,mo) -> Exchange(expl_loc l, expl_expr e,mo)
      | Fetch(l,op,e,mo) -> Fetch(expl_loc l,op,expl_expr e,mo)
      | x -> x 
    in
    function
    | Fence b -> Fence b
    | Seq l -> Seq(List.map (expl_instr subs free) l)
    | If(c,t,e) -> 
       let e = match e with 
	 | None -> None
	 | Some e -> Some(expl_instr subs free e)
       in
       If(expl_expr c,expl_instr subs free t,e)
    | Store(l,e,mo) -> Store(expl_loc l, expl_expr e,mo)
    | Lock l -> Lock(expl_loc l)
    | Unlock l -> Unlock(expl_loc l)
    | Symb s -> find_code s
       
end)
