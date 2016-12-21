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
  open Printf
  open CBase

  let debug = false

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

  let rec match_location subs pat instr =  match_expr subs pat instr
       
  and match_expr subs pat instr =
    let r =  match pat,instr with
    | Const(Constant.Symbolic s),Const(Constant.Concrete c) ->
        Some(add_subs [Cst(s, c)] subs)
    | Const(Constant.Concrete s),Const(Constant.Concrete c) 
      when c=s ->
       Some subs
    | LoadReg(l),LoadReg(l') ->
        (* Awful ack to encode address registers... *)
        let to_add = match symb_reg_name l with
        | None -> Reg (l,l')
        | Some s -> Reg (s,l') in
        Some (add_subs [to_add] subs)
    | LoadMem(l,mo),LoadMem(l',mo') when mo=mo' ->
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
    | _ -> None in
    if debug then
      eprintf "Match_expr pat=<%s> expr=<%s> -> %s\n"
        (CBase.dump_expr pat) (CBase.dump_expr instr)
        (match r with Some _ -> "ok" | None -> "no") ;
    r
       
  let rec match_instr subs pattern instr =
    let r = match pattern,instr with
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
    | StoreReg (r,ex),StoreReg(r',ex') ->
        match_expr (add_subs [Reg (sr_name r,r')] subs) ex ex'
    | StoreMem(l,ex,mo),StoreMem(l',ex',mo') when mo=mo' ->
        begin match match_location subs l l' with
       | None -> None
       | Some subs ->
           match_expr subs ex ex'
       end
    | Lock (l,MutexC11),Lock (l',MutexC11) -> match_location subs l l'
    | Lock (l,MutexLinux),Lock (l',MutexLinux) -> match_location subs l l'
    | Unlock (l,MutexC11),Unlock (l',MutexC11) -> match_location subs l l'
    | Unlock (l,MutexLinux),Unlock (l',MutexLinux) -> match_location subs l l'
    | Symb s,Seq l -> 
       Some(add_subs [Code(s,wrap_pseudo l)] subs)
    | Symb s,ins -> 
       Some(add_subs [Code(s,wrap_pseudo [ins])] subs)
    | _ -> None in
    if debug then
      eprintf "Match Instr <%s> <%s> -> %s\n"
        (dump_instruction pattern)
        (dump_instruction instr)
        (match r with Some _ -> "ok" | None -> "no") ;
    r

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
    let rec expl_loc loc = expl_expr loc

    and expl_expr = function
      | Const(Constant.Symbolic s) -> Const(find_cst s)
      | Const(Constant.Concrete _) as e -> e
      | LoadReg r -> LoadReg(conv_reg r)
      | LoadMem(l,mo) -> LoadMem(expl_loc l,mo)
      | Op(op,e1,e2) -> Op(op,expl_expr e1,expl_expr e2)
      | Exchange(l,e,mo) -> Exchange(expl_loc l, expl_expr e,mo)
      | Fetch(l,op,e,mo) -> Fetch(expl_loc l,op,expl_expr e,mo)
      | ECall (f,es) -> ECall (f,List.map expl_expr es)
      | ECas (e1,e2,e3,mo1,mo2,st) -> ECas (expl_expr e1,expl_expr e2,expl_expr e3,mo1,mo2,st)
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
    | StoreReg(r,e) -> StoreReg(r, expl_expr e)
    | StoreMem(l,e,mo) -> StoreMem(expl_loc l, expl_expr e,mo)
    | Lock (l,k) -> Lock(expl_loc l,k)
    | Unlock (l,k) -> Unlock(expl_loc l,k)
    | Symb s -> find_code s
    | PCall (f,es) -> PCall (f,List.map expl_expr es)
end)
