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

  include Arch.MakeCommon(CBase)

  let debug = false


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
    | Const(Constant.Symbolic ((s,_),_)),Const(Constant.Concrete c) ->
        let c = ParsedConstant.Scalar.to_int c in
        add_subs [Cst(s, c)] subs
    | Const(Constant.Concrete s),Const(Constant.Concrete c)
      when c=s ->
       Some subs
    | LoadReg(l),LoadReg(l') ->
        (* Awful ack to encode address registers... *)
        let to_add = match symb_reg_name l with
        | None -> Reg (l,l')
        | Some s -> Reg (s,l') in
        add_subs [to_add] subs
    | LoadMem(l,mo),LoadMem(l',mo') when mo=mo' ->
       match_location subs l l'
    | Op(op,ex1,ex2),Op(op',ex1',ex2') when op=op' ->
       match_expr subs ex1 ex1' >>> fun subs -> match_expr subs ex2 ex2'
    | Exchange(l,ex,mo),Exchange(l',ex',mo') when mo=mo' ->
       match_location subs l l' >>> fun subs -> match_expr subs ex ex'
    | Fetch(l,op,ex,mo),Fetch(l',op',ex',mo') when mo=mo' && op=op' ->
       match_location subs l l' >>> fun subs -> match_expr subs ex ex'
    | ECall (f,es),ECall (g,fs) when f=g ->
        match_exprs subs es fs
    | _ -> None in
    if debug then
      eprintf "Match_expr pat=<%s> expr=<%s> -> %s\n"
        (CBase.dump_expr pat) (CBase.dump_expr instr)
        (match r with Some _ -> "ok" | None -> "no") ;
    r

  and match_exprs subs es fs = match es,fs with
  | [],[] -> Some subs
  | e::es,f::fs ->
      begin match match_expr subs e f with
      | None -> None
      | Some subs -> match_exprs subs es fs
      end
  | ([],_::_) | (_::_,[]) -> None

  let rec match_instr subs pattern instr =
    let r = match pattern,instr with
    | Fence b,Fence b' when b = b'->
       Some subs
    | Seq (l,b), Seq (l',b') when b=b' ->
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
    | DeclReg (_t,r),DeclReg(_t',r') (* when t = t' *) ->
        add_subs [Reg (sr_name r,r')] subs
    | StoreReg (_ot,r,ex),StoreReg(_ot',r',ex') (* when ot = ot' *) ->
        add_subs [Reg (sr_name r,r')] subs >>> fun subs ->
        match_expr subs ex ex'
    | StoreMem(l,ex,mo),StoreMem(l',ex',mo') when mo=mo' ->
        match_location subs l l' >>> fun subs ->
        match_expr subs ex ex'
    | Lock (l,MutexC11),Lock (l',MutexC11) -> match_location subs l l'
    | Lock (l,MutexLinux),Lock (l',MutexLinux) -> match_location subs l l'
    | Unlock (l,MutexC11),Unlock (l',MutexC11) -> match_location subs l l'
    | Unlock (l,MutexLinux),Unlock (l',MutexLinux) -> match_location subs l l'
    | PCall (f,es),PCall (g,fs) when f = g ->
        match_exprs subs es fs
    | Symb s,Seq (l,_) ->
       add_subs [Code(s,wrap_pseudo l)] subs
    | Symb s,ins ->
       add_subs [Code(s,wrap_pseudo [ins])] subs
    | _ -> None in
    if debug then
      eprintf "Match Instr <%s> <%s> -> %s\n"
        (dump_instruction pattern)
        (dump_instruction instr)
        (match r with Some _ -> "ok" | None -> "no") ;
    r

  let expl_instr subs =
    let conv_reg = conv_reg subs in

    let find_code s st =
      let rec aux = function
        | [] -> raise (Error("No conversion found for code "^s))
        | Code(n,c)::_ when Misc.string_eq n s ->
           Seq (unwrap_pseudo c,true)
        | _::subs -> aux subs
      in aux subs,st
    in

    let find_cst s st =
      let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when Misc.string_eq n s ->
          ParsedConstant.intToV i
      | _::subs -> aux subs
      in aux subs,st
    in

    let rec expl_expr = let open Constant in function
      | Const(Symbolic ((s,_),_)) -> find_cst s >! fun k -> Const k
      | Const(Concrete _|Label _|Tag _) as e -> unitT e
      | LoadReg r -> conv_reg r >! fun r -> LoadReg r
      | LoadMem (loc,mo) -> expl_expr loc >! fun loc -> LoadMem (loc,mo)
      | Op (op,e1,e2) ->
          expl_expr e1 >> fun e1 ->
          expl_expr e2 >! fun e2 ->
          Op (op,e1,e2)
      | Exchange(loc,e,mo) ->
          expl_expr loc >> fun loc ->
          expl_expr e >! fun e ->
          Exchange (loc,e,mo)
      | CmpExchange (loc,o,n,a) ->
          expl_expr loc >> fun loc ->
          expl_expr o >> fun o ->
          expl_expr n >! fun n ->
          CmpExchange(loc,o,n,a)
      | Fetch(loc,op,e,mo) ->
          expl_expr loc >> fun loc ->
          expl_expr e >! fun e ->
          Fetch(loc,op,e,mo)
      | ECall (f,es) ->
          mapT expl_expr es >! fun es -> ECall (f,es)
      | ECas (e1,e2,e3,mo1,mo2,st) ->
          expl_expr e1 >> fun e1 ->
          expl_expr e2 >> fun e2 ->
          expl_expr e3 >! fun e3 ->
          ECas (e1,e2,e3,mo1,mo2,st)
      | TryLock(e,m) -> expl_expr e >! fun e -> TryLock(e,m)
      | IsLocked(e,m) -> expl_expr e >! fun e -> IsLocked(e,m)
      | AtomicOpReturn (loc,op,e,ret,a) ->
          expl_expr loc >> fun loc ->
          expl_expr e   >! fun e ->
          AtomicOpReturn (loc,op,e,ret,a)
      | AtomicAddUnless (loc,u,a,rb) ->
          expl_expr loc >> fun loc ->
          expl_expr u >> fun u ->
          expl_expr a >! fun a ->
          AtomicAddUnless (loc,u,a,rb)
      | ExpSRCU (e,a) -> expl_expr e >! fun e -> ExpSRCU (e,a)
    in let rec expl_instr =  function
      | Fence _|DeclReg _ as i -> unitT i
      | Seq (is,b) ->
          mapT expl_instr is >! fun is -> Seq (is,b)
      | If(c,t,e) ->
          expl_expr c >> fun c ->
          expl_instr t >> fun t ->
          optT expl_instr e >! fun e ->
          If (c,t,e)
      | StoreReg(ot,r,e) ->
          conv_reg r >> fun r ->
          expl_expr e >! fun e ->
          StoreReg (ot,r,e)
      | StoreMem(loc,e,mo) ->
          expl_expr loc >> fun loc ->
          expl_expr e >! fun e ->
          StoreMem(loc,e,mo)
    | Lock (l,k) -> expl_expr l >! fun  l -> Lock(l,k)
    | Unlock (l,k) -> expl_expr l >! fun l -> Unlock(l,k)
    | Symb s -> find_code s
    | PCall (f,es) ->
        mapT expl_expr es >! fun es ->
        PCall (f,es)
    | AtomicOp(e1,op,e2) ->
        expl_expr e1 >> fun e1 ->
        expl_expr e2 >! fun e2 ->
        AtomicOp (e1,op,e2)
    | InstrSRCU (e,a,oe) ->
        expl_expr e >> fun e ->
        optT expl_expr oe >! fun oe ->
        InstrSRCU (e,a,oe) in
    expl_instr
end)
