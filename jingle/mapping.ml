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
 open Printf 

exception Error of string

module type Config = sig
    module Source : Arch.S
    module Target : Arch.S
    val conversions : (string * string) list 
  end

module Make(C:Config) = struct
	   
  module Source = C.Source
  module Target = C.Target

  module Env = struct
    type sub = {
      reg : (Source.reg * Target.reg) list;
      addr: (string * Target.reg) list;
      lab : (string * string) list
    }
		 
    type t = sub * Target.reg list
			      
    let init = {reg = []; lab = []; addr = []},Target.allowed_for_symb 
			    
    let get_register_from_reg (binds,free) reg =
      try (List.assoc reg binds.reg,(binds,free)) with
      | Not_found ->
	 match free with
	 | [] -> raise (Error "No fresh register available.")
	 | r::fs -> r,({binds with reg=(reg,r)::binds.reg},fs)

    let get_register_from_addr (binds,free) addr =
      try (List.assoc addr binds.addr,(binds,free)) with
      | Not_found ->
	 match free with
	 | [] -> raise (Error "No fresh register available.")
	 | r::fs -> r,({binds with addr=(addr,r)::binds.addr},fs)

    let get_label = 
      let fresh_label = 
	let i = ref 0 in (fun () -> incr i;"label"^(string_of_int !i)) in
      fun (binds,free) l -> 
      try (List.assoc l binds.lab,(binds,free)) with
      | Not_found -> let lbl = fresh_label () in
		     lbl,({binds with lab=(l,lbl)::binds.lab},free)

    let get_lab_convs (binds,_) =
      List.map (fun (s,t) -> Target.Lab(s,t)) binds.lab

    let get_free_register (_,free) = free
	    
  end
				  
  let conversions = 
    List.map
      (fun (s,t) ->
       let s = 
	 try Source.Parser.instr_from_string s with
	 | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" s;
		raise e in 
       let t = 
	 try Target.Parser.instr_from_string t with
	 | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" t;
		raise e in
       (s,t)
      )
      C.conversions

  let rec dig subs pat instr = match pat,instr with
    | [],_ -> Some (instr,[])
    | _::_,[] -> None
    | p::ps,i::is -> 
       match Source.match_instruction subs p i with
       | Some _ -> Some([],i::is)
       | None -> 
	  match dig subs (p::ps) is with
	  | None -> None
	  | Some(stash,rem) -> Some(i::stash,rem)
	 
  let rec find_pattern pat instrs subs = 
    let open Source in
    match pat,instrs with
    | pat,Nop::instrs
    | Nop::pat,instrs -> 
       find_pattern pat instrs subs

    | pat,Label(l,Nop)::i::is ->
       find_pattern pat (Label(l,i)::is) subs
    | pat,Label(_,Nop)::[] ->
       find_pattern pat [] subs

    | Symbolic s::pat,instrs ->
       begin 
	 match dig subs pat instrs with
	 | None -> None
	 | Some(stash,rem) -> 
	    find_pattern pat rem (Code(s,stash)::subs)
       end

    | p::ps,i::is ->
       begin
	 match match_instruction subs p i with
	 | None -> None
	 | Some subs ->
	    match find_pattern ps is subs with
	    | None -> None
	    | Some(is,rs,subs) -> Some(i::is,rs,subs)
       end
	 
    | [],rs -> 
       Some([],rs,subs)

    | _,_ -> None

  let get_pattern_seq instrs = 
    let rec aux instrs = 
      let rec find = function
	| [] -> begin
		match find_pattern [] instrs [] with 
		| Some(is,[],[]) -> Some((is,[],[]),[]) 
		| _ -> eprintf "Unmatched instructions:\n%s" 
			       (Source.dump_pseudos instrs);
		       None
	      end
	| (p,conv)::ps ->
	   match find_pattern p instrs [] with
	   | None -> find ps
	   | Some(is,rs,subs) -> 
	      match is,conv with
	      | Source.Label(l,_)::_ as is,(Target.Instruction(_) as c)::cs 
		-> Some((is,Target.Label(l,c)::cs,subs),rs)
	      | _,_ -> Some((is,conv,subs),rs)
      in
      match find conversions with
      | None -> raise (Error "Cannot find conversion rule.")
      | Some(ins,[]) -> [ins]
      | Some(ins,rs) -> ins::(aux rs)
    in aux instrs

  let rec convert env instrs =
    let rec aux env l = match l with
      | [] -> [],env
      | (_src,tgt,subs)::ts ->
	 let conv,env =
	   List.fold_left
	     (fun (cv,env) -> function
		  | Source.Reg(s,c) ->
		     let r,env = Env.get_register_from_reg env c in
		     (Target.Reg(s,r)::cv,env)
		  | Source.Addr(s,a) ->
		      let r,env = Env.get_register_from_addr env a in
		     (Target.Reg(s,r)::cv,env)
		  | Source.Cst(s,c) -> (Target.Cst(s,c)::cv,env)
		  | Source.Lab(s,l) -> 
		     let lbl,env = Env.get_label env l in
		     (*eprintf "%s,%s,%s\n" l s lbl;*)
		     (Target.Lab(s,lbl)::cv,env)
		  | Source.Code(s,c) ->
		     let c,env = convert env c in
		     (Target.Code(s,c)::cv,env)
	     )
	     ([],env) subs in
	 let flw,env = aux env ts
	 in (tgt,(Env.get_lab_convs env)@conv)::flw,env
    in 
    let chunks,env = aux env (get_pattern_seq instrs) in
    let chunks = List.map (fun (tgt,conv) -> 
			   Target.instanciate_with
			     conv (Env.get_free_register env) tgt)
			  chunks in
    let pseudo_p = List.flatten chunks in
    (pseudo_p,env)

  let reg_mapping = 
    List.map (fun (i,(b,_)) ->
	      (i,
	      (List.map (fun (sr,tr) -> 
			 (Source.pp_reg sr,Target.pp_reg tr))
			b.Env.reg)))

  let addr_init = 
    let open MiscParser in
    List.fold_left (fun acc (i,(b,_)) ->
		    acc@
		      (List.map (fun (sa,tr) -> 
				 (Location_reg(i,Target.pp_reg tr),
				  (TyDef,SymbConstant.nameToV sa)))
				b.Env.addr)
		   ) []
		
  let rec dump_map = 
    let rec assocs i = function
      | [] -> ""
      | [sr,tr] -> (string_of_int i)^":"^tr^"="^sr
      | (sr,tr)::r -> (string_of_int i)^":"^tr^"="^sr^","^(assocs i r)
    in
    function
    | [] -> ""
    | [i,asc] -> assocs i asc
    | (i,asc)::r -> 
       let s = assocs i asc in
       if String.compare s "" = 0 
       then dump_map r 
       else s^","^(dump_map r)

  let conv_loc map = MiscParser.(function
    | Location_reg(i,r) ->  
       let r' = try let asc = List.assoc i map in 
		    List.assoc r asc with
		| Not_found -> 
		   raise (Error("Register "^r^" does not appear."))
       in Location_reg(i,r')
    | l -> l)

  let translate chin sres =
    let src = Source.Parser.parse chin sres in
    let open MiscParser in
    let prog = List.map (fun (i,p) ->
	        let p,e = convert Env.init p in
		((i,p),(i,e))) src.prog in
    let prog,convs = List.split prog in
    let map = reg_mapping convs in
    let init = 
      addr_init convs @ 
	List.map (fun (l,r) -> (conv_loc map l,r)) src.init in
    let condition =
      ConstrGen.(map_constr
		   (function
		     | LV(l,v) -> LV(conv_loc map l,v)
		     | LL(l1,l2) -> LL(conv_loc map l1,conv_loc map l2))
		   src.condition) in
    { info = ("Mapping",dump_map map)::src.info;
      init = init;
      prog = prog;
      condition = condition;
      locations = src.locations;
      extra_data = src.extra_data;
    }

end
