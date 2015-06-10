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
      reg : (Source.reg * Target.reg) list
    }
		 
    type t = sub * Target.reg list
			      
    let init = {reg = []},Target.allowed_for_symb 
			    
    let get_concrete_register (binds,free) reg =
      try (List.assoc reg binds.reg,(binds,free)) with
      | Not_found ->
     match free with
     | [] -> raise (Error "No fresh register available.")
     | r::fs -> r,({reg=(reg,r)::binds.reg},fs)
	
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

  let rec find_pattern pat instrs subs = 
    let open Source in
    match pat,instrs with
    | [],rs -> 
       Some([],rs,subs)

    | Nop::pat,instrs
    | pat,Nop::instrs -> 
       find_pattern pat instrs subs

    | Label(_,p)::ps,instrs -> find_pattern (p::ps) instrs subs
    | pat,Label(_,i)::is -> find_pattern pat (i::is) subs
       
    | (Instruction p)::ps,(Instruction i)::is ->
       begin
	 match match_instruction [] p i with
	 | None -> None
	 | Some subs ->
	    match find_pattern ps is subs with
	    | None -> None
	    | Some(is,rs,subs) -> Some(i::is,rs,subs)
       end
	 
    | _,_ -> None

  and get_pattern_seq instrs = 
    let rec aux instrs = 
      let rec find = function 
	| [] -> None
	| (p,conv)::ps -> match find_pattern p instrs [] with
			  | None -> find ps
			  | Some(is,rs,subs) -> Some((is,conv,subs),rs)
      in
      match find conversions with
      | None -> raise (Error "Cannot find conversion rule.")
      | Some(ins,[]) -> [ins]
      | Some(ins,rs) -> ins::(aux rs)
    in aux instrs

  and convert env instrs =
    let rec aux env = function
      | [] -> []
      | (src,tgt,subs)::ts -> 
	 let conv,env = 
	   List.fold_left 
	     (fun (cv,env) -> function
		  | Source.Reg(s,c) -> 
		     let r,env = Env.get_concrete_register env c in
		     (Target.Reg(s,r)::cv,env)
		  | Source.Cst(s,c) -> (Target.Cst(s,c)::cv,env))
	     ([],env) subs in
	 let tgt = 
	   Target.instanciate_with 
	     conv (Env.get_free_register env) tgt
	 in tgt::(aux env ts)
    in List.flatten (aux env (get_pattern_seq instrs))
    
  and translate src = failwith ""
    

end
