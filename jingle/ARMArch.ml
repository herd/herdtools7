include ARMBase

exception Error of string 

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
    | Symbolic s::is -> "codevar:"^s^"\n"^(dump_pseudos is)
    | _ -> assert false

let rec add_subs s s' = match s with
  | [] -> s'
  | s::ss -> 
     if List.mem s s'
     then add_subs ss s'
     else add_subs ss (s::s')

let match_instr subs pattern instr = match pattern,instr with
  | I_ADD(_,r1,r2,MetaConst.Meta m),I_ADD(_,r1',r2',i) 
  | I_SUB(_,r1,r2,MetaConst.Meta m),I_SUB(_,r1',r2',i) 
  | I_AND(_,r1,r2,MetaConst.Meta m),I_AND(_,r1',r2',i) ->
     Some (add_subs [Cst(m,i);
		     Reg(sr_name r1,r1');
		     Reg(sr_name r2,r2')] subs)
  | I_ADD(_,r1,r2,MetaConst.Int i),I_ADD(_,r1',r2',i')
  | I_SUB(_,r1,r2,MetaConst.Int i),I_SUB(_,r1',r2',i')
  | I_AND(_,r1,r2,MetaConst.Int i),I_AND(_,r1',r2',i') when i=i'->
     Some (add_subs [Reg(sr_name r1,r1');
		     Reg(sr_name r2,r2')] subs)
  | I_ADD3(_,r1,r2,r3),I_ADD3(_,r1',r2',r3') 
  | I_SUB3(_,r1,r2,r3),I_SUB3(_,r1',r2',r3') 
  | I_XOR(_,r1,r2,r3),I_XOR(_,r1',r2',r3') ->
     Some (add_subs [Reg(sr_name r1,r1');
		     Reg(sr_name r2,r2');
		     Reg(sr_name r3,r3')] subs)
  | I_B l,I_B l'
  | I_BEQ l,I_BEQ l'
  | I_BNE l,I_BNE l' ->
     Some (add_subs [Lab(l,l')] subs)
  | I_CB(b,r,l),I_CB(b',r',l') when b=b' ->
     Some (add_subs [Lab(l,l');Reg(sr_name r,r')] subs)
  | I_CMPI(r,MetaConst.Meta m),I_CMPI(r',i) ->
     Some (add_subs [Reg(sr_name r,r');Cst(m,i)] subs)
  | I_CMPI(r,MetaConst.Int i),I_CMPI(r',i') when i=i' ->
     Some (add_subs [Reg(sr_name r,r')] subs)
  | I_CMP(r1,r2),I_CMP(r1',r2')
  | I_LDREX(r1,r2),I_LDREX(r1',r2') ->
     Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)
  | I_LDR(r1,r2,c),I_LDR(r1',r2',c')
  | I_STR(r1,r2,c),I_STR(r1',r2',c')
  | I_MOV(r1,r2,c),I_MOV(r1',r2',c') when c=c' ->
     Some (add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)
  | I_LDR3(r1,r2,r3,c),I_LDR3(r1',r2',r3',c')
  | I_STR3(r1,r2,r3,c),I_STR3(r1',r2',r3',c')
  | I_STREX(r1,r2,r3,c),I_STREX(r1',r2',r3',c') when c=c' ->
     Some (add_subs [Reg(sr_name r1,r1');
		     Reg(sr_name r2,r2');
		     Reg(sr_name r3,r3')] subs)
  | I_MOVI(r,MetaConst.Meta m,c),I_MOVI(r',i,c') when c=c' ->
     Some (add_subs [Reg(sr_name r,r');Cst(m,i)] subs)
  | I_MOVI(r,MetaConst.Int i,c),I_MOVI(r',i',c') when i=i' && c=c' ->
     Some (add_subs [Reg(sr_name r,r')] subs)
  | I_DMB b,I_DMB b'
  | I_DSB b,I_DSB b' when barrier_compare b b' = 0 ->
     Some subs
  | I_ISB,I_ISB -> Some subs
  | I_SADD16(r1,r2,r3),I_SADD16(r1',r2',r3')
  | I_SEL(r1,r2,r3),I_SEL(r1',r2',r3') ->
     Some (add_subs [Reg(sr_name r1,r1');
		     Reg(sr_name r2,r2');
		     Reg(sr_name r3,r3')] subs)
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
    function
    | I_ADD(f,r1,r2,MetaConst.Meta v) -> 
       I_ADD(f,conv_reg r1,conv_reg r2,find_cst v)
    | I_SUB(f,r1,r2,MetaConst.Meta v) -> 
       I_SUB(f,conv_reg r1,conv_reg r2,find_cst v)
    | I_AND(f,r1,r2,MetaConst.Meta v) -> 
       I_AND(f,conv_reg r1,conv_reg r2,find_cst v)
    | I_ADD(f,r1,r2,c) -> 
       I_ADD(f,conv_reg r1,conv_reg r2,c)
    | I_SUB(f,r1,r2,c) -> 
       I_SUB(f,conv_reg r1,conv_reg r2,c)
    | I_AND(f,r1,r2,c) -> 
       I_AND(f,conv_reg r1,conv_reg r2,c)
    | I_ADD3(f,r1,r2,r3) -> 
       I_ADD3(f,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_SUB3(f,r1,r2,r3) -> 
       I_SUB3(f,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_XOR(f,r1,r2,r3) -> 
       I_XOR(f,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_B l -> I_B(find_lab l)
    | I_BEQ l -> I_BEQ(find_lab l)
    | I_BNE l -> I_BNE(find_lab l)
    | I_CB(b,r,l) -> I_CB(b,conv_reg r,find_lab l)
    | I_CMPI(r,MetaConst.Meta v) -> I_CMPI(conv_reg r, find_cst v)
    | I_CMPI(r,c) -> I_CMPI(conv_reg r, c)
    | I_CMP(r1,r2) -> I_CMP(conv_reg r1,conv_reg r2)
    | I_LDREX(r1,r2) -> I_LDREX(conv_reg r1,conv_reg r2)
    | I_LDR(r1,r2,c) -> I_LDR(conv_reg r1,conv_reg r2,c)
    | I_STR(r1,r2,c) -> I_STR(conv_reg r1,conv_reg r2,c)
    | I_MOV(r1,r2,c) -> I_MOV(conv_reg r1,conv_reg r2,c)
    | I_LDR3(r1,r2,r3,c) -> I_LDR3(conv_reg r1,conv_reg r2,conv_reg r3,c)
    | I_STR3(r1,r2,r3,c) -> I_STR3(conv_reg r1,conv_reg r2,conv_reg r3,c)
    | I_STREX(r1,r2,r3,c) -> I_STREX(conv_reg r1,conv_reg r2,conv_reg r3,c)
    | I_MOVI(r,MetaConst.Meta v,c) -> I_MOVI(conv_reg r, find_cst v,c)
    | I_MOVI(r,v,c) -> I_MOVI(conv_reg r,v,c)
    | I_DMB b -> I_DMB b
    | I_DSB b -> I_DSB b
    | I_ISB -> I_ISB
    | I_SADD16(r1,r2,r3) -> I_SADD16(conv_reg r1,conv_reg r2,conv_reg r3)
    | I_SEL(r1,r2,r3) -> I_SEL(conv_reg r1,conv_reg r2,conv_reg r3)
   
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

