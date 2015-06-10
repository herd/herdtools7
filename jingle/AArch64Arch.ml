include AArch64Base

exception Error of string 

type mcst = MetaConst.k

type substitution = 
  | Reg of string * reg
  | Cst of string * int

let sr_name = function
  | Symbolic_reg s -> s
  | _ -> raise (Error "Not a symbolic register.")

let cv_name = function
  | MetaConst.Meta s -> s
  | _ -> raise (Error "Not a constant variable.")

let rec match_instruction subs instr pattern = match pattern,instr with
  | I_FENCE _,I_FENCE _
  | I_B _, I_B _
  | I_BC _,I_BC _
    -> Some subs

  | I_CBZ(_,r,_),I_CBZ(_,r',_)
  | I_CBNZ(_,r,_),I_CBNZ(_,r',_)
  | I_MOV(_,r,_),I_MOV(_,r',_)
    -> Some(add_subs [Reg(sr_name r,r')] subs)

  | I_LDAR(_,_,r1,r2),I_LDAR(_,_,r1',r2')
  | I_STLR(_,r1,r2),I_STLR(_,r1',r2')
  | I_SXTW(r1,r2),I_SXTW(r1',r2')
    -> Some(add_subs [Reg(sr_name r1,r1');Reg(sr_name r2,r2')] subs)

  | I_STXR(_,_,r1,r2,r3),I_STXR(_,_,r1',r2',r3') 
    -> Some(add_subs [Reg(sr_name r1,r1');
		      Reg(sr_name r2,r2');
		      Reg(sr_name r3,r3')]
		     subs)
     
  | I_LDR(_,r1,r2,kr),I_LDR(_,r1',r2',kr')
  | I_STR(_,r1,r2,kr),I_STR(_,r1',r2',kr')
  | I_OP3(_,_,r1,r2,kr),I_OP3(_,_,r1',r2',kr')
    -> begin match match_kr subs kr kr' with 
	     | Some subs -> Some(add_subs [Reg(sr_name r1,r1');
					   Reg(sr_name r2,r2')]
					  subs)
	     | None -> None
       end

  | _,_ -> None

and match_kr subs kr kr' = match kr,kr' with
  | K(MetaConst.Meta _ as m),K(MetaConst.Int i) -> 
     Some(add_subs [Cst(cv_name m, i)] subs)
  | RV(_,r),RV(_,r') -> Some(add_subs [Reg(sr_name r,r')] subs)
  | _ -> None

and add_subs s s' = match s with
  | [] -> s'
  | s::ss -> 
     if List.mem s s'
     then add_subs ss s'
     else add_subs ss (s::s')

let rec map_pseudos f = 
  let rec aux = function
    | Nop -> Nop
    | Instruction ins -> Instruction (f ins)
    | Label (lbl,ins) -> Label (lbl, aux ins)
    | Macro (_,_) -> assert false
  in function
  | [] -> []
  | i::is -> (aux i)::(map_pseudos f is)
	
let instanciate_with subs free instrs =
  let get_register = 
    let env,free = ref [],ref free in
    fun s -> try List.assoc s !env with
       | Not_found ->
	  let r = List.hd !free in
	  env := (s,r)::!env;
	  free := List.tl !free;
	  r in
  let find_cst s = 
    let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 -> MetaConst.Int i
      | _::subs -> aux subs
    in aux subs in
  let conv_reg = function
    | Symbolic_reg s ->
       let rec aux = function
	 | [] -> get_register s
	 | Reg(n,r)::_ when String.compare n s = 0 -> r
	 | _::subs -> aux subs
       in aux subs
    | r -> r in

  let expl = 
    let expl_kr = function
      | RV(a,r) -> RV(a,conv_reg r)
      | K(MetaConst.Meta v) -> K(find_cst v)
      | kr -> kr in
    function
    | I_CBZ(a,r,b) -> I_CBZ(a,conv_reg r,b)
    | I_CBNZ(a,r,b) -> I_CBNZ(a,conv_reg r,b)
    | I_MOV(a,r,b) -> I_MOV(a,conv_reg r,b)
    | I_LDAR(a,b,r1,r2) -> I_LDAR(a,b,conv_reg r1,conv_reg r2)
    | I_STLR(a,r1,r2) -> I_STLR(a,conv_reg r1,conv_reg r2)
    | I_SXTW(r1,r2) -> I_SXTW(conv_reg r1,conv_reg r2)
    | I_STXR(a,b,r1,r2,r3) -> I_STXR(a,b,conv_reg r1,conv_reg r2,conv_reg r3)
    | I_LDR(a,r1,r2,kr) -> I_LDR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_STR(a,r1,r2,kr) -> I_STR(a,conv_reg r1,conv_reg r2,expl_kr kr)
    | I_OP3(a,b,r1,r2,kr) -> I_OP3(a,b,conv_reg r1,conv_reg r2,expl_kr kr)
    | instr -> instr
  in
  map_pseudos expl instrs

