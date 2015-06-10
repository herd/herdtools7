include BellBase

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
  | Pld(r,ao,s),Pld(r',ao',s') -> 
     begin
       match match_addr_op subs ao ao' with
       | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
       | None -> None
     end

  | Pst(ao,ri,s),Pst(ao',ri',s') ->
     begin
       match match_addr_op subs ao ao' with
       | Some subs -> match_reg_or_imm subs ri ri'
       | None -> None
     end

  | Prmw(op,s),Prmw(op',s') ->
     match_op subs op op'

  | Pfence _, Pfence _ 
    -> Some subs

  | Pbranch(_,_,_), Pbranch(_,_,_) 
    -> failwith "TODO"

  | _,_ -> None

and match_op subs op op' = match op,op' with
  | Add(r,iar1,iar2),Add(r',iar1',iar2') 
  | And(r,iar1,iar2),And(r',iar1',iar2') ->
     begin match (match_iar subs iar1 iar1') with
	   | Some subs -> 
	      begin match (match_iar subs iar2 iar2') with
		    | Some subs -> Some(add_subs [Reg(sr_name r,r')] subs)
		    | None -> None
	      end
	   | None -> None
     end
  | Mov(r,iar),Mov(r',iar') ->
     begin match (match_iar subs iar iar') with
	   | Some subs -> Some(add_subs [Reg(sr_name r,r')] subs)
	   | None -> None
     end
  | _,_ -> None
       
and match_iar subs iar iar' = match iar,iar' with
  | IAR_roa ra,IAR_roa ra' 
    -> match_reg_or_addr subs ra ra'
  | IAR_imm (MetaConst.Meta _ as m), IAR_imm (MetaConst.Int i) 
    -> Some(add_subs [Cst(cv_name m,i)] subs)
  | _,_ -> None

and match_reg_or_addr subs ra ra' = match ra,ra' with
  | Rega r,Rega r' -> Some(add_subs [Reg(sr_name r,r')] subs)
  | x,y when (compare x y)=0 -> Some subs
  | _,_ -> None

and match_addr_op subs ao ao' = match ao,ao' with
  | Addr_op_atom ra,Addr_op_atom ra'
    -> match_reg_or_addr subs ra ra'
  | Addr_op_add(ra,ri),Addr_op_add(ra',ri') ->
     begin
       match match_reg_or_addr subs ra ra' with
       | Some subs -> match_reg_or_imm subs ri ri'
       | None -> None
     end
  | _,_ -> None

and match_reg_or_imm subs ri ri' = match ri,ri' with
  | Regi r,Regi r' 
    -> Some(add_subs [Reg(sr_name r,r')] subs)
  | Imm(MetaConst.Meta _ as m),Imm(MetaConst.Int i)
    -> Some(add_subs [Cst(cv_name m,i)] subs)
  | i -> Some(subs)

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

  let rec expl = function
    | Pld(r,ao,s) -> Pld(conv_reg r,expl_ao ao,s)
    | Pst(ao,ri,s) -> Pst(expl_ao ao,expl_ri ri,s)
    | Prmw(op,s) -> Prmw(expl_op op,s)
    | Pbranch _ -> failwith "TODO"
    | i -> i
  and expl_ao = function
    | Addr_op_atom ra -> Addr_op_atom(expl_ra ra)
    | Addr_op_add(ra,ri) -> Addr_op_add(expl_ra ra, expl_ri ri)
  and expl_ri = function
    | Regi r -> Regi(conv_reg r)
    | Imm(MetaConst.Meta v) -> Imm(find_cst v)
    | i -> i
  and expl_op = function
    | Add(r,iar1,iar2) -> Add(conv_reg r,expl_iar iar1,expl_iar iar2)
    | And(r,iar1,iar2) -> And(conv_reg r,expl_iar iar1,expl_iar iar2)
    | Mov(r,iar) -> Mov(conv_reg r,expl_iar iar)
  and expl_ra = function
    | Rega r -> Rega(conv_reg r)
    | abs -> abs
  and expl_iar = function
    | IAR_roa ra -> IAR_roa(expl_ra ra)
    | IAR_imm(MetaConst.Meta v) -> IAR_imm(find_cst v)
    | i -> i
   
in map_pseudos expl instrs
