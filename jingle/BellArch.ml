include BellBase

exception Error of string

type mcst = MetaConst.k

type substitution = 
  | Reg of string * reg
  | Cst of string * int
  | Lab of string * string
  | Addr of string * string

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

let rec annots_compare s s' = 
  match s,s' with
    | [],[] -> true
    | x::s,y::s' when String.compare x y = 0 -> annots_compare s s'
    | _ -> false

let rec add_subs s s' = match s with
  | [] -> s'
  | s::ss -> 
     if List.mem s s'
     then add_subs ss s'
     else add_subs ss (s::s')

let match_reg_or_imm subs ri ri' = match ri,ri' with
  | Regi r,Regi r' 
    -> Some(add_subs [Reg(sr_name r,r')] subs)
  | Imm (MetaConst.Meta m),Imm i
    -> Some(add_subs [Cst(m,i)] subs)
  | i -> Some(subs)

let match_reg_or_addr subs ra ra' = match ra,ra' with
  | Rega r,Rega r' -> Some(add_subs [Reg(sr_name r,r')] subs)
  | Abs x,Abs y -> 
     let s = SymbConstant.pp false x in
     let regn = SymbConstant.pp false y in
     Some(add_subs [Addr(s,regn)] subs)
  | _,_ -> None

let match_iar subs iar iar' = match iar,iar' with
  | IAR_roa ra,IAR_roa ra' 
    -> match_reg_or_addr subs ra ra'
  | IAR_imm (MetaConst.Meta m), IAR_imm i 
    -> Some(add_subs [Cst(m,i)] subs)
  | _,_ -> None

let match_op subs op op' = match op,op' with
  | Add(iar1,iar2),Add(iar1',iar2') 
  | And(iar1,iar2),And(iar1',iar2')
  | Xor(iar1,iar2),Xor(iar1',iar2') 
  | Eq(iar1,iar2),Eq(iar1',iar2') 
  | Neq(iar1,iar2),Neq(iar1',iar2') ->
     begin match (match_iar subs iar1 iar1') with
	   | Some subs -> match_iar subs iar2 iar2'
	   | None -> None
     end
  | RAI(iar),RAI(iar') -> match_iar subs iar iar'
  | _,_ -> None

let match_addr_op subs ao ao' = match ao,ao' with
  | Addr_op_atom ra,Addr_op_atom ra'
    -> match_reg_or_addr subs ra ra'
  | Addr_op_add(ra,ri),Addr_op_add(ra',ri') ->
     begin
       match match_reg_or_addr subs ra ra' with
       | Some subs -> match_reg_or_imm subs ri ri'
       | None -> None
     end
  | _,_ -> None

let match_instr subs pattern instr = match pattern,instr with
  | Pld(r,ao,s),Pld(r',ao',s') -> 
     if annots_compare s s'
     then match match_addr_op subs ao ao' with
	  | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
	  | None -> None
     else None

  | Pst(ao,ri,s),Pst(ao',ri',s') ->
    if annots_compare s s'
    then match match_addr_op subs ao ao' with
	 | Some subs -> match_reg_or_imm subs ri ri'
	 | None -> None
    else None

  | Prmw(r,op,ao,s),Prmw(r',op',ao',s') ->
     if annots_compare s s'
     then match match_op subs op op' with
	  | None -> None
	  | Some subs ->
	     match match_addr_op subs ao ao' with
	     | Some subs -> Some (add_subs [Reg(sr_name r,r')] subs)
	     | None -> None
     else None

  | Pfence Fence(s,_), Pfence Fence(s',_) 
    -> if annots_compare s s' 
       then Some subs
       else None

  | Pbranch(_,lp,s), Pbranch(_,li,s') 
    -> if annots_compare s s'
       then Some(add_subs [Lab(lp,li)] subs)
       else None

  | _,_ -> None

let rec match_instruction subs pattern instr= match pattern,instr with
  | Label(lp,insp),Label(li,insi) 
    -> match_instruction (add_subs [Lab(lp,li)] subs) insp insi
  | Label _, _ -> None
  | pattern, Label(_,instr)
    -> match_instruction subs pattern instr
  | Instruction ip, Instruction ii 
    -> match_instr subs ip ii
  | _,_ -> assert false
       
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
      | [] -> raise (Error("No binding for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 ->
	 MetaConst.Int i
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

  let rec expl = function
    | Pld(r,ao,s) -> Pld(conv_reg r,expl_ao ao,s)
    | Pst(ao,ri,s) -> Pst(expl_ao ao,expl_ri ri,s)
    | Prmw(r,op,ao,s) -> Prmw(conv_reg r,expl_op op,expl_ao ao,s)
    | Pbranch(a,l,b) -> Pbranch(a,find_lab l,b)
    | i -> i
  and expl_ao = function
    | Addr_op_atom ra -> Addr_op_atom(expl_ra ra)
    | Addr_op_add(ra,ri) -> Addr_op_add(expl_ra ra, expl_ri ri)
  and expl_ri = function
    | Regi r -> Regi(conv_reg r)
    | Imm(MetaConst.Meta v) -> Imm(find_cst v)
    | i -> i
  and expl_op = function
    | Add(iar1,iar2) -> Add(expl_iar iar1,expl_iar iar2)
    | And(iar1,iar2) -> And(expl_iar iar1,expl_iar iar2)
    | Xor(iar1,iar2) -> Xor(expl_iar iar1,expl_iar iar2)
    | Eq(iar1,iar2) -> Eq(expl_iar iar1,expl_iar iar2)
    | Neq(iar1,iar2) -> Neq(expl_iar iar1,expl_iar iar2)
    | RAI(iar) -> RAI(expl_iar iar)
  and expl_ra = function
    | Rega r -> Rega(conv_reg r)
    | abs -> abs
  and expl_iar = function
    | IAR_roa ra -> IAR_roa(expl_ra ra)
    | IAR_imm(MetaConst.Meta v) -> IAR_imm(find_cst v)
    | i -> i
   
in try map_pseudos expl instrs with
   | e -> Printf.eprintf "Error on translated instructions :%s\n" "";
	  raise e
