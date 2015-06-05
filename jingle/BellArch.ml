include BellBase

type substitution = reg * reg

let rec match_instruction subs instr pattern = match instr,pattern with
  | Pld(r,ao,_),Pld(r',ao',_) -> 
     begin
       match match_addr_op subs ao ao' with
       | Some subs -> Some (add_subs [(r,r')] subs)
       | None -> None
     end

  | Pst(ao,ri,_),Pst(ao',ri',_) ->
     begin
       match match_addr_op subs ao ao' with
       | Some subs -> match_reg_or_imm subs ri ri'
       | None -> None
     end

  | Prmw(op,_),Prmw(op',_) ->
     match_op subs op op'

  | Pfence _, Pfence _ 
    -> Some subs

  | Pbranch(_,_,_), Pbranch(_,_,_) 
    -> Some subs

  | _,_ -> None

and match_op subs op op' = match op,op' with
  | Add(r,iar1,iar2),Add(r',iar1',iar2') 
  | And(r,iar1,iar2),And(r',iar1',iar2') ->
     begin match (match_iar subs iar1 iar1') with
	   | Some subs -> 
	      begin match (match_iar subs iar2 iar2') with
		    | Some subs -> Some(add_subs [r,r'] subs)
		    | None -> None
	      end
	   | None -> None
     end
  | Mov(r,iar),Mov(r',iar') ->
     begin match (match_iar subs iar iar') with
	   | Some subs -> Some(add_subs [r,r'] subs)
	   | None -> None
     end
  | _,_ -> None
       
and match_iar subs iar iar' = match iar,iar' with
  | IAR_roa ra,IAR_roa ra' 
    -> match_reg_or_addr subs ra ra'
  | IAR_imm _, IAR_imm _ 
    -> Some subs
  | _,_ -> None

and match_reg_or_addr subs ra ra' = match ra,ra' with
  | Rega r,Rega r' -> Some(add_subs [r,r'] subs)
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
    -> Some(add_subs [r,r'] subs)
  | _ -> Some subs

and add_subs s s' = match s with
  | [] -> s'
  | (l,r)::ss -> 
     try 
       if (reg_compare (List.assoc l s') r) = 0
       then add_subs ss s'
       else assert false (* test coherency *)
     with Not_found -> add_subs ss ((l,r)::s')

