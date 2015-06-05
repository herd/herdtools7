include AArch64Base

type substitution = reg * reg

let rec match_instruction subs instr pattern = match instr,pattern with
  | I_FENCE _,I_FENCE _
  | I_B _, I_B _
  | I_BC _,I_BC _
    -> Some subs

  | I_CBZ(_,r,_),I_CBZ(_,r',_)
  | I_CBNZ(_,r,_),I_CBNZ(_,r',_)
  | I_MOV(_,r,_),I_MOV(_,r',_)
    -> Some(add_subs [(r,r')] subs)

  | I_LDAR(_,_,r1,r2),I_LDAR(_,_,r1',r2')
  | I_STLR(_,r1,r2),I_STLR(_,r1',r2')
  | I_SXTW(r1,r2),I_SXTW(r1',r2')
    -> Some(add_subs [(r1,r1');(r2,r2')] subs)

  | I_STXR(_,_,r1,r2,r3),I_STXR(_,_,r1',r2',r3') 
    -> Some(add_subs [(r1,r1');(r2,r2');(r3,r3')] subs)
     
  | I_LDR(_,r1,r2,kr),I_LDR(_,r1',r2',kr')
  | I_STR(_,r1,r2,kr),I_STR(_,r1',r2',kr')
  | I_OP3(_,_,r1,r2,kr),I_OP3(_,_,r1',r2',kr')
    -> begin match match_kr subs kr kr' with 
	     | Some subs -> Some(add_subs [(r1,r1');(r2,r2')] subs)
	     | None -> None
       end

  | _,_ -> None

and match_kr subs kr kr' = match kr,kr' with
  | K x,K y when x=y -> Some subs
  | RV(_,r),RV(_,r') -> Some(add_subs [(r,r')] subs)
  | _ -> None

and add_subs s s' = match s with
  | [] -> s'
  | (l,r)::ss -> 
     try 
       if (reg_compare (List.assoc l s') r) = 0
       then add_subs ss s'
       else assert false (* test coherency *)
     with Not_found -> add_subs ss ((l,r)::s')
