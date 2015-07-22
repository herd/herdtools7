include CBase

exception Error of string

type substitution = 
  | Reg of string * reg
  | Cst of string * int
  | Lab of string * string
  | Addr of string * string
  | Code of string * pseudo list

let rec wrap_pseudo = function 
  | [] -> []
  | i::is -> (Instruction i)::(wrap_pseudo is)

let rec unwrap_pseudo = function 
  | [] -> []
  | (Instruction i)::is -> i::(unwrap_pseudo is)
  | (Label(_,p))::is -> (unwrap_pseudo [p])@(unwrap_pseudo is)
  | Nop:: is -> unwrap_pseudo is
  | _ -> assert false

let rec dump_pseudos = function
    | [] -> ""
    | Nop::is -> "*Nop*\n" ^dump_pseudos is
    | Label(s,i)::is -> s^": "^(dump_pseudos (i::is))
    | Instruction i::is -> dump_instruction i ^" ;\n"^
			     (dump_pseudos is)
    | _ -> assert false

let rec add_subs s s' : substitution list = match s with
  | [] -> s'
  | s::ss -> 
     if List.mem s s'
     then add_subs ss s'
     else add_subs ss (s::s')

let match_location subs pat instr = match pat,instr with
  | CBase.Reg s,CBase.Reg s'
  | Mem s,Mem s' when String.compare s s' = 0 ->
     Some(add_subs [Reg(s,s')] subs)
  | _ -> None

let rec match_expr subs pat instr = match pat,instr with
  | Const(Constant.Symbolic s),Const(Constant.Concrete c) ->
     Some(add_subs [Cst(s, c)] subs)
  | Const(Constant.Concrete s),Const(Constant.Concrete c) 
       when c=s ->
     Some subs
  | Load(l,mo),Load(l',mo') when mo=mo' ->
     match_location subs l l'
  | Op(op,ex1,ex2),Op(op',ex1',ex2') when op=op' ->
     begin match match_expr subs ex1 ex1' with
     | None -> None
     | Some subs ->
	match_expr subs ex2 ex2'
     end
  | _ -> None

let rec match_instr subs pattern instr = match pattern,instr with
  | Fence b,Fence b' when barrier_compare b b' = 0 ->
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
  | Store(l,ex,mo),Store(l',ex',mo') when mo=mo' ->
     begin match match_location subs l l' with
     | None -> None
     | Some subs -> match_expr subs ex ex'
     end
  | Fetch(l,op,ex,mo),Fetch(l',op',ex',mo') when mo=mo' && op=op' ->
     begin match match_location subs l l' with
     | None -> None
     | Some subs -> match_expr subs ex ex'
     end
  | Exchange(l,ex,mo),Exchange(l',ex',mo') when mo=mo' ->
     begin match match_location subs l l' with
     | None -> None
     | Some subs -> match_expr subs ex ex'
     end
  | Lock l,Lock l' -> match_location subs l l'
  | Unlock l,Unlock l' -> match_location subs l l'
  | Symb s,Seq l -> 
     Some(add_subs [Code(s,wrap_pseudo l)] subs)
  | Symb s,ins -> 
     Some(add_subs [Code(s,wrap_pseudo [ins])] subs)
  | _ -> None
  
let rec match_instruction subs pattern instr = match pattern,instr with
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
    | Label (_,ins) -> aux ins
    | Symbolic _
    | Macro (_,_) -> assert false
  in function
  | [] -> []
  | i::is -> (aux i)::(map_pseudos f is)

let instanciate_with subs _ instrs = 
  let get_register =
    let env,i = ref [],ref 0 in
    fun s -> try List.assoc s !env with
       | Not_found ->
	  let r = "tmp"^(string_of_int !i) in
	  env := (s,r)::!env;
	  incr i;
	  r in
  let find_cst s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 -> Constant.Concrete i
      | _::subs -> aux subs
    in aux subs in
  let find_code s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for code "^s))
      | Code(n,c)::_ when String.compare n s = 0 ->
	 Seq(unwrap_pseudo c)
      | _::subs -> aux subs
    in aux subs in
  let conv_reg s = 
    let rec aux = function
      | [] -> get_register s
      | Addr(n,r)::_ when String.compare n s = 0 -> get_register r
      | Reg(n,r)::_ when String.compare n s = 0 -> r
      | _::subs -> aux subs
    in aux subs in

  let rec expl =
    let expl_loc = function
      | CBase.Reg s -> CBase.Reg(conv_reg s)
      | Mem s -> Mem(conv_reg s)
    in
    let rec expl_expr = function
      | Const(Constant.Symbolic s) -> Const(find_cst s)
      | Load(l,mo) -> Load(expl_loc l,mo)
      | Op(op,e1,e2) -> Op(op,expl_expr e1,expl_expr e2)
      | x -> x 
    in
    function
    | Fence b -> Fence b
    | Seq l -> Seq(List.map expl l)
    | If(c,t,e) -> 
       let e = match e with 
	 | None -> None
	 | Some e -> Some(expl e)
       in
       If(expl_expr c,expl t,e)
    | Store(l,e,mo) -> Store(expl_loc l, expl_expr e,mo)
    | Fetch(l,op,e,mo) -> Fetch(expl_loc l,op,expl_expr e,mo)
    | Exchange(l,e,mo) -> Exchange(expl_loc l, expl_expr e,mo)
    | Lock l -> Lock(expl_loc l)
    | Unlock l -> Unlock(expl_loc l)
    | Symb s -> find_code s
  in
  map_pseudos expl instrs
