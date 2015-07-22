open Printf

let arch = Archs.c

type reg = string

let parse_reg s = Some s
let pp_reg r = r
let reg_compare = String.compare

type loc = 
  | Reg of reg
  | Mem of reg

let loc_compare l1 l2 = match l1,l2 with
  | Reg s1,Reg s2 -> reg_compare s1 s2
  | Mem s1,Mem s2 -> reg_compare s1 s2
  | Reg _,Mem _ -> -1
  | Mem _,Reg _ -> 1

let dump_loc = function
  | Reg r -> r
  | Mem r -> "*"^r

type mem_order = MemOrder.t 

type barrier = F of MemOrder.t

let pp_barrier (F(m)) = "Fence("^(MemOrder.pp_mem_order m)^")"
let barrier_compare = Pervasives.compare

type expression = 
  | Const of SymbConstant.v
  | Load of loc * mem_order option
  | Op of Op.op * expression * expression

type instruction = 
  | Fence of barrier
  | Seq of instruction list
  | If of expression * instruction * instruction option
  | Store of loc * expression * mem_order option
  | Fetch of loc * Op.op * expression * mem_order
  | Exchange of loc * expression * mem_order
  | Lock of loc
  | Unlock of loc
  | Symb of string

type parsedInstruction = instruction

let rec dump_instruction = 
  let dump_op =
    let open Op in
    function
    | Add -> "add"
    | Sub -> "sub"
    | Or -> "or"
    | Xor -> "xor"
    | And -> "and"
    | _ -> assert false in
  let rec dump_expr = function
    | Const c -> SymbConstant.pp_v c
    | Load(l,None) -> dump_loc l
    | Load(l,Some mo) ->
       sprintf "atomic_load_explicit(%s,%s)"
	       (dump_loc l) (MemOrder.pp_mem_order mo)
    | Op(op,e1,e2) -> 
       sprintf "%s %s %s" (dump_expr e1) (Op.pp_op op) (dump_expr e2)
  in function
  | Fence b -> (pp_barrier b)^";\n"
  | Seq l -> 
     let seq = List.fold_left (fun a i -> a^(dump_instruction i)^"\n") 
			      "" l in
     if List.length l < 2
     then seq
     else "{\n"^seq^"}\n"
  | If(c,t,e) ->
     let els =  match e with
       | None -> ""
       | Some e -> dump_instruction e
     in "if("^dump_expr c^") "^(dump_instruction t)^"else "^els
  | Store(l,e,None) -> 
     sprintf "%s = %s;" (dump_loc l) (dump_expr e)
  | Store(l,e,Some mo) -> 
     sprintf "atomic_load_explicit(%s,%s,%s);"
	     (dump_loc l) (dump_expr e) (MemOrder.pp_mem_order mo)
  | Fetch(l,op,e,mo) -> 
     sprintf "atomic_fetch_%s_explicit(%s,%s,%s);" 
	     (dump_op op) (dump_loc l) (dump_expr e) 
	     (MemOrder.pp_mem_order mo)
  | Exchange(l,e,mo) ->
     sprintf "atomic_exchange_explicit(%s,%s,%s);"
	     (dump_loc l) (dump_expr e) (MemOrder.pp_mem_order mo)
  | Lock l -> 
     sprintf "lock(%s);" (dump_loc l) 
  | Unlock l -> 
     sprintf "unlock(%s);" (dump_loc l)
  | Symb s -> sprintf "codevar:%s;" s


let pp_instruction _mode = dump_instruction 

let allowed_for_symb = List.map (fun x -> "r"^(string_of_int x)) 
				(Misc.interval 0 64)

let fold_regs (_fc,_fs) acc _ins = acc
let map_regs _fc _fs ins = ins
let fold_addrs _f acc _ins = acc
let map_addrs _f ins = ins
let norm_ins ins = ins
let get_next _ins = Warn.fatal "C get_next not implemented"

include Pseudo.Make
	  (struct
	    type ins = instruction
	    type pins = parsedInstruction
	    type reg_arg = reg

	    let rec parsed_expr_tr = function
	      | Const(Constant.Concrete _) as k -> k 
	      | Const(Constant.Symbolic _) -> 
		 Warn.fatal "No constant variable allowed"
	      | Load _ as l -> l
	      | Op(op,e1,e2) -> Op(op,parsed_expr_tr e1,parsed_expr_tr e2)

	    and parsed_tr = function
	      | Fence _ as f -> f
	      | Seq(li) -> Seq(List.map parsed_tr li)
	      | If(e,it,ie) -> 
		 let tr_ie = match ie with
		   | None -> None
		   | Some ie -> Some(parsed_tr ie) in
		 If(parsed_expr_tr e,parsed_tr it,tr_ie)
	      | Store(l,e,mo) -> Store(l,parsed_expr_tr e,mo)
	      | Fetch(l,op,e,mo) -> Fetch(l,op,parsed_expr_tr e,mo)
	      | Exchange(l,e,mo) -> Exchange(l,parsed_expr_tr e,mo)
	      | Lock _ | Unlock _ as i -> i
	      | Symb _ -> Warn.fatal "No term variable allowed"

	    let get_naccesses _ins = 0
	    let fold_labels acc _f _ins = acc
	    let map_labels _f ins = ins
	  end)

let get_macro _s = assert false
