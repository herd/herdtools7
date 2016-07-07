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

let string_of_annot = MemOrderOrAnnot.pp_annot

let arch = Archs.c

type reg = string

let parse_reg s = Some s
let pp_reg r = r
let reg_compare = String.compare
let symb_reg_name r = Some r

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

type barrier = MemOrderOrAnnot.t

let pp_barrier m =
  let open MemOrderOrAnnot in
  match m with
  | MO mo -> "Fence("^(MemOrder.pp_mem_order mo)^")"
  | AN a -> "Fence{"^string_of_annot a^"}"

let barrier_compare = Pervasives.compare

type expression = 
  | Const of SymbConstant.v
  | Op of Op.op * expression * expression
  | Exchange of loc * expression * MemOrderOrAnnot.t
  | Fetch of loc * Op.op * expression * mem_order
  | ECall of string * expression list
  | Load of loc * MemOrderOrAnnot.t

type instruction = 
  | Fence of barrier
  | Seq of instruction list
  | If of expression * instruction * instruction option
  | Store of loc * expression * MemOrderOrAnnot.t
  | Lock of loc
  | Unlock of loc
  | Symb of string
  | PCall of string * expression list

type parsedInstruction = instruction

let dump_op =
  let open Op in
  function
    | Add -> "add"
    | Sub -> "sub"
    | Or -> "or"
    | Xor -> "xor"
    | And -> "and"
    | _ -> assert false


let rec dump_expr =
  let open MemOrderOrAnnot in
  function
    | Const c -> SymbConstant.pp_v c
    | Load(l,AN []) -> dump_loc l
    | Load(l,AN a) ->
        sprintf "__load{%s}(%s)" (string_of_annot a) (dump_loc l)
    | Load(l,MO mo) ->
        sprintf "atomic_load_explicit(%s,%s)"
	  (dump_loc l) (MemOrder.pp_mem_order mo)
    | Op(op,e1,e2) -> 
        sprintf "%s %s %s" (dump_expr e1) (Op.pp_op op) (dump_expr e2)
    | Exchange(l,e,MO mo) ->
        sprintf "atomic_exchange_explicit(%s,%s,%s)"
	  (dump_loc l) (dump_expr e) (MemOrder.pp_mem_order mo)
    | Exchange(l,e,AN a) ->
        sprintf "__xchg{%s}(%s,%s)"
	  (string_of_annot a) (dump_loc l) (dump_expr e)
    | Fetch(l,op,e,mo) -> 
        sprintf "atomic_fetch_%s_explicit(%s,%s,%s);" 
	  (dump_op op) (dump_loc l) (dump_expr e) 
	  (MemOrder.pp_mem_order mo)
    | ECall(f,es) ->
        sprintf "%s(%s)" f (dump_args es)

and dump_args es = String.concat "," (List.map dump_expr es)


let rec dump_instruction = 
  let open MemOrderOrAnnot in
  function
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
  | Store(l,e,AN []) -> 
     sprintf "%s = %s;" (dump_loc l) (dump_expr e)
  | Store(l,e,AN a) ->
      sprintf "__store{%s}(%s,%s);"
        (string_of_annot a) (dump_loc l) (dump_expr e)
  | Store(l,e,MO mo) -> 
     sprintf "atomic_store_explicit(%s,%s,%s);"
	     (dump_loc l) (dump_expr e) (MemOrder.pp_mem_order mo)
  | Lock l -> 
     sprintf "lock(%s);" (dump_loc l) 
  | Unlock l -> 
     sprintf "unlock(%s);" (dump_loc l)
  | Symb s -> sprintf "codevar:%s;" s
  | PCall (f,es) ->
      sprintf "%s(%s);" f (dump_args es)

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
	      | Exchange(l,e,mo) -> Exchange(l,parsed_expr_tr e,mo)
	      | Fetch(l,op,e,mo) -> Fetch(l,op,parsed_expr_tr e,mo)
              | ECall (f,es) -> ECall (f,List.map parsed_expr_tr es)

	    and parsed_tr = function
	      | Fence _ as f -> f
	      | Seq(li) -> Seq(List.map parsed_tr li)
	      | If(e,it,ie) -> 
		 let tr_ie = match ie with
		   | None -> None
		   | Some ie -> Some(parsed_tr ie) in
		 If(parsed_expr_tr e,parsed_tr it,tr_ie)
	      | Store(l,e,mo) -> Store(l,parsed_expr_tr e,mo)
	      | Lock _ | Unlock _ as i -> i
	      | Symb _ -> Warn.fatal "No term variable allowed"
              | PCall (f,es) -> PCall (f,List.map parsed_expr_tr es)

	    let get_naccesses =
              let open MemOrderOrAnnot in
              let get_loc k = function
                | Mem _ -> k+1
                | Reg _ -> k in

              let rec get_exp k = function
                | Const _ -> k
                | Load (loc,AN _) -> get_loc k loc
                | Load (loc,MO _) -> get_loc (k+1) loc
                | Op (_,e1,e2) -> get_exp (get_exp k e1) e2
                | Fetch (_,_,e,_)
                | Exchange (_,e,_) -> get_exp (k+2) e
                | ECall (_,es) -> List.fold_left get_exp k es in

              let rec get_rec k = function
                | Fence _|Symb _-> k
                | Seq seq -> List.fold_left get_rec k seq
                | If (cond,ifso,ifno) ->
                    let k = get_exp k cond in
                    get_opt (get_rec k ifso) ifno
                | Store (loc,e,AN _) -> get_exp (get_loc k loc) e
                | Store (loc,e,MO _) -> get_exp (get_loc (k+1) loc) e
                | Lock _|Unlock _ -> k+1
                | PCall (_,es) ->  List.fold_left get_exp k es

              and get_opt k = function
                | None -> k
                | Some i -> get_rec k i in

              fun i -> get_rec 0 i


	    let fold_labels acc _f _ins = acc
	    let map_labels _f ins = ins
	  end)

let get_macro _s = assert false

(* C specific macros *)

type macro =
  | EDef of string * string list * expression
  | PDef of string * string list * instruction 

type env_macro =
  { expr : (string list * expression) StringMap.t ;
    proc : (string list * instruction) StringMap.t ;
    args : expression StringMap.t ; }

let env_empty =
  {
   expr = StringMap.empty;
   proc = StringMap.empty;
   args = StringMap.empty;
 }

let add m env =  match m with
| EDef (f,args,e) ->
    { env with expr = StringMap.add f (args,e) env.expr ; }
| PDef (f,args,body) ->
    { env with proc = StringMap.add f (args,body) env.proc ; }

open MemOrderOrAnnot

let find_macro f env =
  try StringMap.find f env with
  | Not_found ->
      Warn.user_error "Unknown macro %s" f

let rec build_frame f tr xs es = match xs,es with
| [],[] -> StringMap.empty
| x::xs,e::es -> StringMap.add x (tr e) (build_frame f tr xs es)
| _,_ -> Warn.user_error "Argument mismatch for macro %s" f


let subst_lvalue env loc = try match loc with
  | Reg r ->
      begin match StringMap.find r env.args with
      | Load (loc,AN []) -> loc
      | e ->
          Warn.user_error
            "Bad lvalue '%s' while substituting macro argument %s"
            (dump_expr e) r
      end
  | Mem r ->
      begin match StringMap.find r env.args with
      | Load (Reg r,AN []) -> Mem r
      | e ->
          Warn.user_error
            "Bad lvalue '%s' while substituting macro argument %s"
            (dump_expr e) r
      end
  with Not_found -> loc

        
let rec subst_expr env e = match e with
| Load (Reg r,AN []) ->
    begin try StringMap.find r env.args with Not_found -> e end
| Load (loc,mo) -> Load (subst_lvalue env loc,mo)
| Const _ -> e
| Op (op,e1,e2) -> Op (op,subst_expr env e1,subst_expr env e2)
| Exchange (loc,e,mo) ->  Exchange (subst_lvalue env loc,subst_expr env e,mo)
| Fetch (loc,op,e,mo) -> Fetch (subst_lvalue env loc,op,subst_expr env e,mo)
| ECall (f,es) ->
    let xs,e = find_macro f env.expr in
    let frame = build_frame f (subst_expr env) xs es in 
    subst_expr { env with args = frame; } e

    
let rec subst env i = match i with
| Fence _|Symb _ -> i
| Seq is -> Seq (List.map (subst env) is)
| If (c,ifso,None) ->
    If (subst_expr env c,subst env ifso,None)
| If (c,ifso,Some ifno) ->
    If (subst_expr env c,subst env ifso,Some (subst env ifno))
| Store (loc,e,mo) ->
    Store (subst_lvalue env loc,subst_expr env e,mo)
| Lock loc -> Lock (subst_lvalue env loc)
| Unlock loc -> Unlock (subst_lvalue env loc)
| PCall (f,es) ->
    let xs,body = find_macro f env.proc in
    let frame = build_frame f (subst_expr env) xs es in 
    subst { env with args = frame; } body

let expand ms = match ms with
| [] -> Misc.identity
| _  ->
    let env = List.fold_left (fun e m -> add m e) env_empty ms in
    pseudo_map (subst env)
