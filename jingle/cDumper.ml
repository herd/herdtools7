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
open CBase
open Printf
     
type pseudo = CBase.pseudo

let dump_loc = MiscParser.dump_location
		 
let dump_state_atom a =
  MiscParser.dump_state_atom dump_loc SymbConstant.pp_v a
			     
type state = MiscParser.state
	       
let dump_state st =
  String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom a))
                   st)
		
                
type constr = MiscParser.constr
let dump_atom a =
  let open ConstrGen in
  match a with
  | LV (loc,v) -> dump_state_atom (loc,(MiscParser.TyDef,v))
  | LL (loc1,loc2) ->
     sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)
	     
let dump_constr = ConstrGen.constraints_to_string dump_atom
						  
type location = MiscParser.location
let dump_location = dump_loc


let rec fmt_io io = match io with
  | Nop -> ""
  | Instruction ins -> dump_instruction ins
  | Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | Symbolic s -> "codevar:"^s
  | Macro (f,regs) ->
     sprintf
       "%s(%s)"
       f
        (String.concat "," (List.map pp_reg regs))

let rec unwrap_pseudo = function 
  | [] -> []
  | (Instruction i)::is -> i::(unwrap_pseudo is)
  | (Label(_,p))::is -> (unwrap_pseudo [p])@(unwrap_pseudo is)
  | Nop:: is -> unwrap_pseudo is
  | _::is -> unwrap_pseudo is

let list_loc prog = 
  let module LocSet = 
    Set.Make(struct 
	      type t = loc
	      let compare = loc_compare
	    end) in

  let rec expr s = function
    | Const _ -> s
    | Load(l,_) -> LocSet.add l s
    | Op(_,e1,e2) -> expr (expr s e1) e2
    | Exchange(l,e,_) -> LocSet.add l (expr s e)
  in 
  let rec ins s = function
    | Seq(l) -> List.fold_left ins s l
    | If(c,t,Some e) -> expr (ins (ins s e) t) c
    | If(c,t,None) -> expr (ins s t) c
    | Store(l,e,_) -> LocSet.add l (expr s e)
    | Fetch(l,_,e,_) -> LocSet.add l (expr s e)
    | Lock l -> LocSet.add l s
    | Unlock l -> LocSet.add l s
    | _ -> s
  in
  LocSet.elements (List.fold_left ins LocSet.empty prog)

let get_params init i = 
  List.fold_left 
    (fun a -> 
     function
     | (MiscParser.Location_reg(p,_),
	(_,Constant.Symbolic s)) when i = p ->
	{ CAst.param_ty = CType.(Volatile (Base "int"));
	  CAst.param_name = s }::a
     | _ -> a
    ) [] init

let extract_decl init i prog = 
  let rec find_v s = function
    | [] -> None
    | (MiscParser.Location_reg(n,r),(_,v))::_ 
	 when String.compare s r = 0 
	      && n = i ->
       Some (SymbConstant.pp_v v)
    | _::init -> find_v s init in
  let to_decl = function
    | Reg s -> 
       let aff = match find_v s init with
		| None -> ";"
		| Some s -> " = "^s^";"
       in sprintf "int %s%s" s aff
    | Mem s -> 
       let aff = match find_v s init with
		| None -> ";"
		| Some s -> " = "^s^";"
       in sprintf "int* %s%s" s aff
  in List.map to_decl (list_loc prog)

let code init prog = 
  let open CAst in
  List.map (fun (i,p) ->
	    let params = get_params init i in
	    let decls =  extract_decl init i (unwrap_pseudo p)
	    in Test { proc = i;
		   params = params;
		   body = String.concat "\n" 
			   (decls@(List.map fmt_io p))
	    })
	   prog

let prog = DumpCAst.print_prog
       
       
let do_dump withinfo chan doc t =
  fprintf chan "%s %s\n" (Archs.pp arch) doc.Name.name ;
  begin match doc.Name.doc with
	| "" -> ()
	| doc -> fprintf chan "\"%s\"\n" doc
  end ;
  if withinfo then begin
	List.iter
        (fun (k,i) -> fprintf chan "%s=%s\n" k i)
        t.MiscParser.info
    end ;
  fprintf chan "\n{%s}\n\n" (dump_state  t.MiscParser.init) ;
  prog chan (code t.MiscParser.init t.MiscParser.prog) ;
  let locs = DumpUtils.dump_locations 
	       dump_location t.MiscParser.locations in
  if locs <> "" then fprintf chan "%s\n" locs ;
  begin match t.MiscParser.extra_data with
	| MiscParser.NoExtra|MiscParser.CExtra _ -> ()
	| MiscParser.BellExtra bi ->
           fprintf chan "\n" ;
           BellInfo.pp chan bi ;
           fprintf chan "\n"
  end ;
  fprintf chan "%s\n" (dump_constr t.MiscParser.condition) ;
  ()
    
let dump = do_dump false
let dump_info = do_dump true
			
let (@@) f k = f k
(*		 
let lines doc t =
  begin fun k -> sprintf "%s %s" (Archs.pp arch) doc.Name.name :: k
  end @@
  begin fun k -> match doc.Name.doc with
		   | "" -> k
		   | doc -> sprintf "\"%s\"" doc :: k
  end @@
  begin fun k ->  sprintf "{%s}" (dump_state  t.MiscParser.init) :: k
  end @@
  begin
    fun k ->
    let pp = List.map fmt_col t.MiscParser.prog in
    let pp = Misc.lines_of_prog pp in
    let pp = List.map (sprintf "%s;") pp in
    pp @ ""::k
  end @@
  begin fun k ->
	match t.MiscParser.locations with
		| [] -> k
		| locs ->
		   DumpUtils.dump_locations dump_location locs::k
  end @@
    [dump_constr t.MiscParser.condition]

 *)
