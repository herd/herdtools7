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

module type Parser = sig
  include GenParser.S
	    
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
  end

module type Dumper = sig
    type pseudo
    val dump_info : 
      out_channel -> Name.t ->
      (MiscParser.state, (int * pseudo list) list,
       MiscParser.prop, MiscParser.location)
        MiscParser.result
      -> unit
end
  
module type Common = sig
    
  include ArchBase.S
    
  exception Error of string

  type substitution =
    | Reg of string * reg
    | Cst of string * int
    | Lab of string * string
    | Addr of string * string
    | Code of string * pseudo list

  val pp_subs :  substitution list -> string

  val add_subs : substitution list -> substitution list ->
                 substitution list
  val sr_name : reg -> string
  val cv_name : MetaConst.k -> string
  val dump_pseudos : pseudo list -> string
  val conv_reg : substitution list -> reg list ref -> (string * reg) list ref ->
                 reg -> reg
  val find_lab : substitution list -> reg list ref -> (string * string) list ref ->
                 string -> string
  val find_code : substitution list -> reg list ref ->
                  string -> pseudo list
  val find_cst : substitution list -> reg list ref ->
                 string -> MetaConst.k
    
end
  
module type S = sig
    include Common

    val match_instruction : substitution list -> 
			    parsedPseudo -> pseudo ->
			    substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   pseudo list
   
    module Parser : Parser with type parsedPseudo = parsedPseudo
			   and type pseudo = pseudo

    module Dumper : Dumper with type pseudo = pseudo
  end

module MakeCommon(A:ArchBase.S) = struct

  let debug = false

  include A
    
  exception Error of string

  type substitution =
    | Reg of string * reg
    | Cst of string * int
    | Lab of string * string
    | Addr of string * string
    | Code of string * pseudo list


  let pp_sub = function
    | Reg (s,r) -> sprintf "Reg (%s,%s)" s (pp_reg r)
    | Cst (s,i) -> sprintf "Cst (%s,%i)" s i
    | Lab (l1,l2) -> sprintf "Lab (%s,%s)" l1 l2
    | Addr (s,r) ->  sprintf "Addr (%s,%s)" s r
    | Code (s,_) -> sprintf "Code (%s,...)" s

  let pp_subs subs = String.concat ";" (List.map pp_sub subs)

  let rec add_subs s s' = match s with
    | [] -> s'
    | s::ss -> 
       if List.mem s s'
       then add_subs ss s'
       else add_subs ss (s::s')
	 
  let sr_name r = match symb_reg_name r with
    | Some s -> s
    | None -> raise (Error "Not a symbolic register.")
     
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

  let conv_reg subs free env r =
    let get_register =
      fun s -> try List.assoc s !env with
      | Not_found ->
	 let r = List.hd !free in
	 env := (s,r)::!env;
	 free := List.tl !free;
	 r in
    let res = match symb_reg_name r with
    | Some s ->
        let rec aux = function
	  | [] -> get_register s
	  | Reg(n,r)::_ when String.compare n s = 0 -> r
	  | Addr(n,r)::_ when String.compare n s = 0 -> get_register r
	  | _::subs -> aux subs
        in aux subs
    | None -> r in
    if debug then
      eprintf "conv_reg subs=<%s> %s -> %s\n"
        (pp_subs subs) (pp_reg r) (pp_reg res) ;
    res


  let fresh_lbl =
    let i = ref 0 in
    fun () -> incr i;"lbl"^(string_of_int !i)
       
  let find_lab subs _ label_env l =
    let get_label = 
      fun s -> try List.assoc s !label_env with
      | Not_found ->
	 let l = fresh_lbl () in
	 label_env := (s,l)::!label_env;
	 l in
    let rec aux = function
      | [] -> get_label l
      | Lab(n,lbl)::_ when String.compare n l = 0 -> lbl
      | _::subs -> aux subs
    in aux subs

  let find_code subs _ s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for code "^s))
      | Code(n,c)::_ when String.compare n s = 0 -> c
      | _::subs -> aux subs
    in aux subs 

  let find_cst subs _ s =
    let rec aux = function
      | [] -> raise (Error("No conversion found for constant "^s))
      | Cst(n,i)::_ when String.compare n s = 0 -> MetaConst.Int i
      | _::subs -> aux subs
    in aux subs
    
end
  
module MakeParser
	 (A:ArchBase.S)
	 (P:sig
	      include GenParser.LexParse
		      with type instruction = A.parsedPseudo
	      val instr_parser : 
		(Lexing.lexbuf -> token) -> Lexing.lexbuf ->
		A.parsedPseudo list
	    end) = struct
  include GenParser.Make(GenParser.DefaultConfig)(A)(P)
			       
  type parsedPseudo = A.parsedPseudo
  let instr_from_string s =
    GenParser.call_parser "themes" (Lexing.from_string s) 
			  P.lexer P.instr_parser
		
end

module MakeArch(I:sig
  include Common
  val match_instr : substitution list ->
                    parsedInstruction ->
                    instruction ->
                    substitution list option
  val expl_instr : substitution list -> reg list ref -> (string * string) list ref ->
                   (string * reg) list ref ->
                   parsedInstruction ->
                   parsedInstruction
end) = struct
  include I
    
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
    let label_env = ref [] in
    let reg_env = ref [] in
    let expl_instr = expl_instr subs (ref free) label_env reg_env in
    let find_lab = find_lab subs (ref free) label_env in
    let find_code = find_code subs (ref free) in
    let rec expl_pseudos = 
      let rec aux = function
	| Nop -> []
	| Instruction ins ->
	   [pseudo_parsed_tr (Instruction (expl_instr ins))]
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
    
end 
  
module DefaultDumper(A:ArchBase.S) = struct 
  type pseudo = A.pseudo
  include SimpleDumper.Make(SimpleDumper.OutChannel)
      (struct
	module A = A

        let dump_loc = MiscParser.dump_location

        let dump_state_atom a =
          MiscParser.dump_state_atom dump_loc ParsedConstant.pp_v a

        type state = MiscParser.state

        let dump_state st =
          String.concat " "
            (List.map
               (fun a -> sprintf "%s;" (dump_state_atom a))
               st)

            
        type prop = MiscParser.prop

        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (loc,v) -> dump_state_atom (loc,(MiscParser.TyDef,v))
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = MiscParser.location
        let dump_location = dump_loc
      end)
end
