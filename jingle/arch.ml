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
      (MiscParser.state, (MiscParser.proc * pseudo list) list,
       MiscParser.prop, MiscParser.location,MiscParser.maybev,MiscParser.fault_type)
        MiscParser.result
      -> unit
end

module type Base = sig
  include ArchBase.S
  val dump_parsedInstruction : parsedInstruction -> string
end

module type Common = sig

  include Base

  type st =
      {
       free : reg list;
       reg_env : (string * reg) list;
       label_env : (string * string) list;
      }

  exception Error of string

  type substitution =
    | Reg of string * reg
    | Cst of string * int
    | Lab of string * string
    | Addr of string * string
    | Code of string * pseudo list

  val pp_subs :  substitution list -> string

  val add_subs : substitution list -> substitution list ->
                 substitution list option
  val sr_name : reg -> string
  val cv_name : MetaConst.k -> string
  val dump_pseudos : pseudo list -> string
  val debug_pseudos : pseudo list -> string
  val debug_pats : parsedInstruction kpseudo list -> string

  val conv_reg : substitution list -> reg -> st -> reg * st
  val find_lab : substitution list -> string -> st -> string * st
  val find_code : substitution list -> string -> st -> pseudo list * st
  val find_cst : substitution list -> MetaConst.k -> st ->  MetaConst.k * st
  val (>>>) : 'a option -> ('a -> 'b option) -> 'b option
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

module MakeCommon(A:Base) = struct

  let debug = false

  include A

  type st =
      {
       free : reg list;
       reg_env : (string * reg) list;
       label_env : (string * string) list;
     }

(* State monad *)
  let (>>) f g = fun st ->
    let r,st = f st in
    g r st

  let unitT r st = r,st

  let (>!) f g = fun st ->
    let r,st = f st in
    g r,st

  let (>!!) f g = fun st ->
    let rs,stt = List.split (List.map f st) in
    List.map g rs,stt

  let mapT f =
    let rec map_rec = function
      | [] -> unitT []
      | x::xs ->
          f x >> fun y -> map_rec xs >! fun ys -> y::ys in
    map_rec

  let optT f = function
    | None -> unitT None
    | Some x -> f x >! fun y -> Some y


(* Substitutions *)
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

  let same_key b1 b2 = match b1,b2 with
  | (Reg (s1,_),Reg (s2,_))
  | (Cst (s1,_),Cst (s2,_))
  | (Lab (s1,_),Lab (s2,_))
  | (Addr (s1,_),Addr (s2,_))
  | (Code (s1,_),Code (s2,_))
    -> Misc.string_eq s1 s2
  | _ -> false

  let same_val b1 b2 = match b1,b2 with
  | (Reg (_,r1),Reg (_,r2)) -> A.reg_compare r1 r2 = 0
  | (Cst (_,i1),Cst (_,i2)) -> Misc.int_eq i1 i2
  | (Lab (_,s1),Lab (_,s2))
  | (Addr (_,s1),Addr (_,s2))
       -> Misc.string_eq s1 s2
  | (Code (s,_),Code (_,_)) ->
      Warn.user_error "Code variable %s used non-linearily" s
  | _ -> false

  let rec find_prev b1 subs = match subs with
  | [] -> None
  | b2::subs ->
      if same_key b1 b2 then Some b2
      else find_prev b1 subs

  let add_subs subs1 subs2 =
    try
      let k =
        List.fold_right
          (fun b1 k -> match find_prev b1 k with
          | None -> b1::k
          | Some b2 ->
              if same_val b1 b2 then k
              else raise Exit)
          subs1 subs2 in
      Some k
    with Exit -> None

  let sr_name r = match symb_reg_name r with
    | Some s -> s
    | None -> raise (Error "Not a symbolic register.")

  let match_reg r r' subs =  match symb_reg_name r with
  | Some s -> add_subs [Reg (s,r')] subs
  | None -> if r = r' then Some subs else None

  let combine f x y g z t = match f x y with
  | None -> None
  | Some b -> match g z t with
    | None ->  None
    | Some c -> Some (b@c)

  let cv_name = function
    | MetaConst.Meta s -> s
    | _ -> raise (Error "Not a constant variable.")

  let rec dump_pseudo di = function
    | Nop -> "*Nop*"
    | Label (lab,p) -> sprintf "%s: %s" lab (dump_pseudo di p)
    | Instruction i -> di i
    | Symbolic v -> "codevar:"^v
    | Macro _ -> assert false
    | Pagealign | Skip _ -> assert false (* Not implemented yet *)

  let rec dump_pseudos = function
    | [] -> ""
    | p::is -> dump_pseudo dump_instruction p ^";\n"^dump_pseudos is

  let rec do_debug_pseudos di k ps = match ps with
  | [] -> []
  | p::ps ->
      if k <= 0 then ["..."]
      else dump_pseudo di p::do_debug_pseudos di (k-1) ps

  let debug_pseudos ps =
    String.concat "; " (do_debug_pseudos dump_instruction  3 ps)

  let debug_pats ps =
    String.concat "; " (do_debug_pseudos dump_parsedInstruction 3 ps)

  let alloc_reg st = match st.free with
  | r::free -> r,{ st with free=free; }
  | [] -> raise (Error "register free list exhausted")

  let conv_reg subs r st =
    let get_register =
      fun s -> try List.assoc s st.reg_env,st with
      | Not_found ->
          let r,st = alloc_reg st in
          r,{ st with reg_env = (s,r)::st.reg_env; } in

    let res = match symb_reg_name r with
    | Some s ->
        let rec aux = function
          | [] -> get_register s
          | Reg(n,r)::_ when Misc.string_eq n s -> r,st
          | Addr(n,r)::_ when Misc.string_eq n s -> get_register r
          | _::subs -> aux subs
        in aux subs
    | None -> r,st in
    if debug then begin
      let res,_ = res in
      eprintf "conv_reg subs=<%s> %s -> %s\n"
        (pp_subs subs) (pp_reg r) (pp_reg res)
    end ;
    res


  let fresh_lbl =
    let i = ref 0 in
    fun () -> incr i;"lbl"^(string_of_int !i)

  let find_lab subs lab st =

    let get_label =
      fun s -> try List.assoc s st.label_env,st with
      | Not_found ->
         let lab = fresh_lbl () in
         lab,{ st with label_env=(s,lab)::st.label_env; }  in

    let rec aux = function
      | [] -> get_label lab
      | Lab(n,lbl)::_ when Misc.string_eq n lab -> lbl,st
      | _::subs -> aux subs
    in aux subs

  let find_code subs s st =
    let rec aux = function
      | [] -> raise (Error("No conversion found for code "^s))
      | Code(n,c)::_ when Misc.string_eq n s -> c
      | _::subs -> aux subs
    in aux subs,st

  let find_cst subs k st = match k with
  | MetaConst.Meta s ->
      let rec aux = function
        | [] -> raise (Error("No conversion found for constant "^s))
        | Cst(n,i)::_ when  Misc.string_eq n s -> MetaConst.Int i
        | _::subs -> aux subs
      in aux subs,st
  | _ -> k,st

  let (>>>) v f = match v with
  | None -> None
  | Some v -> f v

end

module MakeParser
         (A:Base)
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
    GenParserUtils.call_parser "themes" (Lexing.from_string s)
                          P.lexer P.instr_parser

end

module MakeArch(I:sig
  include Common
  val match_instr : substitution list ->
                    parsedInstruction ->
                    instruction ->
                    substitution list option
  val expl_instr :
      substitution list -> parsedInstruction -> st -> parsedInstruction * st
end) = struct
  include I

  let rec match_instruction subs pattern instr = match pattern,instr with
    | Label(lp,insp),Label(li,insi)
      ->
        add_subs [Lab(lp,li)] subs >>> fun subs ->
        match_instruction subs insp insi
    | Label _, _ -> None
    | pattern, Label(_,instr)
      -> match_instruction subs pattern instr
    | Instruction ip, Instruction ii
      -> match_instr subs ip ii
    | Nop,Nop -> Some subs
    | _,_ -> assert false

  let instanciate_with subs free instrs =
    let expl_instr = expl_instr subs in
    let find_lab = find_lab subs in
    let find_code = find_code subs in

    let rec expl_pseudos =

      let rec aux p st = match p with
        | Nop -> [],st
        | Instruction ins ->
            let ins,st = expl_instr ins st in
            [pseudo_parsed_tr (Instruction ins)],st
        | Label (lbl,ins) ->
            begin
              let lbl,st = find_lab lbl st in
              let inss,st = aux ins st in
              let inss = match inss with
              | [] ->  [pseudo_parsed_tr (Label (lbl, Nop))]
              | ins::inss ->  Label(lbl,ins)::inss in
              inss,st
            end
        | Symbolic s -> find_code s st
        | Macro (_,_) -> assert false
        | Pagealign | Skip _ -> assert false (* Not implemented yet *)
      in fun is st -> match is with
      | [] -> []
      | i::is ->
          let i,st = aux i st in
          let is = expl_pseudos is st in
          i@is in
    let st = { free; reg_env=[]; label_env=[];} in
    expl_pseudos instrs st

end

module DefaultDumper(A:ArchBase.S) = struct
  type pseudo = A.pseudo
  include SimpleDumper.Make(struct let compat = false end)
      (struct
        module A = A

        type v = ParsedConstant.v
        let dump_v = ParsedConstant.pp_v

        let dump_loc = MiscParser.dump_location

        let dump_state_atom a =
          MiscParser.dump_state_atom MiscParser.is_global dump_loc dump_v a

        type state = MiscParser.state

        let dump_state st =
          DumpUtils.dump_state
            dump_state_atom
            (MiscParser.env_for_pp st)

        type prop = MiscParser.prop

        let dump_atom a =
          ConstrGen.dump_atom
            dump_loc MiscParser.dump_location_brk ParsedConstant.pp_v MiscParser.dump_fault_type
            a

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = MiscParser.location
        let dump_location = dump_loc

        type fault_type = MiscParser.fault_type
        let dump_fault_type = MiscParser.dump_fault_type
      end)
end
