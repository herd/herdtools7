(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Latex pretty print of programs *)

module type Config = sig
  val withindex : bool
  val ascommands : bool
  val texmacros : bool
  val hexa : bool
  val outputdir : string option
end

module Make(O:Config)(A:Arch_tools.S) =
  struct

    open Printf

(* Internal handling of test *)
    type test =
        {
         arch : Archs.t ;
         name : Name.t ;
         prog : (MiscParser.proc * A.pseudo list) list ;
         init : (A.location * (TestType.t * A.v)) list ;
         constr : (A.location, A.v, A.fault_type) ConstrGen.prop ConstrGen.constr ;
       }

    let just_name test = test.name.Name.name
    let very_readable_name test = test.name.Name.name

(* Latex printing the program *)

    let a_pp_instruction i =
      let bodytext = A.pp_instruction PPMode.Latex i in
      if O.texmacros
      then "\\asm{" ^ bodytext ^ "}"
      else bodytext

    let pp_pseudo i k = match i with
    | A.Nop -> k
    | _ ->
        let rec pp_rec i =  match i with
        | A.Nop -> ""
        | A.Instruction ins -> a_pp_instruction ins
        | A.Label (lbl,i) -> sprintf "%s: %s" lbl (pp_rec i)
        | A.Symbolic s -> sprintf "codevar:%s" s
        | A.Macro (_,_) -> assert false 
        | A.Align n -> sprintf ".p2align %d" n in
        pp_rec i::k

    let is_nil = function [] -> true | _::_ -> false

    let listi f n =
      let rec do_rec k =
        if k < n then f k::do_rec (k+1) else [] in
      do_rec 0

    let extract cols =
      let cs =
        List.map
	  (fun col -> match col with
	  | [] -> None,[]
	  | x::col -> Some x,col) cols in
      List.split cs

    let pp_rows chan row0 row_start row_end delim cols =
      let rec do_iter n cols =
        if not (List.for_all is_nil cols) then begin
	  let row,cols = extract cols in
	  fprintf chan "%s%s%s%s"
	    row_start (if n=0 then row0 else "")
	    (String.concat delim (List.map (Misc.proj_opt "") row))
	    row_end ;
	  do_iter (n+1) cols
        end in
      do_iter 0 cols

    let pp_arch a =
      match a with
      | `PPC -> "POWER"
      | `X86_64 -> "X86\\_64"
      | _ -> Archs.pp a

    let pp_prog chan prog test initial_opt finals_opt show_po =
      let divd_program =
        List.map
	  (fun (_p,c) ->  List.fold_right  pp_pseudo c [])
	  prog in
      let numprocs = List.length divd_program in
      let maxinstrs =
        List.fold_right
	  (fun instrs maxcnt ->
	    let numinstrs = List.length instrs in
	    max numinstrs maxcnt)
	  divd_program 0 in
      if O.withindex then
        fprintf chan "\\index{%s}\n" (very_readable_name test);
      fprintf chan "\\begin{tabular}{" ;
      (* Tabular argument *)
      output_string chan (if show_po then "|l|" else "|") ;
      List.iter (fun _ -> output_string chan  "l|") prog ;
      output_string chan "}\n" ;
      let numberofcolumns = (if show_po then 1 else 0) + numprocs in
      let pp_arch =
        if test.arch <> `X86 then
          pp_arch test.arch
        else "" in
      if numberofcolumns > 1 then
        fprintf chan
          "\\multicolumn{%i}{@{}l}{\\makebox[0pt][l]{%s}}\n&\\multicolumn{1}{r@{}}{\\makebox[0pt][r]{%s}}\\\\\n"
          (numberofcolumns-1)
          (just_name test)
          pp_arch
      else
        fprintf chan "\\multicolumn{%i}{@{}l}{%s %s}\\\\\n"
          (numberofcolumns) (just_name test) pp_arch ;
      fprintf chan "\\hline\n" ;
      (* Header, made annoying by first '|' *)
      let width = "\\litmuscolwidth" ^ match numprocs with
      | 2 -> "Two"
      | 3 -> "Three"
      | 4 -> "Four"
      | _ -> "Two" in
      let pp_ao = function
        | None -> ""
        | Some a -> ":" ^ String.concat "," a in
      let pp_f = function
        | MiscParser.Main -> ""
        | MiscParser.FaultHandler -> ".f" in
      let pp_proc s chan (p,ao,f) =
        let th =
          if O.texmacros then sprintf "\\myth{%i%s%s}" p (pp_ao ao) (pp_f f)
          else sprintf "Thread %i%s%s" p (pp_ao ao) (pp_f f) in
        fprintf chan
          "\\multicolumn{1}{%sc|}{\\makebox[%s][c]{%s}}"
          s width th in
      begin if show_po then
        fprintf chan "\\multicolumn{1}{|c|}{%s}" (just_name test)
      else match prog with
      | (p,_)::_ -> pp_proc "|" chan p
      | [] -> assert false
      end ;
      let remaining =
        if show_po then prog
        else match prog with _::rem -> rem | [] -> assert false in
      List.iter
        (fun (p,_) -> fprintf chan " & %a" (pp_proc "") p)
        remaining ;
      output_string chan " \\\\ \\hline\n" ;
      (* Now instructions *)
      let cols =
        if show_po then
	  listi (fun i -> sprintf "poi:%i" i) maxinstrs::divd_program
        else
	  divd_program in
      pp_rows chan "\\rule{0ex}{2ex}" "" "\\\\\n" " & " cols ;
      output_string chan "\\hline\n" ;
      (* States *)
      let _initial_final so =
        match so with
        | None -> ()
        | Some s ->
	    fprintf chan "\\multicolumn{%i}{|l|}{%s}\\\\\\hline\n"
	      (numprocs + if show_po then 1 else 0) s in
      let initial_final_line_breaking so =
        match so with
        | None -> ()
        | Some s ->
            let wedge = "\\mywedge" in
            let parts = Str.split (Str.regexp_string wedge) s in
            let allowed_parts_on_first_line = (numprocs + 1) in
            let rec take n acc xs =
              if n=0 then (List.rev acc,xs) else match xs with
              | [] -> (List.rev acc,xs)
              | x::xs' -> take (n-1) (x::acc) xs' in
	    let rec do_print_parts str_list init =
	      if List.length str_list <= allowed_parts_on_first_line then
	        begin
		  let line =
		    (if init then "" else "$\\phantom{Initial state:}") ^
		    String.concat wedge
		      (if init then str_list else " "::str_list) in
	          fprintf chan "\\multicolumn{%i}{|l|}{%s}\\\\\\hline\n"
		    (numprocs + if show_po then 1 else 0)
		    line
	        end
	      else
	        begin
                  let (first_line,second_line) =
		    take allowed_parts_on_first_line [] str_list in
                  let first_line =
		    (if init then "" else "$\\hspace{5em}") ^
		    String.concat wedge
		      (if init then first_line else " "::first_line) ^ "$" in
	          fprintf chan "\\multicolumn{%i}{|l|}{%s}\\\\\n"
		    (numprocs + if show_po then 1 else 0) first_line ;
		  do_print_parts second_line false
	        end
	    in
	    do_print_parts parts true in
      initial_final_line_breaking initial_opt ;
      List.iter initial_final_line_breaking finals_opt ;
      (* Ouf *)
      output_string chan "\\end{tabular}\n"

    let pp_v = ParsedConstant.pp O.hexa
    let pp_location = A.pp_location
    let pp_location_brk = A.pp_location_brk


    let pp_nice_state sc sep pp =
      let pp = List.map pp sc in
      String.concat sep pp


    let pp_equal = "\\mathord{=}"
    let pp_mbox = sprintf "\\mbox{%s}"
    let pp_asm =
      if O.texmacros then sprintf "\\asm{%s}"
      else fun s -> s
    let pp_asm_v v = pp_asm (pp_v v)

    let pp_initial_state sc =
      let open TestType in
      "\\begin{tabular}[t]{|l|}\n\\hline\n"
      ^ "Initial state\\\\ \\hline \n"
      ^ pp_nice_state sc ""
          (fun (l,(t,v)) -> match t with
          | TyDef ->
	    "{} "^ pp_mbox (pp_location l) ^ " " ^
              pp_equal ^ " " ^ pp_v v ^ "\\\\ "
          | TyDefPointer ->
	    "{} "^ pp_mbox ("*" ^ pp_location l) ^ " " ^
              pp_equal ^ " " ^ pp_v v ^ "\\\\ "
          | Ty t ->
	    "{} "^ pp_mbox (t ^ " " ^ pp_location l) ^ " " ^
              pp_equal ^ " " ^ pp_v v ^ "\\\\ "
          | Pointer t ->
	    "{} "^ pp_mbox (t ^ " *" ^ pp_location l) ^ " " ^
              pp_equal ^ " " ^ pp_v v ^ "\\\\ "
          | TyArray _|Atomic _ -> Warn.fatal "Array/Atomic type not implemented...")
      ^ "\\hline \\end{tabular}\n"

    let is_zero = function
      | Constant.Concrete "0" -> true
      | _ -> false

    let pp_initial_state_flat sc =
      let non_zero_constraints =
        List.filter_map
	  (fun (l,(_,v))->
            if is_zero v then None
            else
              let ppv =
                if O.texmacros then "\\asm{" ^ pp_v v ^"}"
                else "\\mbox{" ^ pp_v v ^ "}" in
              Some (" \\mbox{"^ pp_location l ^
                      "} \\mathord{=} " ^ ppv ^" "))
	  sc in (* That will do *)
      match non_zero_constraints with
      | [] -> None
      | _ ->
          let has_zero =
            List.exists
              (fun (_,(_,v)) -> is_zero v)
              sc in
          Some
            begin
              "Initial state: $"
	      ^ String.concat "\\mywedge " non_zero_constraints
              ^ "$"
	      ^
                begin if has_zero then
                  sprintf " (elsewhere $%s$)"
                    (if O.texmacros then "\\asm{0}" else "0")
                else ""
                end
          end

    let pp_binding l v =  pp_location l ^ "= "^ pp_v v

    let pp_final_calc_state sc =
      "\\begin{tabular}[t]{|l|}\n\\hline\n" ^
      "Final state (calculated)\\\\ \\hline \n" ^
      pp_nice_state sc ""
        (fun (l,v) -> "{} "^ pp_binding l v ^"\\\\\n")
      ^ "\\hline \\end{tabular}\n"

    let pp_ft _ft = "" (* FIXME *)
    let pp_asm_ft ft = pp_asm (pp_ft ft)

    open ConstrGen

    module Constr = struct


      let arg =
        { ConstrGen.pp_true = "\\top" ;
          pp_false = "\\perp" ;
          pp_not = "\\neg" ;
          pp_and = "\\mywedge" ;
          pp_or = "\\vee" ;
          pp_implies = "\\Righarrow" ;
          pp_mbox = sprintf "\\mbox{%s}" ;
          pp_atom = fun a ->
            match a with
            | LV (loc,v) ->
                let pp_loc =
                  match loc,v with
                  | (Deref _,_)
                  | (_,Constant.ConcreteVector _)
                    -> pp_location
                  | _ -> pp_location_brk in
              pp_mbox (ConstrGen.dump_rloc pp_loc loc) ^
              pp_equal ^
              pp_mbox (pp_asm_v v)
          | LL (l1,l2) ->
              pp_mbox (pp_location_brk l1) ^
              pp_equal ^
              pp_mbox (pp_location_brk l2) ;
          | FF f ->
              pp_mbox (Fault.pp_fatom pp_asm_v pp_asm_ft f)
        }

      let enddollar = sprintf "$%s$"

      let pp_as_kind c =
        let bodytext = pp_kind (kind_of c) in
        if O.texmacros then
          bodytext ^ " Final State" else bodytext

      let pp =
        let pp_prop p = enddollar (ConstrGen.pp_prop arg p) in
        fun c -> match c with
        | ExistsState p
        | NotExistsState p
        | ForallStates p ->
            pp_as_kind c ^ ": "^ pp_prop p
    end

    let pp_constraint cstr = match cstr with
    | ForallStates (And []) -> None
    | _ -> Some (Constr.pp cstr)
    let pp_newcommand chan name =
      Printf.fprintf chan "\\newcommand{\\%s}{" name

    let pp_close_par chan = output_string chan "}%\n"
(* partial escaping of strings into legal tex command identifiers *)
  let tex_command_escape s =
    let char_list_of_string s =
      let n = String.length s in
      let rec f i = if i=n then [] else String.get s i :: f (i+1) in
      f 0 in
    String.concat ""
      (List.map
         (fun c -> match c with
         | '-' ->"M"
         | '_' ->"U"
         | '#' -> "H"
         | '\'' -> "P"
         | '0' -> "Zero"
         | '1' -> "One"
         | '2' -> "Two"
         | '3' -> "Three"
         | '4' -> "Four"
         | '5' -> "Five"
         | '6' -> "Six"
         | '7' -> "Seven"
         | '8' -> "Eight"
         | '9' -> "Nine"
         | _ -> String.make 1 c)
         (char_list_of_string s))

    let get_tex t = tex_command_escape t.name.Name.texname
    let texname_prog t = get_tex t ^ "Prog"
    let texname_initial t = get_tex t ^ "Initial"
    let texname_final t = get_tex t ^ "Final"
    let texname_final_calc t = get_tex t ^ "FinalCalc"

    let dump_prog_internal test as_tex_commands fd =
      let ppd_isc = pp_initial_state_flat test.init in
      let constr = test.constr in
      let ppd_constr = [pp_constraint constr] in
      if as_tex_commands then pp_newcommand fd (texname_prog test) ;
      pp_prog fd
	test.prog
        test ppd_isc
	ppd_constr
            false ; (* do not show po *)
      if as_tex_commands then
        begin
          pp_close_par fd ;
          pp_newcommand fd (texname_initial test) ;
          output_string fd (pp_initial_state test.init);
          pp_close_par fd ;
          pp_newcommand fd (texname_final test) ;
          output_string fd (Constr.pp constr) ;
          pp_close_par fd
        end

    let dump_prog_filename test as_tex_commands  =
      Misc.output_protect (dump_prog_internal test as_tex_commands)

    let filebase test = Misc.filebase test.name.Name.file

    let dump_prog name parsed =
      let test =
        { arch = A.arch ;
          name = name ;
          prog = parsed.MiscParser.prog;
          init = parsed.MiscParser.init ;
          constr = parsed.MiscParser.condition;
        } in
      match O.outputdir with
      | None -> dump_prog_internal test O.ascommands stdout
      | Some d ->
          let b = filebase test in
          let f = Filename.concat d b in
          dump_prog_filename test true (f ^ "-prog.tex") ;
          dump_prog_filename test false (f ^ "-prg.tex")
  end
