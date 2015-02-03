(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


open AST
open Printf

let rec fprintf_list s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> fprintf chan "(%s %a %a)" s f x (fprintf_list s f) xs

let rec fprintf_list_infix s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> 
    fprintf chan "(%a %s %a)" 
      f x s (fprintf_list_infix s f) xs

let lem_of_konst chan = function
  | Empty SET -> fprintf chan "emps"
  | Empty RLN -> fprintf chan "empr"

let rec lem_of_op2 args chan es = function
  | Union -> fprintf_list_infix "union" (lem_of_exp args) chan es
  | Inter -> fprintf_list_infix "inter" (lem_of_exp args) chan es
  | Diff -> fprintf_list_infix "\\" (lem_of_exp args) chan es
  | Seq -> fprintf_list "seq" (lem_of_exp args) chan es
  | Cartesian -> fprintf_list "cross" (lem_of_exp args) chan es
  | _ -> Warn.fatal "lem_of_op2"

and lem_of_op1 args chan e = function
  | Plus -> fprintf chan "(tch %a)" (lem_of_exp args) e
  | Star -> fprintf chan "(rtc %a)" (lem_of_exp args) e
  | Opt -> fprintf chan "(rc X %a)" (lem_of_exp args) e
  | Select _ -> fprintf chan "Select not done yet"
  | Inv -> fprintf chan "(inv %a)" (lem_of_exp args) e
  | Square -> fprintf chan "(cross %a %a)" (lem_of_exp args) e (lem_of_exp args) e
  | Ext -> fprintf chan "(ext %a)" (lem_of_exp args) e
  | Int -> fprintf chan "(int %a)" (lem_of_exp args) e
  | NoId -> fprintf chan "(noid %a)" (lem_of_exp args) e
  | Set_to_rln -> fprintf chan "(stor %a)" (lem_of_exp args) e
  | Comp SET -> fprintf chan "(comps X %a)" (lem_of_exp args) e
  | Comp RLN -> fprintf chan "(compr X %a)" (lem_of_exp args) e
  | _ -> Warn.fatal "Unknown operator in herd2lem"
and lem_of_var args chan x = 
  match x with
  | "rf" | "asw" | "lo" ->
    fprintf chan "X.%sh" x
  | "po" | "addr" | "data" | "co" | "S" -> 
    fprintf chan "X.%s" x
  | "_" -> fprintf chan "(unis X)"
  | "id" -> fprintf chan "idh X"
  | _ -> 
    let x = Str.global_replace (Str.regexp_string "-") "_" x in
    if List.mem x args then
      fprintf chan "%s" x
    else
      fprintf chan "(%s X)" x

and lem_of_exp args chan = function
  | Konst (_,k) -> lem_of_konst chan k
  | Var (_,x) -> lem_of_var args chan x
  | Op1 (_,op1, e) -> lem_of_op1 args chan e op1
  | Op (_,op2, es) -> lem_of_op2 args chan es op2
  | App (_,e,es) -> fprintf chan "(%a(%a))"
                    (lem_of_exp args) e 
                    (fprintf_list_infix "," (lem_of_exp args)) es 
  | Bind _ -> fprintf chan "Bindings not done yet"
  | BindRec _ -> fprintf chan "Recursive bindings not done yet"
  | Fun _ -> fprintf chan "Local functions not done yet"
  | _ -> Warn.fatal "explicitset/match/tag etc. in herd2lem"


and lem_of_binding chan (x, e) = 
  match e with
    | Fun (_,xs,e,_,_) ->
      fprintf chan "let %s X (%a) = %a" 
        x 
        (fprintf_list_infix "," (fun _ x -> fprintf chan "%s" x)) xs
        (lem_of_exp xs) e
    | _ ->
      fprintf chan "let %s X = %a" 
        x 
        (lem_of_exp []) e

let fprintf_so x chan so = 
  fprintf chan "%s" (match so with
    | None -> x
    | Some s -> s)

let lem_of_test = function
  | Acyclic -> "acyclic"
  | Irreflexive -> "irreflexive"
  | TestEmpty -> "is_empty"

let provides : string list ref = ref []
let requires : string list ref = ref []
let seen_requires_clause : bool ref = ref false

let lem_of_ins chan = function
  | Let (_,bs) -> List.iter (lem_of_binding chan) bs
  | Rec (_,bs) -> List.iter (lem_of_binding chan) bs
  (* doesn't handle recursion properly *)
  | Test (_,_, test, exp, name, test_type) ->
    let name = begin match name with 
        | None -> Warn.user_error "You need to give each constraint a name!\n"
        | Some name -> name 
    end in
    fprintf chan "let %s X = %s (%a)" 
      name
      (lem_of_test test)
      (lem_of_exp []) exp;
    begin match test_type with
      | Provides -> 
        if (!seen_requires_clause) then
          Warn.user_error "Provides-clause follows requires-clause!";
        provides := name :: (!provides)
      | Requires -> 
        seen_requires_clause := true;
        requires := name :: (!requires)
    end
  | UnShow _ -> ()
  | Show _ -> ()
  | ShowAs _ -> ()
  | Latex _ -> ()
  | Include (_,file) ->
    let file = String.capitalize file in
    let file = String.sub file 0 (String.length file - 4) in
    provides := (sprintf "%s.provides_clauses" file) :: (!provides);
    requires := (sprintf "%s.requires_clauses" file) :: (!requires);
    fprintf chan "open import %s" file
  | ProcedureTest _|Procedure _|Call _|Enum _|Debug _|Foreach _
  | ForOrder _
    ->
      Warn.fatal "procedure/call/enum/debug/foreach in herd2lem"

let lem_of_prog chan prog = 
  fprintf chan "open import Pervasives\n";
  fprintf chan "open import Relation\n";
  fprintf chan "open import Herd\n\n";
  List.iter (fprintf chan "%a\n\n" lem_of_ins) prog;
  fprintf chan "let provides_clauses X =\n";
  fprintf chan "  herd_provides X &&\n";
  List.iter (fprintf chan "  %s X &&\n") (List.rev (!provides));
  fprintf chan "  true\n\n";
  fprintf chan "let requires_clauses X =\n";
  List.iter (fprintf chan "  %s X &&\n") (List.rev (!requires));
  fprintf chan "  true\n\n";
  fprintf chan "let model =\n";
  fprintf chan "  <| provides = provides_clauses;\n";
  fprintf chan "     requires = requires_clauses;\n";
  fprintf chan "  |>\n";
