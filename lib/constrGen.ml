(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


type ('loc,'v) atom =
  | LV of 'loc * 'v
  | LL of 'loc * 'loc

type ('l,'v) prop =
  | Atom of ('l, 'v) atom
  | Not of ('l,'v) prop
  | And of ('l,'v) prop list
  | Or of ('l,'v) prop list
  | Implies of ('l,'v) prop * ('l,'v) prop

type 'prop constr =
    ForallStates of 'prop
  | ExistsState of 'prop
  | NotExistsState of 'prop

type  ('loc,'v) cond = ('loc,'v) prop constr

let constr_true = ForallStates (And [])

let is_true = function
  |  ForallStates (And []) -> true
  | _ -> false

let is_existential p = match p with
| ExistsState _ -> true
| ForallStates _
| NotExistsState _ -> false

let prop_of = function
| ExistsState p
| ForallStates p
| NotExistsState p -> p

(*
let rec nnf p = match p with
| Atom _|Not (Atom _) -> p
| Not (Not p) -> nnf p
| Not (Or ps) -> And (List.map (fun p -> nnf (Not p)) ps)
| Not (And ps) -> Or (List.map (fun p -> nnf (Not p)) ps)
| Not (Implies (p,q)) -> And [nnf p; nnf (Not q)]
| Implies (p,q) -> Or [nnf (Not p); nnf q]
| And ps -> And (List.map nnf ps)
| Or ps -> Or (List.map nnf ps)


let distri_and ps qs =
  List.fold_right
    (fun p k ->
      List.fold_right
        (fun q k -> (p@q)::k) qs k)
    ps

let rec do_dnf p = match p with
| Atom _|Not (Atom _) -> [[p]]
| Or ps ->
    let pss = List.map do_dnf ps in
    List.concat pss
| And ps ->
    let pss = List.map do_dnf ps in
    List.concat pss
| Not _|Implies _ -> assert false

    
let dnf p =
  let p = nnf p in
  do_dnf p

*)

(* Style of constraints in papers *)
type kind =
  | Allow
  | Forbid
  | Require
  | Unconstrained

let kind_of = function
  | ExistsState _ -> Allow
  | ForallStates _ -> Require
  | NotExistsState _ -> Forbid

let set_kind k c =
  let p = prop_of c in
  match k with
  | Allow -> ExistsState p
  | Forbid -> NotExistsState p
  | Require -> ForallStates p
  | Unconstrained -> c

let pp_kind = function
  | Allow -> "Allowed"
  | Forbid -> "Forbidden"
  | Require -> "Required"
  | Unconstrained -> "Final"

let parse_kind = function
  | "Allow"|"Allowed" -> Some Allow
  | "Require" | "Required" -> Some Require
  | "Forbid"|"Forbidden" -> Some Forbid
  | ""|"Unknown"|"???"|"---" -> Some Unconstrained
  | _ -> None

let rec fold_prop f_atom p = match p with
| Atom a -> f_atom a
| Not p -> fold_prop f_atom p
| And ps
| Or ps ->
    List.fold_right (fold_prop f_atom) ps
| Implies (p,q) ->
    fun y -> fold_prop f_atom q (fold_prop f_atom p y)
      
let fold_constr f_atom c = match c with
| ForallStates p 
| ExistsState p 
| NotExistsState p -> fold_prop f_atom p   

let rec map_prop c_atom p = match p with
| Atom a ->
    let a = c_atom a in  Atom a
| Not p ->
    Not (map_prop c_atom p)
| And pl ->
    And (List.map (fun p -> map_prop c_atom p) pl)
| Or pl ->
    Or (List.map (fun p -> map_prop c_atom p) pl)
| Implies (p,q) ->
    Implies (map_prop c_atom p, map_prop c_atom q)

let map_constr f c = match c with
| ForallStates p -> ForallStates (map_prop f p)
| ExistsState p -> ExistsState (map_prop f p)
| NotExistsState p -> NotExistsState (map_prop f p)

(* Pretty print *)

type op = O_top | O_and | O_or | O_implies

let is_paren above here = match above,here with
| O_top,_ -> false
| _,O_top -> assert false
| O_implies,O_implies -> false
| O_implies,(O_or|O_and) -> true
| O_and,O_and -> false
| O_and,(O_or|O_implies) -> true
| O_or,(O_and|O_or) -> false
| O_or,O_implies -> true

let paren above here s =
  if is_paren above here then "(" ^ s ^ ")"
  else s

type 'atom pp_arg =
    { pp_true : string;
      pp_false : string;
      pp_not : string;
      pp_or : string;
      pp_and : string;
      pp_implies : string;
      pp_mbox : string -> string;
      pp_atom : 'atom -> string; }

let pp_prop arg =

  let rec pp_prop above p = match p with
  | Atom a -> arg.pp_atom a
  | Not p -> arg.pp_not ^ "(" ^ pp_prop O_top p ^ ")"
  | And [] -> arg.pp_true
  | And ps ->
      paren above O_and
        (String.concat (arg.pp_and)  (List.map (pp_prop O_and) ps))
  | Or [] -> arg.pp_false
  | Or ps ->
      paren above O_or
        (String.concat (arg.pp_or)  (List.map (pp_prop O_or) ps))
  | Implies (p1,p2) ->
      paren above O_implies
        (pp_prop O_implies p1 ^
         arg.pp_implies ^
         pp_prop O_implies p2) in
  pp_prop O_top 



let mk_arg pp_atom =
    { pp_true="true";
      pp_false="false";
      pp_not="not " ;
      pp_or=" \\/ " ;
      pp_and=" /\\ " ;
      pp_implies=" => ";
      pp_mbox=(fun s -> s) ;
      pp_atom=pp_atom; }

let dump_prop pp_atom =
  fun chan p -> output_string chan (pp_prop (mk_arg pp_atom) p)

let prop_to_string pp_atom = pp_prop (mk_arg pp_atom)

open Printf

let dump_constraints chan pp_atom c = match c with
| ForallStates p ->
    fprintf chan "forall (%a)" (dump_prop pp_atom) p
| ExistsState p ->
    fprintf chan "exists (%a)" (dump_prop pp_atom) p
| NotExistsState p ->
    fprintf chan "~exists (%a)" (dump_prop pp_atom) p

let constraints_to_string pp_atom c =  match c with
| ForallStates p ->
    sprintf "forall (%s)" (prop_to_string pp_atom p)
| ExistsState p ->
    sprintf "exists (%s)" (prop_to_string pp_atom p)
| NotExistsState p ->
    sprintf "~exists (%s)" (prop_to_string pp_atom p)
