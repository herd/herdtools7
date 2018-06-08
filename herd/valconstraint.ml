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

(** A simple constraint solver *)

(* Constraints: v is any value: a constant cst or a variable S.
    Possible constraints are
    v1 := v2
    v1 := v2 (op) v3
    Solutions are
    S -> cst *)

module type S = sig

  type atom
  type cst
  type location
  type state

  type expr =
    | Atom of atom
    | ReadInit of location * state
    | Unop of Op.op1 * atom
    | Binop of Op.op * atom * atom
    | Terop of Op.op3 * atom * atom * atom


  type rvalue = expr

  type cnstrnt =
    | Assign of atom * rvalue
    | Unroll of string (* unrolling stopped *)

  type cnstrnts = cnstrnt list
  val pp_cnstrnts : cnstrnt list -> string

  type solution 

  type answer =
    | NoSolns
    | Maybe of solution *  cnstrnts

  val pp_answer : answer -> string

  val solve : cnstrnt list -> answer
end

module type Config = sig
  val hexa : bool
  val debug : bool
end

module Make (C:Config) (A:Arch_herd.S) : S
with type  atom = A.V.v
and type cst = A.V.Cst.v
and type solution = A.V.solution
and type location = A.location
and type state = A.state =
  struct

    open Printf
    module V = A.V

    type atom = V.v
    type cst = V.Cst.v
    type location = A.location
    type state = A.state

    type expr =
      | Atom of atom
      | ReadInit of location * state
      | Unop of Op.op1 * atom
      | Binop of Op.op * atom * atom
      | Terop of Op.op3 * atom * atom * atom

    let map_expr fv e = match e with
    | Atom v -> Atom (fv v)
    | ReadInit (loc,s) -> ReadInit (A.map_loc fv loc,s)
    | Unop (o,a1) -> Unop (o,fv a1)
    | Binop (o,a1,a2) -> Binop (o,fv a1, fv a2)
    | Terop (o,a1,a2,a3) -> Terop (o,fv a1, fv a2, fv a3)

    type rvalue = expr

    type cnstrnt =
      | Assign of V.v * rvalue
      | Unroll of string

    type cnstrnts = cnstrnt list

    let pp_atom a = V.pp C.hexa a

    let pp_expr e =
      match e with
      | Atom a -> pp_atom a
      | ReadInit(loc,_) -> A.dump_location loc ^ " in init"
      | Unop (o,a1) -> sprintf "%s(%s)" (Op.pp_op1 C.hexa o) (pp_atom a1)
      | Binop (o,a1,a2) -> pp_atom a1 ^ Op.pp_op o ^ pp_atom a2
      | Terop (op,a1,a2,a3) ->
	  Op.pp_op3 op
	    (pp_atom a1) (pp_atom a2) (pp_atom a3)
	    
    let pp_rvalue e = pp_expr e

    let pp_cnstrnt cnstr =  match cnstr  with
      | Assign (v,rval) -> 
	  (V.pp C.hexa v) ^ ":=" ^(pp_rvalue rval)
      | Unroll msg -> "Unroll "^msg

    let pp_cnstrnts lst = 
      String.concat "\n" 
	(List.map pp_cnstrnt lst)

    type solution = V.solution

    type answer =
      | NoSolns
      | Maybe of solution * cnstrnts

	    
    let pp_answer =

      let pp_cns cns = match cns with
      | [] -> ""
      | _::_ ->
	  "\nUnsolved equations:\n" ^
	  (pp_cnstrnts cns) in

      fun soln -> match soln with
      | NoSolns -> "No solutions"
      | Maybe (sol,cns) ->
	  let sol_pped =
	    let bds =
	      V.Solution.fold
		(fun v i k -> (v,i)::k)
		sol [] in
	    String.concat ", "
	      (List.map
		 (fun (v,i) -> V.pp_csym v ^ "<-" ^ V.pp C.hexa i) bds) in

	  sol_pped ^ pp_cns cns

(**************************************)
(* Initial phase: normalize variables *)
(**************************************)

(*
  straightforward union-find.
  <http://en.wikipedia.org/wiki/Disjoint-set_data_structure>
*)

(* Collect all variables in partition *)

    module OV = struct
      type t = V.csym
      let compare = V.compare_csym
    end
    module Part = Partition.Make (OV)

    let add_var t v = match v with
    | V.Val _ -> t
    | V.Var x -> Part.add t x

    let add_var_loc t loc = match A.undetermined_vars_in_loc loc with
    | None -> t
    | Some v -> add_var  t v

    let add_vars_expr t e = match e with
    | Atom v ->  add_var t v
    | ReadInit (loc,_) -> add_var_loc t loc
    | Unop (_,v) -> add_var t v
    | Binop (_,v1,v2) ->
        add_var (add_var t v1) v2
    | Terop (_,v1,v2,v3) ->
        add_var (add_var (add_var t v1) v2) v3

    let add_vars_cn t cn = match cn with
    | Assign (v,e) ->
        add_vars_expr (add_var t v) e
    | Unroll _ -> t

    let add_vars_cns cns = List.fold_left add_vars_cn (Part.create ()) cns

(* Perform union-find *)

    let uf_cn t cn = match cn with
    | Assign (V.Var v,Atom (V.Var w)) -> Part.union t v w
    | _ -> ()

    let uf_cns t cns =
      List.iter (uf_cn t) cns ;
      Part.as_solution t

(* Simplify equations *)

    let  subst_atom m v = match v with
    | V.Val _ -> v
    | V.Var x ->
        try V.Var (Part.Sol.find x m)
        with Not_found -> v

    let subst_expr m = map_expr (subst_atom m)

    let subst_cn m cn k = match cn with
    | Assign (v,Atom w) ->
        let v = subst_atom m v
        and w = subst_atom m w in
        if V.compare v w = 0 then k else Assign (v,Atom w)::k
    | Assign (v,e) ->
        let v = subst_atom m v
        and e = subst_expr m e in
        Assign (v,e)::k
    | Unroll _ -> cn::k

    let subst_cns soln cns = List.fold_right (subst_cn soln) cns []


(* All together *)

    let normalize_vars cns =
      let t = add_vars_cns cns in
      let m = uf_cns t cns in
      let cns = subst_cns m cns in
      if C.debug then begin 
       eprintf "* Normalizes to *\n%s\n%!" (pp_cnstrnts cns)
      end ;
      m,cns


(*****************)
(* Solver proper *)
(*****************)

(*
  Solver proceeds by iterating three simple steps,
  could use a topological sorting, to be more efficient.
  Not needed at the moment.
 *)


(* Phase 1: detection of contradictions and erasure of trivial equations *)
    exception Contradiction

    let mk_atom_from_expr e =
      try match e with
      | Atom _ -> e
      | ReadInit (loc,init) -> Atom (A.look_address_in_state init loc)
      | Unop (op,v1) -> Atom (V.op1 op v1)
      | Binop (op,v1,v2) -> Atom (V.op op v1 v2)
      | Terop (op,v1,v2,v3) -> Atom (V.op3 op v1 v2 v3)
      with
      | A.LocUndetermined
      | V.Undetermined -> e

    let check_true_false cn k = match cn with
    | Assign (v,e) ->
	let e = mk_atom_from_expr e in
	begin match e with
	| Atom w ->
	    if V.is_var_determined v && V.is_var_determined w then
	      if V.compare v w = 0 then k
	      else raise Contradiction
	    else
	      Assign (v,e)::k
	| ReadInit _ 
	| Unop _|Binop _|Terop _ -> Assign (v,e)::k
	end
    | Unroll _ -> cn::k

    let check_true_false_constraints cns =
      List.fold_right check_true_false cns []

(* Phase 3, substitution *)

    let simplify_vars_in_atom soln v = match v with
    | V.Val _ -> v
    | V.Var sym ->
        try V.Val (V.Solution.find sym soln) with Not_found -> v

    let simplify_vars_in_expr soln = map_expr (simplify_vars_in_atom soln)

    let simplify_vars_in_cnstrnt soln cn =
      match cn with
      | Assign (v,rval) ->
	  let v = simplify_vars_in_atom soln v in
	  let rval = simplify_vars_in_expr soln rval in
	  Assign (v,rval)
      | Unroll _ -> cn


    let simplify_vars_in_cnstrnts soln cs =
      List.map (simplify_vars_in_cnstrnt soln) cs


(* Phase 2, "solving": just collect equations S := cst / cst := S *)
    let singleton v i = V.Solution.add v i V.Solution.empty
    and empty = V.Solution.empty

    let solve_cnstrnt cnstr = match cnstr with
    | Assign (V.Var v,Atom (V.Val i))
    | Assign (V.Val i,Atom (V.Var v)) ->
        singleton v i
    | Assign (V.Val _,Atom (V.Val _)) ->
    (* By previous application of check_true_false *)
        assert false
    | Assign (V.Var _,Atom (V.Var _))
    (* can occur in spite of variable normalization (ternary if) *)
    | Assign (_,(Unop _|Binop _|Terop _|ReadInit _)) -> empty
    | Unroll _ -> empty

(* merge of solutions, with consistency check *)
    let merge sol1 sol2 =
      V.Solution.fold
	(fun v i k ->
	  try
	    let i' = V.Solution.find v sol2 in
	    if V.Cst.compare i i' = 0 then
	      V.Solution.add v i k
	    else
	      raise Contradiction
	  with Not_found -> V.Solution.add v i k)
	sol1 sol2

let solve_cnstrnts =
  List.fold_left
    (fun solns cnstr -> merge (solve_cnstrnt cnstr) solns)
    V.Solution.empty

(*******************************)
(* Iterate basic solving steps *)
(*******************************)

    (* Just union since there are no variables in rhs of solutions *)
    let compose_sols sol1 sol2 = V.Solution.fold V.Solution.add sol1 sol2

    let rec solve_step cns solns_final =
      (* Phase 1, check individual constraint validity *)
      let cns = check_true_false_constraints cns in
      (* Phase 2, orient constraints S := cst / cst := S *)
      let solns = solve_cnstrnts cns in      
      if V.Solution.is_empty solns then
	solns_final,cns
      else
	(* Phase 3, and iteration *)
	let cns =  simplify_vars_in_cnstrnts solns cns
	and solns_final = compose_sols solns solns_final in
	solve_step cns solns_final

    let add_vars_solns m solns0 =
      Part.Sol.fold
        (fun  x y solns ->
          try
            let cst = V.Solution.find y solns0 in
            V.Solution.add x (V.Val cst) solns
          with Not_found ->
            V.Solution.add x (V.Var y) solns)
        m
        (V.Solution.map (fun x -> V.Val x) solns0)
      
    let solve lst = 
      if C.debug then begin
	prerr_endline "** Solve **" ;
	eprintf "%s\n" (pp_cnstrnts lst) ; flush stderr
      end ;
      let m,lst = normalize_vars lst in
      let sol = 
	try
	  let solns,lst = solve_step lst V.Solution.empty in
          let solns = add_vars_solns m solns in
	  Maybe (solns,lst)
	with Contradiction -> NoSolns in
      if C.debug then begin
	eprintf "Solutions: %s\n" (pp_answer sol) ; flush stderr
      end ;
      sol

(*********************************)
(* Topological sort-based solver *)
(*********************************)

  end
