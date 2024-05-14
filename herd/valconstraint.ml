(****************************************************************************)
(*                           The diy toolsuite                              *)
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

(** A simple constraint solver. The default solver
 * [solve_topo] proceeds by one pass by following dependencies, while
 * the previous solver [solve_std] proceeds by iterating substitution
 * and computation steps untill stabilisation.
 * Both solvers are invoked after a "normalisation" step that
 * identifies classes of equivant variables resulting from equations
 * of the form [x := y], see [normalise_vars] below.
 *)

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
  type arch_op
  type arch_op1

  type expr =
    | Atom of atom
    | ReadInit of location * state
    | Unop of (arch_op1 Op.op1) * atom
    | Binop of (arch_op Op.op) * atom * atom
    | Terop of Op.op3 * atom * atom * atom


  type rvalue = expr

  type cnstrnt =
    | Assign of atom * rvalue
    | Failed of exn (* Delay exceptions *)
    | Warn of string

  type cnstrnts = cnstrnt list
  val pp_cnstrnts : cnstrnt list -> string

  type solution

  type answer =
    | NoSolns
    | Maybe of solution *  cnstrnts

  val pp_answer : answer -> string

(* Extract delayed exception, if present, or warning, if present. *)
  val get_failed :  cnstrnts -> cnstrnt option

  val solve : cnstrnt list -> answer
end

module type Config = sig
  val hexa : bool
  val debug : Debug_herd.t
  val keep_failed_as_undetermined : bool
  val old_solver : bool
end

module Make (C:Config) (A:Arch_herd.S) : S
with type atom = A.V.v
and type cst = A.V.Cst.v
and type arch_op = A.V.arch_op
and type arch_op1 = A.V.arch_op1
and type solution = A.V.solution
and type location = A.location
and type state = A.state =
  struct

    let debug_solver = C.debug.Debug_herd.solver

    open Printf
    module V = A.V

    type atom = V.v
    type cst = V.Cst.v
    type location = A.location
    type state = A.state
    type arch_op = V.arch_op
    type arch_op1 = V.arch_op1

    type expr =
      | Atom of atom
      | ReadInit of location * state
      | Unop of V.op1_t * atom
      | Binop of V.op_t * atom * atom
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
      | Failed of exn
      | Warn of string

    type cnstrnts = cnstrnt list

    let pp_atom a = V.pp C.hexa a

    let pp_expr e =
      match e with
      | Atom a -> pp_atom a
      | ReadInit(loc,_) -> A.dump_location loc ^ " in init"
      | Unop (o,a1) ->
          sprintf "%s(%s)"
            (Op.pp_op1 C.hexa V.pp_arch_op1 o) (pp_atom a1)
      | Binop (o,a1,a2) ->
          if Op.is_infix o then
            pp_atom a1 ^ Op.pp_op o V.pp_arch_op ^ pp_atom a2
          else
            Printf.sprintf "%s(%s,%s)"
              (Op.pp_op o V.pp_arch_op) (pp_atom a1) (pp_atom a2)
      | Terop (op,a1,a2,a3) ->
          Op.pp_op3 op
            (pp_atom a1) (pp_atom a2) (pp_atom a3)

    let pp_rvalue e = pp_expr e

    let pp_cnstrnt cnstr =  match cnstr  with
      | Assign (v,rval) ->
          (V.pp C.hexa v) ^ ":=" ^(pp_rvalue rval)
      | Failed e  -> sprintf "Failed %s" (Printexc.to_string e)
      | Warn e  -> e

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

    let fold_var f = function
      | V.Val _ -> Fun.id
      | V.Var x -> f x

    let fold_loc f loc =
      match A.undetermined_vars_in_loc_opt loc with
      | None -> Fun.id
      | Some v -> fold_var f v

    let fold_vars_expr f e t = match e with
      | Atom v ->  fold_var f v t
      | ReadInit (loc,_) -> fold_loc f loc t
      | Unop (_,v) -> fold_var f v t
      | Binop (_,v1,v2) ->
         fold_var f v1 t |> fold_var f v2
      | Terop (_,v1,v2,v3) ->
         fold_var f v1 t |> fold_var f v2  |> fold_var f v3

    let add_vars_expr = fold_vars_expr Part.add
    and add_var = fold_var Part.add

    let add_vars_cn t cn = match cn with
    | Assign (v,e) ->
        add_var v t |> add_vars_expr e
    | Failed _ | Warn _ -> t

    let add_vars_cns cns = List.fold_left add_vars_cn (Part.create ()) cns

(* Perform union-find *)

    let uf_cn t cn = match cn with
    | Assign (V.Var v,Atom (V.Var w)) -> Part.union t v w
    | _ -> ()

    let uf_cns t cns =
      List.iter (uf_cn t) cns ;
      Part.as_solution t

(* Simplify equations *)

    let subst_atom m v =
      V.map_csym
        (fun x ->
          try V.Var (Part.Sol.find x m)
          with Not_found -> V.Var x)
        v
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
    | Failed _ | Warn _ -> cn::k

    let subst_cns soln cns = List.fold_right (subst_cn soln) cns []


(* All together *)

    (** [normalise cns], where [cns] is a list of equations,
     * that is, compute equivalence classes of variables and
     * replace variables by their representative.
     * The function returns a pair [(m,cns)], where cns collects
     * the new equations, with equations [x := y] removed and
     * variables replaced by representative. The mapping [m]
     * is from variables to representative.
     *)
    let normalize_vars cns =
      let t = add_vars_cns cns in
      let m = uf_cns t cns in
      let cns = subst_cns m cns in
      if false && debug_solver then begin
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
      with (* [expr] still contains at least one undetermined sub-expression *)
      | A.LocUndetermined
      | V.Undetermined -> e


    let check_true_false cn k = match cn with
    | Assign (v,e) ->
       begin
         try
           let e = mk_atom_from_expr e in
           begin match e with
           | Atom w ->
              if V.is_var_determined v && V.is_var_determined w then
                if V.compare v w = 0 then k
                else raise Contradiction
              else
                Assign (v,e)::k
           | ReadInit _| Unop _|Binop _|Terop _ ->
              Assign (v,e)::k
           end
         (* Delay failure to preserve potential contradiction *)
         with
         | Contradiction|Misc.Timeout as e -> raise e
         | e ->
            if C.debug.Debug_herd.exc then raise e
            else if C.keep_failed_as_undetermined then cn :: k
            else
              let () =
                if debug_solver then begin
                  eprintf "Solving %s\n" (pp_cnstrnt cn) ;
                  eprintf "Delaying exception in solver: %s\n%!"
                    (Printexc.to_string e)
                end in
              Failed e :: k
       end
    | Failed _ | Warn _ -> cn::k

    let check_true_false_constraints cns =
      List.fold_right check_true_false cns []

(* Phase 3, substitution *)

    let simplify_vars_in_var soln x =
      try V.Val (V.Solution.find x soln)
      with Not_found -> V.Var x

    let simplify_vars_in_atom soln v =
      V.map_csym (simplify_vars_in_var soln) v

    let simplify_vars_in_expr soln = map_expr (simplify_vars_in_atom soln)

    let simplify_vars_in_cnstrnt soln cn =
      match cn with
      | Assign (v,rval) ->
          let v = simplify_vars_in_atom soln v in
          let rval = simplify_vars_in_expr soln rval in
          Assign (v,rval)
      | Failed _ | Warn _ -> cn

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
    | Failed _ | Warn _ -> empty

(* merge of solutions, with consistency check *)
    let add_sol x cst sol =
      try
        let cst' = V.Solution.find x sol in
        if V.Cst.eq cst cst' then sol
        else raise Contradiction
      with
      | Not_found -> V.Solution.add x cst sol

    let merge sol1 sol2 = V.Solution.fold add_sol sol1 sol2

    let solve_cnstrnts =
      List.fold_left
        (fun solns cnstr -> merge (solve_cnstrnt cnstr) solns)
        V.Solution.empty

(************************)
(* Raise exceptions now *)
(************************)

let get_failed cns =
  List.fold_left
    (fun r cn ->
      match cn,r with
      | Failed _,_ -> Some cn
      | Warn _,None -> Some cn
      | (Assign _,_)|(Warn _,Some _) -> r)
    None cns

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
      if V.Solution.is_empty solns then begin
        solns_final,cns
      end else
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

    let solve_std lst =
      if debug_solver then begin
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
      if debug_solver then begin
        eprintf "Solutions: %s\n" (pp_answer sol) ; flush stderr
      end ;
      sol

(*********************************)
(* Topological sort-based solver *)
(*********************************)

    module OrderedEq = struct
      type t = cnstrnt

      let atom_compare = A.V.compare

      let atom2_compare p1 p2 =
        Misc.pair_compare atom_compare atom_compare p1 p2

      let atom3_compare (e1,e2,e3) (f1,f2,f3) =
        Misc.pair_compare
          atom_compare atom2_compare (e1,(e2,e3)) (f1,(f2,f3))

      let expr_compare e1 e2 = match e1,e2 with
        | Atom v1,Atom v2 -> atom_compare v1 v2
        | ReadInit (loc1,_),ReadInit (loc2,_) ->
           A.location_compare loc1 loc2 (* second componant is fixed *)
        | Unop (op1,e1),Unop (op2,e2) ->
           Misc.pair_compare
             Misc.polymorphic_compare
             atom_compare
             (op1,e1) (op2,e2)
        | Binop (o,e1,e2),Binop (p,f1,f2) ->
           Misc.pair_compare
             Misc.polymorphic_compare
             atom2_compare
             (o,(e1,e2)) (p,(f1,f2))
        | Terop (o,e1,e2,e3),Terop (p,f1,f2,f3) ->
           Misc.pair_compare
             Misc.polymorphic_compare
             atom3_compare
             (o,(e1,e2,e3)) (p,(f1,f2,f3))
        | (Atom _,(ReadInit _|Unop _|Binop _|Terop _))
        | (ReadInit _,(Unop _|Binop _|Terop _))
        | (Unop _,(Binop _|Terop _))
        | (Binop _,Terop _)
          -> -1
        | ((ReadInit _|Unop _|Binop _|Terop _),Atom _)
        | ((Unop _|Binop _|Terop _),ReadInit _)
        | ((Binop _|Terop _),Unop _)
        | (Terop _,Binop _)
          -> 1

      let compare c1 c2 = match c1,c2 with
        | Assign (v1,e1),Assign (v2,e2) ->
           Misc.pair_compare
             atom_compare
             expr_compare
             (v1,e1) (v2,e2)
        | Failed exn1,Failed exn2 ->
           Misc.polymorphic_compare exn1 exn2
        | Warn w1,Warn w2 ->
           String.compare w1 w2
        | (Assign _,(Failed _|Warn _))
        | (Failed _,Warn _)
          -> -1
        | ((Failed _|Warn _),Assign _)
          | (Warn _,Failed _)
          -> 1
    end

    module EqSet = MySet.Make(OrderedEq)

    module VarEnv = A.V.Solution

    let env_find csym m =
      try VarEnv.find csym m with Not_found -> EqSet.empty

    let env_add csym c =
      VarEnv.update csym @@
        function
        | None -> Some (EqSet.singleton c)
        | Some old -> Some (EqSet.add c old)

    let var2eq cs =
      (* Construct the map from x to all equations of the form [x = <e>] *)
      List.fold_left
        (fun m c ->
          match c with
          | Assign (V.Var csym,_) -> env_add csym c m
          | Assign (V.Val _,_)|Warn _|Failed _ -> m)
       VarEnv.empty cs

    module EqRel = InnerRel.Make(OrderedEq)

    let debug_topo chan ns r =
      EqRel.scc_kont
        (fun cs () ->
          Printf.fprintf chan "{%s}\n%!"
            (List.map pp_cnstrnt cs |> String.concat ", "))
        ()
        ns r

    let eq2g cs =
      let cs =
        List.map
          (fun c ->
            match c with
            | Assign (V.Val _ as c,Atom (V.Var _ as y)) ->
               Assign (y,Atom c)
            | _ -> c)
          cs in
      let m = var2eq cs in
      let add_rels eq0 e g =
        let add_rel csym g =
          let eqs = env_find csym m in
          EqSet.fold (fun eq g -> EqRel.add (eq0,eq) g) eqs g in
        fold_vars_expr add_rel e g in

      let rel =
        List.fold_left
          (fun rel c ->
            match c with
            | Assign (_,e)  -> add_rels c e rel
            | Warn _|Failed _ -> rel)
          EqRel.empty cs in
      let cs = EqSet.of_list cs in
      cs,rel

    (** [solv_one c sol eqs], where c is an equation, [sol] is a solution
     *   (map from variables to constants) and [eqs] is a list of equations,
     *   evaluates the equation [c] w.r.t. to solution [sol]
     *   and returns [(sol,eqs)] updated, with:
     *     - [sol] updated to add all the variable affections found;
     *     - [eqs] updated to add the unsolved equations.
     *)
    let solve_one c sol eqs =
      match c with
      | Warn _|Failed _ -> sol,c::eqs
      | Assign (v0,e) ->
         begin
           try
             let v = simplify_vars_in_atom sol v0
             and e = simplify_vars_in_expr sol e |> mk_atom_from_expr in
             match v,e with
             | V.Var x,Atom (V.Val atom) ->
                add_sol x atom sol,eqs
             | V.Val c1,Atom (V.Val c2) ->
                if V.Cst.eq c1 c2 then sol,eqs
                else raise Contradiction
             (* Last case below can occur when called on a
                strongly connected component. *)
             | _,_ -> sol,Assign (v,e)::eqs
           with
           | Contradiction|Misc.Timeout as exn -> raise exn
           | exn ->
              if C.debug.Debug_herd.exc then raise exn ;
              (sol,Failed exn::eqs)
         end

    let topo_step cs (sol,eqs) =
      match cs with
      | [] -> assert false
      | [c] -> solve_one c sol eqs
      | scc ->
         (* Attempt to partial solve *)
         List.fold_left
           (fun (sol,scc) c -> solve_one c sol scc)
           (sol,eqs) scc

    (** [solve_top_step [cs] tries to solve the system [cs] by sorting [cs]
      * topologically, returns [(sol,cs,sccs)], where
      *   - [sol] is the "solution" resulting from the propagation of
      *     solved equations x = cst;
      *   - [cs] are fake equations such as delayed warnings;
      *   - [sccs] are unsolved recusive equations at the end.
      * Raises `Contradiction` in case solving equations results in
      * some contradictory equation cst = cst`
      *)
    let solve_topo_step cs =
      let ns,r = eq2g cs in
      if debug_solver then begin
        if false then begin
          prerr_endline "** Solve topo **" ;
          eprintf "%s\n%!" (pp_cnstrnts cs) ;
          prerr_endline "** Graph **" ;
          EqRel.pp stderr ""
            (fun chan (c1,c2) ->
              fprintf chan "(%s) <- (%s)\n"
                (pp_cnstrnt c1) (pp_cnstrnt c2))
            r
        end ;
(*
        eprintf "** Equations **\n%!" ;
        eprintf "%s\n" (pp_cnstrnts cs) ; flush stderr ;
*)
        eprintf "** Equations ordered**\n%!" ;
        debug_topo stderr ns r
      end ;
      EqRel.scc_kont topo_step (V.Solution.empty,[]) ns r

    let solve_topo cs =
      (* Replace equivalent variables by a class representative *)
      let m,cs = normalize_vars cs in
      let sol =
        try
          (* Solve in one scan *)
          let sol,cs = solve_topo_step cs in
          (* Add solutions of the form x := y *)
          let sol = add_vars_solns m sol in
          Maybe (sol,cs)
        with
        | Contradiction -> NoSolns in
      if debug_solver then begin
          eprintf "Solutions: %s\n" (pp_answer sol) ; flush stderr
        end ;
      sol

    let solve cs =
      if C.old_solver then
        solve_std cs
      else
        solve_topo cs

  end
