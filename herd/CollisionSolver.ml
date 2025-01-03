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

  (* Pointer Authentication collision solver state *)
  type solver_state

  val init_solver : solver_state

  type solution

  type answer =
    | NoSolns
    | Maybe of solution *  cnstrnts * solver_state

  val pp_answer : answer -> string

(* Extract delayed exception, if present, or warning, if present. *)
  val get_failed :  cnstrnts -> cnstrnt option

  val solve : solver_state -> cnstrnt list -> answer
end

module type Config = sig
  val hexa : bool
  val debug : Debug_herd.t
  val keep_failed_as_undetermined : bool
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
    module PAC = Constant.PAC

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
      if debug_solver then begin
       eprintf "* Normalizes to *\n%s\n%!" (pp_cnstrnts cns)
      end ;
      m,cns

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

(**************************************)
(* Construct the topological order    *)
(**************************************)


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

    type solution = V.solution

    type solver_state =
      { solver: PAC.solver_state (* Collision solver *)
      ; solution: cst V.Solution.t (* Current variable assignation to constants *)
    }

    type answer =
      | NoSolns
      | Maybe of solution * cnstrnts * solver_state

    let init_solver = {
      solver= PAC.empty_solver;
      solution= V.Solution.empty;
    }

    type 'a solver_monad =
      solver_state -> (solver_state * 'a) list

    (* Return the current value of the PAC collision solver *)
    let get_solver : PAC.solver_state solver_monad =
      fun st -> [st,st.solver]

    (* Return the current assignation of variables *)
    let get_solution : cst V.Solution.t solver_monad =
      fun st -> [st,st.solution]

    let set_solver : PAC.solver_state -> unit solver_monad =
      fun solver st -> [{st with solver},()]

    let set_solution : cst V.Solution.t -> unit solver_monad =
      fun solution st -> [{st with solution},()]

    let (let*) (x: 'a solver_monad) (f: 'a -> 'b solver_monad) : 'b solver_monad =
      fun st ->
        List.concat
          (List.map (fun (st, y) -> f y st) (x st))

    let map (f: 'a -> 'b) (x: 'a solver_monad) : 'b solver_monad =
      fun st ->
        List.map (fun (st, y) -> (st, f y)) (x st)

    let (let+) x f = map f x

    let pure : 'a -> 'a solver_monad = fun x st -> [st,x]

    let contradiction : 'a solver_monad = fun _ -> []

    (* Split the execution in two parts *)
    let alternative (f: 'a solver_monad) (g: 'a solver_monad) :
      'a solver_monad = fun st ->
        List.concat [f st; g st]

    (* Assume that two constants are equals *)
    let assume_equality x y : unit solver_monad =
      let* solver = get_solver in
      match Constant.collision x y with
      | Some (px,py) -> begin
        match PAC.add_equality px py solver with
        | Some solver -> set_solver solver
        | None -> contradiction
      end
      | None ->
          if V.Cst.eq x y
          then pure ()
          else contradiction


    (* Assert the presence or abscence of a collision *)
    let collision x y (vtrue:'a) (vfalse:'a) : 'a solver_monad =
      let* solver = get_solver in
      match PAC.add_equality x y solver,PAC.add_inequality x y solver with
      | Some s1,Some s2 ->
          alternative
            (let+ _ = set_solver s1 in vtrue)
            (let+ _ = set_solver s2 in vfalse)
      | None,Some s2 ->
          let+ _ = set_solver s2 in vfalse
      | Some s1,None ->
          let+ _ = set_solver s1 in vtrue
      | None,None -> assert false

    (* Add a new solution into the solver state, and resolve associated PAC
       collision contraints *)
    let add_solution (x: V.csym) (cst: cst) : unit solver_monad =
      let* solution = get_solution in
      try begin
        let cst' = V.Solution.find x solution in
        assume_equality cst cst'
      end with Not_found -> begin
        set_solution (V.Solution.add x cst solution)
      end

    (* Process an assignation of a variable and a constant *)
    let process_assignation (v: V.v) (cst: cst) : unit solver_monad =
      match v with
      | V.Var x ->
          add_solution x cst
      | V.Val cst' ->
          assume_equality cst cst'

    (* Substitute a value by it's assiciated constant if it correspond to a
       solved variable *)
    let subst_value : V.v -> V.v solver_monad = fun v ->
      let+ solution = get_solution in
      V.map_csym (fun x ->
        try V.Val (V.Solution.find x solution)
        with Not_found -> V.Var x) v

    (* Substitute all the variables (csym) in an expression by their value in
     * the current solution *)
    let subst_expr : expr -> expr solver_monad = fun expr ->
      let+ solution = get_solution in
      map_expr (V.map_csym (fun x ->
        try V.Val (V.Solution.find x solution)
        with Not_found -> V.Var x
      )) expr

    (* Result of the evaluation of an expression *)
    type eval_result
      = Cst of cst
        (* The evaluation is a success and return a constant *)
      | Undetermined
        (* The evaluation failed because some variables are undetermined *)
      | Failure of exn
        (* The evaluation failed because of a delayed exception *)

    (* Evaluate a value of type V.v *)
    let eval_value : V.v -> eval_result = function
      | V.Var _ -> Undetermined
      | V.Val c -> Cst c

    let eval_expr : expr -> eval_result solver_monad = fun expr ->
      let* expr = subst_expr expr in
      try
        pure (eval_value (match expr with
        | Atom v ->
            v
        | ReadInit (loc,init) ->
            A.look_address_in_state init loc
        | Unop (op, v) ->
            V.op1 op v
        | Binop (op, v1, v2) ->
            V.op op v1 v2
        | Terop (op, v1, v2, v3) ->
            V.op3 op v1 v2 v3))
      with
      | V.CollisionPAC (px, py, vtrue, vfalse) ->
          let+ v = collision px py vtrue vfalse in
          eval_value v
      | V.Undetermined | A.LocUndetermined ->
          pure Undetermined
      | exn ->
          pure (Failure exn)


(**************************************)
(* Second phase: topological order *)
(**************************************)

    (* Sort the SCC (Strongly connected component) in topological order *)
    let topo_order (constraints: cnstrnts) : cnstrnts list =
      let ns,r = eq2g constraints in
      List.rev (EqRel.scc_kont List.cons [] ns r)

    (* Try to solve one constraint and return the list of unsolved constraints *)
    let solve_one : cnstrnt -> cnstrnts solver_monad = function
      | Assign (v, expr) -> begin
        let* v = subst_value v in
        let* expr = subst_expr expr in
        let* result = eval_expr expr in

        match result with
        | Cst cst ->
            let+ _ = process_assignation v cst in []
        | Undetermined ->
            pure [Assign (v, expr)]
        | Failure exn ->
            pure [Failed exn]
      end
      | Warn str -> pure [Warn str]
      | Failed exn -> pure [Failed exn]

    (* Attemp to solve a SCC *)
    let rec solve_scc : cnstrnts -> cnstrnts solver_monad = function
      | c :: cs ->
          let* l1 = solve_one c in
          let+ l2 = solve_scc cs in
          l1 @ l2
      | [] ->
          pure []

    (* Solve an ordered list of SCC *)
    let rec solve_many : cnstrnts list -> cnstrnts solver_monad = function
      | c :: cs ->
          let* l1 = solve_scc c in
          let+ l2 = solve_many cs in
          l1 @ l2
      | [] ->
          pure []

    let solve_topo (state: solver_state) (constraints : cnstrnts) : answer =
      let m,constraints = normalize_vars constraints in
      if debug_solver then
        Printf.printf "*** Solve ***\n%s\n" (pp_cnstrnts constraints);
      let constraints = topo_order constraints in
      if debug_solver then
        Printf.printf "*** Ordered ***\n%s\n" (pp_cnstrnts (List.concat
        constraints));
      match solve_many constraints state with
      | (state, constraints) :: _ as solutions -> begin
          if debug_solver then begin
            Printf.printf "found %d solutions\n%s" (List.length solutions)
            (PAC.pp_solver state.solver)
          end;
          let solution = add_vars_solns m state.solution in
          Maybe (solution, constraints, state)
      end
      | _ ->
          NoSolns

    let get_failed cns =
      List.fold_left
        (fun r cn ->
          match cn,r with
          | Failed _,_ -> Some cn
          | Warn _,None -> Some cn
          | (Assign _,_)|(Warn _,Some _) -> r)
        None cns

    let pp_answer =
      let pp_cns cns = match cns with
      | [] -> ""
      | _::_ ->
          "\nUnsolved equations:\n" ^
          (pp_cnstrnts cns) in

      fun soln -> match soln with
      | NoSolns -> "No solutions"
      | Maybe (sol,cns, _) ->
          let sol_pped =
            let bds =
              V.Solution.fold
                (fun v i k -> (v,i)::k)
                sol [] in
            String.concat ", "
              (List.map
                 (fun (v,i) -> V.pp_csym v ^ "<-" ^ V.pp C.hexa i) bds) in

          sol_pped ^ pp_cns cns


    let solve state cs =
      let answer = solve_topo state cs in
      if debug_solver then Printf.printf "Answer: %s\n" (pp_answer answer);
      answer
  end
