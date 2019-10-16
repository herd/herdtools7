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

module type S =  sig
  type elt0

  module Elts : MySet.S with type elt = elt0

  include Rel.S with
  type elt1 = elt0 and type elt2 = elt0
  and module Elts1 = Elts
  and module Elts2 = Elts

(* All elements related *)
  val nodes : t -> Elts.t

(* Inverse *)
  val inverse : t -> t

(* Set to relation *)
  val set_to_rln : Elts.t -> t

(* Are e1 and e2 related by the transitive closure of relation.
   Does not detect cycles *)
  val mem_transitive : elt1 * elt2 -> t -> bool

(* Nodes reachable from node and set of nodes [argument included in result] *)
  val reachable : elt0 -> t -> Elts.t
  val reachable_from_set : Elts.t -> t -> Elts.t

(* One path from one node to another, returns [] if none *)
  val path : elt0 -> elt0 -> t -> elt0 list
(* All leaves *)
  val leaves : t -> Elts.t
(* All leaves reachable from node *)
  val leaves_from : elt0 -> t -> Elts.t
(* All roots, ie all nodes with no predecessor *)
  val roots : t -> Elts.t

(* Idem backwards *)
  val up :  elt0 -> t -> Elts.t
  val up_from_set : Elts.t -> t -> Elts.t

(* Does not detect cycles either *)
  val transitive_closure : t -> t

(* Direct cycles *)
  val is_reflexive : t -> bool
  val is_irreflexive : t -> bool

(* Explicit acyclicity check,  cost (one dfs) is neglectible
   w.r.t. transitive closure *)
  val get_cycle : t ->  elt0 list option
  val is_acyclic : t -> bool
  val is_cyclic : t -> bool

(* Transformation 'order' like lists into relations *)
  (* without transitive closure *)
  val order_to_succ : elt0 list -> t
  (* with transitive closure *)
  val order_to_rel : elt0 list -> t

(* Also for cycles *)
  val cycle_to_rel : elt0 list -> t
  val cycle_option_to_rel : elt0 list option -> t

  exception Cyclic
(* Topological sort *)
  val topo :  Elts.t -> t -> elt0 list


(****************************************************)
(* Continuation based all_topos (see next function) *)
(****************************************************)

(* Orders as a lists *)
  val all_topos_kont :  Elts.t -> t -> (elt0 list -> 'a -> 'a) -> 'a -> 'a
(* Orders as relations *)
  val all_topos_kont_rel :
      Elts.t -> t -> (t -> 'a) -> (t -> 'a -> 'a) -> 'a -> 'a

(* All toplogical orders, raises Cyclic in case of cycle
  Enhancement: all_topos nodes edges still works
  when edges relates elts not in nodes *)


  val all_topos : bool (* verbose *)-> Elts.t -> t -> elt0 list list

(* Is the parent relation of a hierarchy *)
  val is_hierarchy : Elts.t -> t -> bool

(* Remove any transitivity edges
   [LUC: set argument. removed, it is useless, since set = nodes rel is ok]
   remove_transitive_edges [set] rel
      [assumes rel \in set \times set]
      returns rel' \subset rel such that rel'
        does not have (e1, e2) if it has both (e1, e3) and (e3, e2),
        but transitive_closure rel' = transitive_closure rel
*)
  val remove_transitive_edges : t -> t

(* Sequence composition of relation *)
  val sequence : t-> t -> t
  val sequences : t list -> t

(* Equivalence classes, applies to equivalence relations only (unchecked) *)
  val classes : t -> Elts.t list

(* strata ie sets of nodes by increasing distance *)
  val strata : Elts.t -> t -> Elts.t list
end

module Make(O:MySet.OrderedType) : S
with type elt0 = O.t
and module Elts = MySet.Make(O) =
  struct

    type elt0 = O.t


    module Elts = MySet.Make(O)

    include Rel.Make(O)(O)


    let nodes t =
      let xs =
        fold (fun (x,y) k -> Elts.add x (Elts.add y Elts.empty)::k) t [] in
      Elts.unions xs



(* Inverse *)
    let inverse t = fold (fun (x,y) k -> add (y,x) k) t empty

(* Set to relation *)
    let set_to_rln s = Elts.fold (fun x k -> add (x,x) k) s empty

(* Internal tranformation to successor map *)

(*********************************************)
(* Transitive closure does not detect cycles *)
(*********************************************)

    exception Found

    let is_reachable r e1 e2 =
      let rec dfs e seen =
        if O.compare e e2 = 0 then raise Found
        else if Elts.mem e seen then seen
        else
          Elts.fold
            dfs (succs r e) (Elts.add e seen) in
      (* dfs e1 Elts.empty would yield reflexive-transitive closure *)
      Elts.fold dfs (succs r e1) (Elts.singleton e1)

    let mem_transitive (e1, e2) r =
      try ignore (is_reachable r e1 e2) ; false
      with Found -> true

    let reachable_from_set start r =
      let rec dfs e seen =
        if Elts.mem e seen then seen
        else
          Elts.fold dfs (succs r e) (Elts.add e seen) in
      Elts.fold dfs start Elts.empty

    let reachable e r = reachable_from_set (Elts.singleton e) r

    exception Path of elt0 list

    let path e1 e2 t =
      let rec dfs e seen =
        if Elts.mem e seen then seen
        else
          Elts.fold
            (fun e seen ->
              if O.compare e e2 = 0 then raise (Path [e]) ;
              try dfs e seen
              with Path p -> raise (Path (e::p)))
            (succs t e)
            (Elts.add e seen) in
      try
        ignore (dfs e1 Elts.empty) ;
        []
      with Path es -> e1::es

    let leaves t =
      let all_nodes = nodes t in
      let non_leaves =
        Elts.of_list (fold (fun (e,_) k -> e::k) t []) in
      Elts.diff all_nodes non_leaves

    let leaves_from e t =
      let rec dfs e (leaves,seen as r) =
        if Elts.mem e seen then r
        else
          let es = succs t e in
          if Elts.is_empty es then
            Elts.add e leaves, Elts.add e seen
          else Elts.fold dfs es (leaves,Elts.add e seen) in
      let leaves,_ = dfs e (Elts.empty,Elts.empty) in
      leaves

    let roots t =
      let all_nodes = nodes t in
      let non_roots =
        Elts.of_list (fold (fun (_,e) k -> e::k) t []) in
      Elts.diff all_nodes non_roots


(* Back (and up) *)
    let up_from_set start r =
      let rec dfs e seen =
        if Elts.mem e seen then seen
        else
          Elts.fold dfs (preds r e) (Elts.add e seen) in
      Elts.fold dfs start Elts.empty

    let up e r = up_from_set (Elts.singleton e) r

(* Reflexivity check *)
    let is_reflexive r = exists (fun (e1,e2) -> O.compare e1 e2 = 0) r

    let is_irreflexive r = not (is_reflexive r)

(* Some operations can be accerelated with maps *)
    module M = struct
      module ME = Map.Make(O)

      let succs e m = try ME.find e m with Not_found -> Elts.empty

      let add x y m = ME.add x (Elts.add y (succs x m)) m

      let adds x ys m = ME.add x (Elts.union ys (succs x m)) m

      let to_map_ok p r =
        fold
          (fun (e1,e2) m -> if p e1 e2 then add e1 e2 m else m)
          r ME.empty

      let to_map r = to_map_ok (fun _ _ -> true) r

      let of_map m =
        let xs =
          ME.fold
            (fun e1 es k -> of_succs e1 es::k)
            m [] in
        unions xs

(* Transitive closure the naive way,
   not significantly worse than before... *)

      let rec tr m0 =
        let m1 =
          ME.fold
            (fun x ys m ->
              let zs =
                Elts.fold
                  (fun y k -> succs y m::k)
                  ys [] in
              adds x (Elts.unions zs) m)
            m0 m0 in
        if ME.equal Elts.equal m0 m1 then m0
        else tr m1

(* Acyclicity check *)
      exception Cycle of (Elts.elt list)

      let rec mk_cycle f = function
        | [] -> assert false
        | e::rem ->
            if O.compare f e = 0 then [e]
            else e::mk_cycle f rem

      let get_cycle m =
        let rec dfs path above e seen =
          if Elts.mem e above then
            raise (Cycle (e::mk_cycle e path)) ;
          if Elts.mem e seen then
            seen
          else
            Elts.fold
              (dfs (e::path) (Elts.add e above))
              (succs e m) (Elts.add e seen) in
        try
          let _ =
            ME.fold
              (fun x _ -> dfs [] Elts.empty x) m Elts.empty in
          None
        with Cycle e -> Some (List.rev e)

      let fold = ME.fold

      let reachable e e_succs m =
        let rec dfs e seen =
          if Elts.mem e seen then seen
          else
            Elts.fold dfs (succs e m) (Elts.add e seen) in
        Elts.fold dfs e_succs (Elts.singleton e)

      let cc m =
        let _,ccs =
          ME.fold
            (fun e succs (seen,ccs as r) ->
              if Elts.mem e seen then r
              else
                let cc = reachable e succs m in
                Elts.union cc seen,cc::ccs)
            m (Elts.empty,[]) in
        ccs

      let strata es r =
        let m =
          to_map_ok
            (fun e1 e2 -> Elts.mem e1 es && Elts.mem e2 es) r in
        let nss = ME.fold (fun _ ns k -> ns::k) m [] in
        let st0 = Elts.diff es (Elts.unions nss) in
        if Elts.is_empty st0 then [es]
        else
          let rec do_rec seen st =
            let stplus =
              Elts.diff
                (Elts.unions (Elts.fold (fun e k -> succs e m::k) st []))
                seen in
            if Elts.is_empty stplus then []
            else stplus::do_rec (Elts.union seen stplus) stplus in
          st0::do_rec st0 st0
    end


    let transitive_closure r = M.of_map (M.tr (M.to_map r))



(* Acyclicity check *)

    let get_cycle r = M.get_cycle (M.to_map r)

    let is_acyclic r = match get_cycle r with
    | None -> true
    | Some _ -> false

    let is_cyclic r = not (is_acyclic r)

(* From lists to relations *)

    let rec order_to_succ = function
      | []|[_] -> empty
      | e1::(e2::_ as es) ->
          add (e1,e2) (order_to_succ es)

    let rec order_to_pairs k evts = match evts with
    | [] -> k
    | e1 :: tl ->
        let k = List.fold_left (fun k e2 -> (e1,e2)::k) k tl in
        order_to_pairs k tl

    let order_to_rel es = of_list (order_to_pairs []  es)

    let cycle_to_rel cy =
      let rec do_rec seen = function
        | [] -> assert false
        | [e] ->
            if Elts.is_empty seen then singleton (e,e)
            else  begin
              assert (Elts.mem e seen) ;
              empty
            end
        | e1::(e2::_ as rem) ->
            if Elts.mem e1 seen then empty
            else
              add (e1,e2) (do_rec (Elts.add e1 seen) rem) in
      do_rec Elts.empty cy

    let cycle_option_to_rel = function
      | None -> empty
      | Some cy -> cycle_to_rel cy


(* Topological sort *)
    exception Cyclic

    let topo all_nodes t =
      let m = M.to_map t in
      let rec dfs above n (o,seen as r) =
        if Elts.mem n above then raise Cyclic
        else if Elts.mem n seen then r
        else
          let o,seen =
            Elts.fold (dfs (Elts.add n above))
              (M.succs n m) (o,Elts.add n seen) in
          n::o,seen in
      let all_succs =
        Elts.unions
          (M.fold (fun _ ns k -> ns::k) m []) in
      let ns = Elts.diff all_nodes all_succs in
      let o,_ =
        Elts.fold
          (dfs Elts.empty) ns ([],Elts.empty) in
      o




(* calculate all topological orderings of
   an acyclic directed graph, method 2
   following http://sunburn.stanford.edu/~knuth/fasc2b.ps.gz
   (found by Gilles).
   Raises Cyclic when the graph is cyclic *)


    let rec do_all_topos nodes edges =
      if Elts.is_empty nodes then
        [[]]
      else
        let n = (* Minimal node (no predecessor) *)
          try Elts.find (fun n -> not (exists_pred edges n)) nodes
          with Not_found -> raise Cyclic in
        let n_succs, others =
          partition (fun (n1,_n2) -> O.compare n n1 = 0) edges in
        let mss = do_all_topos (Elts.remove n nodes) others in
        (*
          find all the legitimate places to insert n in ms,
          ie all the points before m s.t. there is
          an (n,m) edge in g *)
        let rec insert = function
          | [] -> [[n]]
          | m::ms as all ->
              (n::all)::
              (if not (mem (n,m) n_succs) then
                List.rev_map
                  (fun ms -> m::ms)
                  (insert ms)
              else
                []) in
        List.fold_left
          (fun k ms -> ms@k)
          []
          (List.rev_map insert mss)


    let _all_topos1 nodes edges =
      do_all_topos nodes
        (filter
           (fun (e1,e2) -> Elts.mem e1 nodes && Elts.mem e2 nodes)
           edges)

(* New version of all_topos *)
    module EMap =
      Map.Make
        (struct
          type t = O.t
          let compare = O.compare
        end)

    let find_def d k m =
      try EMap.find k m
      with Not_found -> d

    let find_count = find_def 0
    let find_pred = find_def Elts.empty

    let make_count nodes edges =
      fold (fun (n1,n2) m ->
        if Elts.mem n1  nodes &&  Elts.mem n2  nodes then
          EMap.add n1 (find_count n1 m + 1) m
        else m) edges EMap.empty

    let make_preds nodes edges =
      fold
        (fun (n1,n2) m ->
          if Elts.mem n1  nodes &&  Elts.mem n2  nodes then
            EMap.add n2 (Elts.add n1 (find_pred n2 m)) m
          else m)
        edges EMap.empty

    let do_all_mem_topos set_preds kont =

      let rec do_aux ws count_succ pref res =
(* ws is the working set, ie minimal nodes
   count_succ is a map elt -> count of its succs
   set_pred is a map elt -> set of its preds
   pref is the prefix vos being constructed
   res is the list of vos already constructed *)
        if Elts.is_empty ws then
          if EMap.is_empty count_succ then kont pref res
          else raise Cyclic
        else
          Elts.fold
            (fun n res -> (* a maximal node (no successor) *)
              let ws = Elts.remove n ws in
              let n_preds = find_pred n set_preds in
              let count_succ,ws =
                Elts.fold
                  (fun pred (c,ws) ->
                    let p_succ = find_count pred c - 1 in
                    let _ = assert (p_succ >= 0) in
                    if p_succ > 0 then
                      EMap.add pred p_succ c,ws
                    else
                      let c = EMap.remove pred c in
                      let ws = Elts.add pred ws in
                      c,ws)
                  n_preds (count_succ,ws) in
              do_aux ws count_succ (n::pref) res) ws res in
      do_aux

    let fold_topos_ext kont res nodes edges =
      let count_succ = make_count nodes edges in
      let set_preds = make_preds nodes edges in
      let ws =
        Elts.filter (fun n -> find_count n count_succ = 0) nodes in
      do_all_mem_topos set_preds kont ws count_succ [] res

    let all_topos_kont nodes edges kont res =
      fold_topos_ext kont res nodes edges

    let  all_topos_kont_rel es vb kfail kont res =
      try all_topos_kont es vb (fun k res -> kont (order_to_rel k) res) res
      with Cyclic -> kfail vb

    let all_topos2 = fold_topos_ext Misc.cons []

    let all_topos verbose nodes edges =
      let nss = all_topos2 nodes edges in
      if verbose then begin
        let nres = List.length nss in
        if nres > 1023 then begin
          Printf.eprintf "Warning: all topos produced %i orderings\n%!" nres
        end
      end ;
      nss

(* Is the parent relation of a hierarchy *)
    let is_hierarchy nodes edges =
      is_acyclic edges &&
      begin
        let m = M.to_map edges in
        try
          let zero =
            Elts.fold
              (fun e k ->
                match Elts.cardinal (M.succs e m) with
                | 0 -> e::k
                | 1 -> k
                | _ -> raise Exit)
              nodes [] in
          match zero with
          | [] -> Elts.cardinal nodes = 1 && is_empty edges
          | [_] -> true
          | _ -> false
        with Exit -> false
      end


(***************************)
(* Remove transitive edges *)
(***************************)

(*

  The problem is not as simple as removing all edges e1 --> e2
  s.t. there exists e3 with e1 -->+ e3 --->+ e2.

  See for instance
  Vincent Dubois & C\'ecile Bothorel
  "Transitive reduction for social networks and visuallization"
  International conference on web intelligence (WI'05)

  However a very simple algorithm exists.
 *)

    let remove_transitive_edge  r rel =
      let new_rel = remove r rel in
      if mem_transitive r new_rel then new_rel
      else rel

    let remove_transitive_edges rel = fold remove_transitive_edge rel rel

(************)
(* Sequence *)
(************)

    let sequence r1 r2 =
      let m2 = M.to_map r2 in
      fold
        (fun (e1,e2) ->
          Elts.fold
            (fun e3 -> add (e1,e3))
            (M.succs e2 m2))
        r1 empty

    let rec seq_rec rs = match rs with
    | []|[_] as rs -> rs
    | r1::r2::rs -> sequence r1 r2::seq_rec rs

    let rec sequences rs = match rs with
    | [] -> empty
    | [r] -> r
    | _ -> sequences (seq_rec rs)

(* Equivalence classes *)
    let classes r = M.cc (M.to_map r)

(**********)
(* Strata *)
(**********)
    let strata es r = M.strata es r

  end
