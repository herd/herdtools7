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

  val filter_nodes : (elt0 -> bool) -> t -> t
  val map_nodes : (elt0 -> elt0) -> t -> t

(* Inverse *)
  val inverse : t -> t

(* Set to relation *)
  val set_to_rln : Elts.t -> t

(* Are e1 and e2 related by the transitive closure of relation.
   Does not detect cycles *)
  val exists_path : elt1 * elt2 -> t -> bool

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

(* Transitive closure *)
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

(* Topological sort *)
  exception Cyclic
  val topo_kont : (elt0 -> 'a -> 'a) -> 'a -> Elts.t -> t -> 'a
  val topo :  Elts.t -> t -> elt0 list

  val pseudo_topo_kont :  (elt0 -> 'a -> 'a) -> 'a -> Elts.t -> t -> 'a

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

(* Strongly connected compoments, processed in inverse dependency order. *)
  val scc_kont : (elt0 list -> 'a -> 'a) -> 'a -> Elts.t -> t -> 'a


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
  val transitive3 : t -> t
  val sequences : t list -> t

(* Equivalence classes, applies to any relation.
   Behaves as if the argument relation r is first
   transformed into `(r | r^-1)*`.
*)
  val classes : t -> Elts.t list

(* strata ie sets of nodes by increasing distance *)
  val strata : Elts.t -> t -> Elts.t list

(*****************************************************************************)
(* "Bisimulation" w.r.t. relation r, with initial equivalence relation equiv *)
(*****************************************************************************)

(*
   `bisimulation T E0` computes the greater equivalence E such that
   E0 greater than E
   e1 <-E-> e2,  e1 -T-> e1' ==> exists e2' s.t. e2 -T-> e2', e1' <-E-> e2'
   e1 <-E-> e2,  e2 -T-> e2' ==> exists e1' s.t. e1 -T-> e1', e2' <-E-> e1'
*)
  val bisimulation : t (* transition *) -> t (* equivalence *)-> t


  (* Second argument is delimiter (as in String.concat) *)
  val pp :
    out_channel -> string ->
    (out_channel -> (elt0 * elt0) -> unit) -> t -> unit

 (* As above, but sprintf style instead of fprintf style *)
  val pp_str :
    string -> ((elt0 * elt0) -> string) -> t -> string

end

module Make(O:MySet.OrderedType) : S
  with type elt0 = O.t
 and module Elts = MySet.Make(O) = struct
  type elt0 = O.t


  module Elts = MySet.Make(O)

  include Rel.Make(O)(O)


  (* Extended Tarjan algorithm for SCC.
   *  Adaptation from R. Sedgwick's book "Algorithms".
   *  Two computing functions are provided:
   *  - kont : node -> note -> 'a -> 'a
   *  - kont_scc : node -> node list -> 'a -> 'a
   *    The function kont is called for every tree edge discovered by dfs,
   *    while the function kont_scc is called for every strongly connected
   *    component. The first argument is teh "root" of the scc, the second
   *    argument is the scc.
   *    Those functions suffice to compute the transitive closure of
   *    the input graph. See function "tr" below.
   *)

  module NodeMap = Map.Make(O)

  module FullSCC = struct

    type state =
      { id : int;
        visit : int NodeMap.t;
        stack : elt0 list; }

    let rec pop_until n ns =
      match ns with
      | [] -> assert false
      | m::ns ->
          if O.compare n m = 0 then [m],ns
          else
            let ms,ns = pop_until n ns in
            m::ms,ns

    let dfs kont kont_scc m =
      let rec dfs n ((res,s) as r) =
        try NodeMap.find n s.visit,r
        with
        | Not_found ->
            let min = s.id in
            let s = {
              id = min + 1;
              visit = NodeMap.add n min s.visit;
              stack = n :: s.stack;
            } in
            let min,(res,s) as r =
              Elts.fold
                (fun v (m0,r) ->
                   let m1,(res,s) = dfs v r in
                   Misc.min_int m0 m1,(kont n v res,s))
                (succs m n)
                (min,(res,s)) in
            let valk =
              try NodeMap.find n s.visit with Not_found -> assert false in
            if not (Misc.int_eq min valk) then
              r (* n is part of previously returned scc *)
            else
              let scc,stack = pop_until n s.stack in
              let visit =
                List.fold_left
                  (fun visit n -> NodeMap.add n max_int visit)
                  s.visit scc in
              let res = kont_scc n scc res in
              min,(res,{ s with stack; visit; }) in
      dfs

    (* Nodes are implicit keys of map *)
    let scan_map kont kont_scc res m =
      let dfs = dfs kont kont_scc m in
      let (res,_) =
        M.fold
          (fun n _ r -> let _,r = dfs n r in r)
          m
          (res,{id=0; visit=NodeMap.empty; stack=[];} ) in
      res

    (* Graph is given as set of notes + relation *)
    let scan_nodes_rel kont kont_scc res nodes m =
      let dfs = dfs kont kont_scc m in
      let (res,_) =
        Elts.fold
          (fun n r -> let _,r = dfs n r in r)
          nodes
          (res,{id=0; visit=NodeMap.empty; stack=[];} ) in
      res
  end


  (* Nodes operations *)

  let nodes t =
    M.fold (fun x ys k -> Elts.add x ys::k) t [] |> Elts.unions

  let filter_nodes p t = restrict_domains p p t

  let add_set x ys m =
    M.update x
      (function
        | None -> Some ys
        | Some zs -> Some (Elts.union ys zs))
      m

  let map_nodes f m =
    M.fold
      (fun e es k ->
         let e = f e and es = Elts.map f es in
         add_set e es k)
      m empty

  (* Inverse *)
  let inverse t = fold (fun (x,y) k -> add (y,x) k) t empty

  (* Set to relation *)
  let set_to_rln s =
    Elts.fold
      (fun x k -> M.add x (Elts.singleton x) k)
      s empty

  (* Various path functions *)

  exception Found

  let do_is_reachable succs r e1 e2 =
    let rec dfs e seen =
      if O.compare e e2 = 0 then raise Found
      else if Elts.mem e seen then seen
      else
        Elts.fold
          dfs (succs r e) (Elts.add e seen) in
    (* dfs e1 Elts.empty would yield reflexive-transitive closure *)
    Elts.fold dfs (succs r e1) (Elts.singleton e1)

  let is_reachable r e1 e2 = do_is_reachable succs r e1 e2

  let exists_path (e1, e2) r =
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

  (* Leaves and roots *)

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


  (*  Transitive closure by SCC scan. See for instance page 49 of:
   *        NUUTILA, Esko.
   *        Efficient transitive closure computation in large digraphs.
   *        Doctor of technology dissertation,
   *        Helsinki University of technology
   *        1995.
   *)

  let transitive_closure (m:t) =
    let kont n m res =
      M.update n
        (fun o ->
           (let vr = succs res m in
           match o with
           | None -> Elts.add m vr
           | Some vn -> Elts.add m (Elts.union vr vn))
           |> Option.some)
        res

    and kont_scc n scc res =
      match scc with
      | [] -> assert false
      | [_] -> res
      | ms ->
          let vn = try M.find n res with Not_found -> assert false in
          List.fold_left
            (fun res m -> M.add m vn res)
            res ms in
    FullSCC.scan_map kont kont_scc M.empty m

  (* Reflexivity check *)

  let is_reflexive m =
    try
      M.iter
        (fun e es -> if Elts.mem e es then raise Exit)
        m ;
      false
    with Exit -> true

  let is_irreflexive r = not (is_reflexive r)

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
          (succs m e) (Elts.add e seen) in
    try
      let _ =
        M.fold
          (fun x _ -> dfs [] Elts.empty x) m Elts.empty in
      None
    with Cycle e -> Some (List.rev e)

  let is_acyclic m =
    match  get_cycle m with
    | None -> true
    | Some _ -> false

  let is_cyclic m = not (is_acyclic m)

  (* Build relations from orders given as lists *)

  let rec order_to_succ = function
    | []|[_] -> empty
    | e1::(e2::_ as es) ->
        add (e1,e2) (order_to_succ es)

  let order_to_rel =
    let rec do_rec k evts = match evts with
      | [] -> k
      | e :: es ->
          do_rec (M.add e (Elts.of_list es) k) es in
    do_rec empty

  (* Build relation from cycle given as a list *)

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

    let topo_kont kont res all_nodes m =
      let rec dfs above n (o,seen as r) =
        if Elts.mem n above then raise Cyclic
        else if Elts.mem n seen then r
        else
          let res,seen =
            Elts.fold (dfs (Elts.add n above))
              (succs m n) (o,Elts.add n seen) in
          kont n res,seen in
      let ns = all_nodes in
      let o,_seen =
        Elts.fold
          (dfs Elts.empty) ns (res,Elts.empty) in
      (* Some node has not been visited, we have a cycle *)
      o

    let topo all_nodes t = topo_kont Misc.cons [] all_nodes t

    let pseudo_topo_kont kont res all_nodes (m:t) =
      let rec dfs n (res,seen as r) =
        if Elts.mem n seen then r
        else
          let res,seen =
            Elts.fold dfs (succs m n) (res,Elts.add n seen) in
          kont n res,seen in
      (* Search graph from non-successors *)
      let ns = all_nodes in
      let res,_ = Elts.fold dfs ns (res,Elts.empty) in
      res

    (* Enumerate all topological orders *)

    let find_def d k m =
      try NodeMap.find k m
      with Not_found -> d

    let find_count = find_def 0
    let find_pred = find_def Elts.empty

    let make_count nodes edges =
      fold (fun (n1,n2) m ->
        if Elts.mem n1 nodes &&  Elts.mem n2 nodes then
          NodeMap.update n1
            (function
              | None -> Some 1
              | Some c -> Some (c+1))
            m
        else m) edges NodeMap.empty

    let make_preds nodes edges =
      fold
        (fun (n1,n2) m ->
           if Elts.mem n1 nodes &&  Elts.mem n2 nodes then
             NodeMap.update n2
               (function
                 | None -> Some (Elts.singleton n1)
                 | Some vs -> Some (Elts.add n1  vs))
               m
          else m)
        edges NodeMap.empty

    let do_all_mem_topos set_preds kont =

      let rec do_aux ws count_succ pref res =
(* ws is the working set, ie minimal nodes
   count_succ is a map elt -> count of its succs
   set_pred is a map elt -> set of its preds
   pref is the prefix vos being constructed
   res is the list of vos already constructed *)
        if Elts.is_empty ws then
          if NodeMap.is_empty count_succ then kont pref res
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
                      NodeMap.add pred p_succ c,ws
                    else
                      let c = NodeMap.remove pred c in
                      let ws = Elts.add pred ws in
                      c,ws)
                  n_preds (count_succ,ws) in
              do_aux ws count_succ (n::pref) res) ws res in
      do_aux

    let fold_topos_ext kont res nodes edges =
      let edges = transitive_closure edges in
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

    let all_topos verbose nodes edges =
      let nss = fold_topos_ext Misc.cons [] nodes edges in
      if verbose then begin
        let nres = List.length nss in
        if nres > 1023 then begin
          Printf.eprintf "Warning: all topos produced %i orderings\n%!" nres
        end
      end ;
      nss

    (* Scan strongly connected components, in inverse dependency order *)

    let scc_kont kont res nodes edges =
      let kont _ _ r = r
      and kont_scc _ ns r = kont ns r in
      FullSCC.scan_nodes_rel kont kont_scc res nodes edges


    (* Check hierarchy *)

    let is_hierarchy nodes m =
      is_acyclic m &&
      begin
        try
          let zero =
            Elts.fold
              (fun e k ->
                match Elts.cardinal (succs m e) with
                | 0 -> e::k
                | 1 -> k
                | _ -> raise Exit)
              nodes [] in
          match zero with
          | [] -> Elts.cardinal nodes = 1 && M.is_empty m
          | [_] -> true
          | _ -> false
        with Exit -> false
      end

    (*
     * Remove transitivity
     * The problem is not as simple as removing all edges e1 --> e2
     *  s.t. there exists e3 with e1 -->+ e3 --->+ e2.
     *
     *  See for instance
     *  Vincent Dubois & C\'ecile Bothorel
     * "Transitive reduction for social networks and visuallization"
     * International conference on web intelligence (WI'05)
     *
     * However a very simple algorithm exists.
     *)

    let remove_transitive_edge  r rel =
      let new_rel = remove r rel in
      if exists_path r new_rel then new_rel
      else rel

    let remove_transitive_edges rel = fold remove_transitive_edge rel rel

    (* Sequence *)

    let sequence m1 m2 =
      M.fold
        (fun x ys k ->
           let zss =
             Elts.fold
               (fun y k -> succs m2 y::k)
               ys [] in
           M.add x (Elts.unions zss) k)
        m1 empty

    let transitive3 m = sequence m @@ sequence m m

    let rec seq_rec rs = match rs with
    | []|[_] as rs -> rs
    | r1::r2::rs -> sequence r1 r2::seq_rec rs

    let rec sequences rs = match rs with
    | [] -> empty
    | [r] -> r
    | _ -> sequences (seq_rec rs)

    (* Equivalence classes *)

    let cc m =

      let reachable e e_succs =
        let rec dfs e seen =
          if Elts.mem e seen then seen
          else
            Elts.fold dfs (succs m e) (Elts.add e seen) in
        Elts.fold dfs e_succs (Elts.singleton e) in

      let _,ccs =
        M.fold
          (fun e succs (seen,ccs as r) ->
             if Elts.mem e seen then r
           else
             let cc = reachable e succs in
             Elts.union cc seen,cc::ccs)
          m (Elts.empty,[]) in
      ccs

    let classes m = union m (inverse m) |> cc

    (* Strata *)

    let strata es r =
    let m =
      restrict_rel
        (fun e1 e2 -> Elts.mem e1 es && Elts.mem e2 es)
        r in
    let nss = M.fold (fun _ ns k -> ns::k) m [] in
    let st0 = Elts.diff es (Elts.unions nss) in
    if Elts.is_empty st0 then [es]
    else
      let rec do_rec seen st =
        let stplus =
          Elts.diff
            (Elts.unions (Elts.fold (fun e k -> succs m e::k) st []))
            seen in
        if Elts.is_empty stplus then []
        else stplus::do_rec (Elts.union seen stplus) stplus in
      st0::do_rec st0 st0



  (****************)
  (* bisimulation *)
  (****************)

  let ok x y e =  Elts.mem x (succs e y)

  let matches xs ys e =
    Elts.for_all
      (fun x -> Elts.exists (fun y -> ok x y e) ys)
      xs

  let step t e =
    M.fold
      (fun x ys k ->
         let next_x = succs t x in
         Elts.fold
           (fun y k ->
              if O.compare x y = 0 then
                (* Optimisation, when identical will stay in o forever *)
                add (x,y) k
              else
                let next_y = succs t y in
                if
                  matches next_x next_y e &&
                  matches next_y next_x e
                then
                  add (x,y) k
                else k)
           ys k)
      e M.empty

  let rec fix t e =
    let next = step t e in
    if subrel e next then e else fix t next

  let bisimulation t e0 = fix t e0

  (* Pretty print *)

  let pp chan delim pp_elt s =
    let fst = ref true in
    iter
      (fun p ->
         if not !fst then output_string chan delim ;
         pp_elt chan p ;
         fst := false)
      s

  and pp_str delim pp_elt m =
    fold (fun p k -> pp_elt p::k) m []
    |> String.concat delim

end
