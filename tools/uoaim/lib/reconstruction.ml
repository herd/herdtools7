(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Log = (val Logs.src_log (Logs.Src.create "reconstruction") : Logs.LOG)

(** Nondeterminism monad: represents a computation that may produce more than
    one value, or none at all. Lists provide implementations of the
    nondeterministic monad with different representations of a sequence of
    values. *)
module NonDet : sig
  type 'a t

  val empty : 'a t
  val of_list : 'a list -> 'a t

  val enumerate : 'a t -> 'a list
  (** Enumerate all possible values of the input. *)

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  include Util.Applicative with type 'a t := 'a t
end = struct
  include Misc.List

  let pure = singleton
  let enumerate l = l
  let of_list l = l
end

module TraverseNonDet = Util.ListTraversal (NonDet)

module TraverseOption = Util.ListTraversal (struct
  include Misc.Option

  let pure = Option.some
end)

module EffSet = Set.Make (Eff)
module EffMap = Map.Make (Eff)
module C = Constraint

let rec endpoint_combos : 'a list -> ('a * 'a) list =
  let open Misc.List.Syntax in
  function
  | [] -> []
  | x :: xs ->
      let pairs =
        let* y = xs in
        [ (x, y) ]
      in
      pairs @ endpoint_combos xs

let endpoints_of_constr : C.t -> EffSet.t = function
  | C.Set (e, _) -> EffSet.of_list [ e ]
  | C.Rel (e1, _, e2) -> EffSet.of_list [ e1; e2 ]

module G =
  Graph.Make
    (Eff)
    (struct
      type t = Cat.rel_exp

      let pp = Cat.RelExp.pp
    end)

module SPGraph = struct
  module G = G

  let series = Cat.RelExp.seq
  let parallel = Cat.RelExp.inter
  let invert = Cat.RelExp.invert
end

(**********************)
(**** Conjunctions ****)
(**********************)

module Conj : sig
  type 'a t

  val pure : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val empty : 'a t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val to_list : 'a t -> 'a list
end = struct
  include Misc.List

  let pure = singleton
  let empty = []
  let to_list l = l
end

let endpoints_of_conj : C.t Conj.t -> EffSet.t =
 fun conj ->
  Conj.fold_left EffSet.union EffSet.empty (Conj.map endpoints_of_constr conj)

(* Given a list of conjunctions, take the maximal set of effects shared by all
   conjunctions. *)
let common_conjs_endpoints (conj : C.t Conj.t list) : EffSet.t =
  match List.map endpoints_of_conj conj with
  | [] -> raise (Invalid_argument "empty list")
  | x :: xs -> List.fold_left EffSet.inter x xs

let set_exp_of_conj (e : Eff.t) (conj : C.t Conj.t) : Cat.set_exp =
  let sets =
    conj |> Conj.to_list
    |> List.map (function
      | C.Set (e', exp) when Eff.equal e e' -> exp
      | _ -> raise (Invalid_argument "invalid set constraint"))
  in
  Cat.SetExp.inter sets

(************************)
(**** Reconstruction ****)
(************************)

(* Synthesize a cat expression from a conjunction of constraints.
   The process may fail with return value [None] if the set of constraints is
   incomplete. *)
let exp_of_conj ~src_eff ~tgt_eff (conj : C.t Conj.t) : Cat.rel_exp option =
  Log.debug (fun m ->
      m "attempting synthesis of conjunction on endpoints (%a, %a)" Eff.pp
        src_eff Eff.pp tgt_eff);
  let eff_map, graph =
    conj
    |> Conj.fold_left
         (fun (m, g) c ->
           match c with
           | C.Set (s, exp) ->
               ( EffMap.update s
                   (function
                     | None -> Some exp
                     | Some other_exp ->
                         Some (Cat.SetExp.inter [ exp; other_exp ]))
                   m,
                 g )
           | C.Rel (src_eff, exp, tgt_eff) ->
               (m, G.insert g (src_eff, exp, tgt_eff)))
         (EffMap.empty, G.empty)
  in
  let from_set i = Option.map Cat.RelExp.of_set (EffMap.find_opt i eff_map) in
  let module SP = SeriesParallel.Make (struct
    include SPGraph

    let from_set = from_set
  end) in
  let endpoints = [ src_eff; tgt_eff ] in
  let g = SP.reduce ~endpoints graph in
  if EffSet.equal (EffSet.of_list (G.nodes g)) (EffSet.of_list endpoints) then
    match G.edges g with
    | [ (x, lbl, y) ] ->
        let x, lbl, y =
          if Misc.pair_eq Eff.equal Eff.equal (x, y) (src_eff, tgt_eff) then
            (x, lbl, y)
          else (y, Cat.RelExp.invert lbl, x)
        in
        let from_set i = Option.to_list (from_set i) in
        let series = from_set x @ [ lbl ] @ from_set y in
        Some (SPGraph.series series)
    | _ -> None
  else None

let invalid_disj stru =
  let err =
    Format.asprintf "disjunction shape not supported: %a" (Structure.pp C.pp)
      stru
  in
  raise (Invalid_argument err)

(* Turn a constraint structure into a conjunction of constraints.
   The resulting conjunction is generated non-deterministically to account
   for ambiguity that may be introduced by disjunction nodes. *)
let rec to_conjunction : C.t Structure.t -> C.t Conj.t NonDet.t = function
  | Structure.Constr c -> NonDet.pure (Conj.pure c)
  | Structure.And l ->
      TraverseNonDet.traverse to_conjunction l
      |> NonDet.map (fun conjs -> List.fold_left Conj.append Conj.empty conjs)
  | Structure.Or l as stru ->
      let open NonDet.Syntax in
      let* conjs = TraverseNonDet.traverse to_conjunction l in
      let endps = common_conjs_endpoints conjs in
      let* c =
        match EffSet.elements endps with
        | [] -> invalid_disj stru
        | [ e ] ->
            let exps = List.map (set_exp_of_conj e) conjs in
            NonDet.pure (C.Set (e, Cat.SetExp.union exps))
        | es -> (
            let* src_eff, tgt_eff = NonDet.of_list (endpoint_combos es) in
            let constrs =
              TraverseOption.traverse (exp_of_conj ~src_eff ~tgt_eff) conjs
            in
            match constrs with
            | Some exprs ->
                NonDet.pure (C.Rel (src_eff, Cat.RelExp.union exprs, tgt_eff))
            | None -> NonDet.empty)
      in
      NonDet.pure (Conj.pure c)

let exp_of_structure ~src_eff ~tgt_eff : C.t Structure.t -> Cat.rel_exp list =
 fun stru ->
  let conj = to_conjunction stru in
  let conjs = NonDet.enumerate conj in
  List.filter_map (exp_of_conj ~src_eff ~tgt_eff) conjs
