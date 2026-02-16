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

module type S = sig

  type e1
  type e2

  module Es1 : MySet.S with type elt = e1
  module Es2 : MySet.S with type elt = e2

  module M : MyMap.S with type key = e1

  include MyRel.S
    with type elt1 = e1 and type elt2 = e2
    and module Elts1 = Es1 and module Elts2 = Es2
    and type t = Es2.t M.t
end

module Make
  (O1:MySet.OrderedType)
  (O2:MySet.OrderedType) : S
with
type e1 = O1.t and type e2 = O2.t
and module Es1 = MySet.Make(O1)
and module Es2 = MySet.Make(O2)
 =
  struct

    type e1 = O1.t
    type e2 = O2.t

    module Es1 = MySet.Make(O1)
    module Es2 = MySet.Make(O2)

    type elt1 = e1
    and elt2 = e2

    module Elts1 = Es1
    module Elts2 = Es2


    module M = MyMap.Make(O1)

    type t = Elts2.t M.t

    (* Set of pairs *)

    let o2_equal y1 y2 = O2.compare y1 y2 = 0

    let compare m1 m2 = M.compare Elts2.compare m1 m2
    and equal m1 m2 = M.equal Elts2.equal m1 m2
    and is_empty = M.is_empty
    and mem (x,y) m =
      try Elts2.exists (o2_equal y) @@ M.find x m
      with Not_found -> false

    let empty = M.empty

    let singleton (e1,e2) = M.singleton e1 (Elts2.singleton e2)

    let add (x,y) m =
      M.update x
        (fun ys ->
           (match ys with
            | None -> Elts2.singleton y
            | Some ys -> Elts2.add y ys )
           |> Option.some)
        m

    let of_list ps =  List.fold_left (fun k p -> add p k) empty ps

    let remove (x,y) m =
      M.update x
        (function
          | None -> None
           | Some ys ->
               let ys = Elts2.remove y ys in
               if Elts2.is_empty ys then None
               else Some ys)
        m

    exception Found of (elt1 * elt2)

    let choose m =
      try
        M.iter
          (fun x ys ->
             try raise (Found (x,Elts2.choose ys))
             with Not_found -> ())
          m ;
        raise Not_found
      with Found p -> p

    let cardinal m =
      M.fold
        (fun _ ys k -> Elts2.cardinal ys + k)
        m 0

    let iter f m =
      M.iter (fun x ys ->  Elts2.iter (fun y -> f (x,y)) ys) m

    let fold f m k =
      M.fold
        (fun x ys k -> Elts2.fold (fun y k -> f (x,y) k) ys k)
        m k

    let exists p m =
      try
        iter
          (fun elt -> if p elt then raise Exit)
          m ;
        false
        with Exit -> true

    let for_all p m =
      M.for_all
        (fun x ys -> Elts2.for_all (fun y -> p (x,y)) ys)
        m

    let to_seq =
      let open! Seq in
      let rec seq_pairs m () =
        match m () with
        | Nil -> Nil
        | Cons ((x,ys),m) ->
            let rec do_rec ys () = match ys () with
              | Nil -> seq_pairs m ()
              | Cons (y,ys) -> Cons ((x,y),do_rec ys) in
            do_rec (Elts2.to_seq ys) () in
      fun m -> seq_pairs (M.to_seq m)

    let split3 t =
      let (x,y as p) = choose t in
      let l,ys,h = M.split x t in
      match ys with
      | None -> assert false (* Since choose succeeded *)
      | Some ys ->
          let ys = Elts2.remove y ys in
          if Elts2.is_empty ys then l,p,h
          else M.add x ys l,p,h

    let exists_succ m x =
      try not (Elts2.is_empty @@ M.find x m)
      with Not_found -> false

    let exists_pred m y =
      M.exists (fun _ ys ->  Elts2.exists (o2_equal y) ys)
        m

    let succs rel x = M.safe_find Elts2.empty x rel

    let preds rel y =
      M.fold
        (fun x ys k ->
           if
             Elts2.exists (o2_equal y) ys
           then x::k else k)
        rel [] |> Elts1.of_list


    let cartesian xs ys =
      if Elts2.is_empty ys then empty
      else
        Elts1.fold (fun x k -> M.add x ys k) xs empty

    let of_pred set1 set2 pred =
      Elts1.fold
        (fun e1 k ->
          Elts2.fold
            (fun e2 k ->
              if pred e1 e2 then
                add (e1,e2) k
              else k)
            set2 k)
        set1 empty

(* Domains and Codomains *)
    let domain m =
      M.fold (fun x _ k -> x::k) m [] |> Elts1.of_list

    let codomain m =
      M.fold (fun _ ys k -> ys::k) m [] |> Elts2.unions

(* Restrictions *)
    let restrict_domain p m =
      M.filter
        (fun x ys -> p x && not (Elts2.is_empty ys))
        m
    and restrict_codomain p m =
      M.filter_map
        (fun _ ys ->
           let ys = Elts2.filter p ys in
           if Elts2.is_empty ys then None else Some ys)
        m

    and restrict_domains p1 p2 m =
      M.filter_map
        (fun x ys ->
           if p1 x then
             let ys = Elts2.filter p2 ys in
             if Elts2.is_empty ys then None
             else Some ys
           else None)
        m

    and restrict_rel p m =
      M.filter_map
        (fun x ys ->
           let ys = Elts2.filter (p x) ys in
           if Elts2.is_empty ys then None
           else Some ys)
        m

    (* Set like operations *)

    let subrel m1 m2 =
        try
          M.iter
            (fun x ys1 ->
               let ys2 = succs m2 x in
               if not (Elts2.subset ys1 ys2) then raise Exit)
            m1 ;
          true
        with Exit -> false

    let subset = subrel

    let union m1 m2 =
      M.union_std
        (fun _ ys1 ys2 ->  Some (Elts2.union ys1 ys2)) m1 m2

    let union3 m1 m2 m3 = union m1 @@ union m2 m3
    and union4 m1 m2 m3 m4 = union (union m1 m2) (union m3 m4)
    and union5 m1 m2 m3 m4 m5 =
      union (union (union m1 m2) (union m3 m4)) m5
    let union6 m1 m2 m3 m4 m5 m6 =
      union3 (union m1 m2) (union m3 m4) (union m5 m6)

    let rec union2 k = function
      | [] -> k
      | [m] -> m::k
      | m1::m2::ms -> union2 (union m1 m2::k) ms

    let rec unions = function
      | [] -> M.empty
      | [m] -> m
      | ms -> unions (union2 [] ms)

    let inter m1 m2 =
      M.merge
        (fun _ s1 s2 ->
           match s1,s2 with
           | Some s1,Some s2 ->
               let s = Elts2.inter s1 s2 in
               if Elts2.is_empty s then None
               else Some s
           | (None,Some _)|(Some _,None)|(None,None) -> None)
        m1 m2

    let diff m1 m2 =
      M.merge
        (fun _ s1 s2 ->
           match s1,s2 with
           | None,(None|Some _) -> None
           | Some _,None -> s1
           | Some s1,Some s2 ->
               let s = Elts2.diff s1 s2 in
               if Elts2.is_empty s then None
               else Some s)
        m1 m2
  end
