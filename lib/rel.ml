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

  type elt1
  type elt2

  include MySet.S with type elt = elt1 * elt2


  module Elts1 : MySet.S with type elt = elt1
  module Elts2 : MySet.S with type elt = elt2
  val exists_succ : t -> elt1 -> bool
  val exists_pred : t -> elt2 -> bool
 
  val succs : t -> elt1 -> Elts2.t
  val preds : t -> elt2 -> Elts1.t

(* Various ways to build a relation *)
  val cartesian : Elts1.t -> Elts2.t -> t
  val of_preds : Elts1.t -> elt2 -> t
  val of_succs : elt1 -> Elts2.t -> t
  val of_pred :
      Elts1.t -> Elts2.t ->
	(elt1 -> elt2 -> bool) -> t

(* Extract domain and codomain *)
  val domain : t -> Elts1.t
  val codomain : t -> Elts2.t

(* Restriction of domain/codomain *)
  val restrict_domain : (elt1 -> bool) -> t -> t
  val restrict_codomain : (elt2 -> bool) -> t -> t
  val restrict_domains : (elt1 -> bool) -> (elt2 -> bool) -> t -> t
  val restrict_rel : (elt1 -> elt2 -> bool) -> t -> t

end

module Make
  (O1:MySet.OrderedType)
  (O2:MySet.OrderedType) : S
with
   type elt1 = O1.t and type elt2 = O2.t
   and module Elts1 = MySet.Make(O1)
   and module Elts2 = MySet.Make(O2)
 =
  struct

    type elt1 = O1.t
    type elt2 = O2.t
  
    include
      MySet.Make
	(struct
	  type t = elt1 * elt2

	  let compare (x1,y1) (x2,y2) = match O1.compare x1 x2 with
	  | 0 ->  O2.compare y1 y2
	  | r -> r
	end)


    module Elts1 = MySet.Make(O1)
    module Elts2 = MySet.Make(O2)

    let exists_succ rel x0 =
      exists (fun (x,_) -> O1.compare x0 x = 0) rel

    let exists_pred rel y0 =
      exists (fun (_,y) -> O2.compare y0 y = 0) rel

    let succs rel x =
      fold
	(fun (x1,y) k ->
	  if O1.compare x x1 = 0 then
	    Elts2.add y k
	  else
	    k)
	rel Elts2.empty

    let preds rel y =
      fold
	(fun (x,y1) k ->
	  if O2.compare y y1 = 0 then
	    Elts1.add x k
	  else
	    k)
	rel Elts1.empty

    let of_succs x ys =
      Elts2.fold
        (fun y -> add (x,y))
        ys empty

    let of_preds xs y =
      Elts1.fold (fun x -> add (x,y))
        xs empty

    let cartesian xs ys =
      if Elts1.is_empty xs || Elts2.is_empty ys then
        empty
      else
        Elts1.fold
	  (fun x acc ->
	    Elts2.fold
	      (fun y acc -> add (x,y) acc)
	      ys acc)
	  xs empty

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
    let extract f xs2set r =
      let xs = fold (fun c k -> f c::k) r [] in
      xs2set xs
    let domain r = extract fst Elts1.of_list r
    let codomain r = extract snd Elts2.of_list r

    let filter_list p r =
      let cs = fold (fun c k -> if p c then c::k else k) r [] in
      of_list cs


    let restrict_domain p r = filter_list (fun (e,_) -> p e) r
    and restrict_codomain p r = filter_list (fun (_,e) -> p e) r
    and restrict_domains p1 p2 r = filter_list (fun (e1,e2) -> p1 e1 && p2 e2) r
    and restrict_rel p r = filter_list (fun (e1,e2) -> p e1 e2) r
  end
