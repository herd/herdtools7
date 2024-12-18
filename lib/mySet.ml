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

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  (* Iterate with counter *)
  val iteri : (int -> elt -> unit) -> t -> unit
  (* Iterate over  cartesian product *)
  val iter2 : (elt -> elt -> unit) -> t -> t -> unit
  (* Exists on cartesian product *)
  val exists2 : (elt -> elt -> bool) -> t -> t -> bool

  val find : (elt -> bool) -> t -> elt
  (** Like exists, but returns an elt that satisfy the predicate,
     raises Not_found, if no such elt exists *)

  val find_opt : (elt -> bool) -> t -> elt option
  (** Like find,  option version *)

  (* Check for a singleton *)
  val is_singleton : t -> bool
  val as_singleton : t -> elt option

  (* Returns list of elements when cardinal <= some bound *)
  val as_small : int -> t -> elt list option

 (* union of small number of sets *)
  val union3 : t -> t -> t -> t
  val union4 : t -> t -> t -> t -> t
  val union5 : t -> t -> t -> t -> t -> t
  val union6 : t -> t -> t -> t -> t -> t -> t

  (* Quite convenient: union of sets given in a list *)
  val unions : t list -> t

  (* Should be obvious *)
  val map_list : (elt -> 'a) -> t -> 'a list
  val map_union : (elt -> t) -> t -> t
  val disjoint : t -> t -> bool

  (* Decomposition, should be efficient an trivial, given
     set implementation as a tree. It is not. *)
  val split3 : t -> t * elt * t

  (* second argument is delimiter (as in String.concat) *)
  val pp : out_channel -> string -> (out_channel -> elt -> unit) -> t -> unit

 (* As above, but sprintf style instead of fprintf style *)
  val pp_str : string -> (elt -> string) -> t -> string

end

module Make(O:OrderedType) : S with type elt = O.t =
  struct
    include Set.Make(O)

    let iteri f s =
      let _ = fold (fun e i -> f i e ; i+1) s 0 in ()

    let iter2 f s1 s2 = iter (fun e1 -> iter (f e1) s2) s1
    let exists2 p s1 s2 = exists (fun e1 -> exists (p e1) s2) s1

    exception Found of elt

    let find pred set =
      try
        iter
          (fun e -> if pred e then raise (Found e))
          set ;
        raise Not_found
      with Found e -> e

    let find_opt pred set =
      try
        iter
          (fun e -> if pred e then raise (Found e))
          set ;
        None
      with Found e -> Some e

    let is_singleton rs =
      try
        let r = choose rs in
        is_empty (remove r rs)
      with Not_found -> false

    let as_singleton rs =
      try
        let r = choose rs in
        if is_empty (remove r rs) then
          Some r
        else None
      with Not_found -> None

    let as_small n rs =
      let rec do_rec n rs =
        if is_empty rs then []
        else if n <= 0 then raise Exit
        else
          let r = try choose rs with Not_found -> assert false in
          r::do_rec (n-1) (remove r rs) in
      try Some (do_rec n rs) with Exit -> None

    let union3 s1 s2 s3 = union s1 (union s2 s3)
    let union4 s1 s2 s3 s4 = union (union s1 s2) (union s3 s4)
    let union5 s1 s2 s3 s4 s5 = union4 s1 s2 s3 (union s4 s5)
    let union6 s1 s2 s3 s4 s5 s6 = union (union4 s1 s2 s3 s4) (union s5 s6)

    let rec union2 k sets = match sets with
    | [] -> k
    | [s] -> s::k
    | s1::s2::sets -> union2 (union s1 s2::k) sets

    let rec unions sets = match sets with
    | [] -> empty
    | [s] -> s
    | _   -> unions (union2 [] sets)

(* Reverse to preserve set ordering *)
    let map_list f s = List.rev (fold (fun e k -> f e::k) s [])
    let map_union f s = unions (map_list f s)

    let disjoint s1 s2 = for_all (fun e -> not (mem e s2)) s1



(* split3, no gain unfortunately *)
    let split3 t =
      let e = choose t in
      let less,_,more = split e t in
      less,e,more

    let pp chan delim pp_elt s =
      try
        let fst = min_elt s in
        pp_elt chan fst ;
        let s = remove fst s in
        iter
          (fun elt ->
            Printf.fprintf chan "%s%a" delim pp_elt elt)
          s
      with Not_found -> ()

    let pp_str delim pp_elt s =
      try
        let fst = min_elt s in
        let fst_str = pp_elt fst in
        let s = remove fst s in
        fold
          (fun elt k ->
            k ^ (Printf.sprintf "%s%s" delim (pp_elt elt)))
          s fst_str
      with Not_found -> ""
  end
