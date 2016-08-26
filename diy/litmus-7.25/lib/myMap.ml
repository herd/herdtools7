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

(** Operations on maps *)

open Printf

module type S = sig
  include Map.S

  val pp : out_channel -> (out_channel -> key -> 'a -> unit) -> 'a t -> unit
  val pp_str_delim :  string -> (key -> 'a -> string) -> 'a t -> string
  val pp_str : (key -> 'a -> string) -> 'a t -> string

(* find with a default value *)
  val safe_find : 'a -> key -> 'a t -> 'a

(* map union *)
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val unions : ('a -> 'a -> 'a) -> 'a t list -> 'a t

(* List bindings *)
  val bindings : 'a t -> (key * 'a) list
  val add_bindings : (key * 'a) list -> 'a t -> 'a t
  val from_bindings :  (key * 'a) list -> 'a t
end

module Make(O:Set.OrderedType) : S with type key = O.t =
  struct
    include Map.Make(O)

    let pp_str_delim delim pp_bind m =
      let bds = fold (fun k v r -> (k,v)::r) m [] in
      let bds = List.map (fun (k,v) -> pp_bind k v) bds in
      String.concat delim bds

    let pp_str pp_bind m = pp_str_delim ";" pp_bind m


    let pp chan pp_bind m =
      iter
        (fun k v -> pp_bind chan k v ; fprintf chan ";")
        m

    let safe_find d k m = try find k m with Not_found -> d

    let union u m1 m2 =
      fold
        (fun k v1 m ->
          try
            let v2 = find k m2 in
            add k (u v1 v2) m
          with Not_found -> add k v1 m)
        m1 m2

    let unions u ms = match ms with
    | [] -> empty
    | m::ms -> List.fold_left (union u) m ms

    let bindings m = fold (fun k v xs -> (k,v)::xs) m []
    let add_bindings bds m =
      List.fold_left (fun m (k,v) -> add k v m) m bds
    let from_bindings bds = add_bindings bds empty
        
  end
