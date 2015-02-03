(*********************************************************************)
(*                        Offence                                    *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Operations on maps *)

open Printf

module type S = sig
  include Map.S
  val pp : out_channel -> (out_channel -> key -> 'a -> unit) -> 'a t -> unit
  val pp_str : (key -> 'a -> string) -> 'a t -> string

(* find with a default value *)
  val safe_find : 'a -> key -> 'a t -> 'a

(* map union *)
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

(* List bindings *)
  val bindings : 'a t -> (key * 'a) list
  val from_bindings :  (key * 'a) list -> 'a t
end

module Make(O:Set.OrderedType) : S with type key = O.t =
  struct
    include Map.Make(O)

    let pp_str pp_bind m =
      let bds = fold (fun k v r -> (k,v)::r) m [] in
      let bds = List.map (fun (k,v) -> pp_bind k v) bds in
      String.concat "; " bds

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

    let bindings m = fold (fun k v xs -> (k,v)::xs) m []
    let from_bindings bds =
      List.fold_left (fun m (k,v) -> add k v m) empty bds
  end
