(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Imperative disjoint set data structure *)

module Make (O:Set.OrderedType) : sig
  type t

(* All creation steps must preceed union/find operations *)
  val create : unit -> t
  val add : t -> O.t -> t

(* Union/Find *)
  val find : t -> O.t -> O.t
  val union : t -> O.t -> O.t ->  unit

(* Extract result *)
  module Sol : Map.S with type key = O.t
  val as_solution : t -> O.t Sol.t

end = struct

  type cell = { value : O.t; mutable rank : int; mutable parent : cell; }

  module M = Map.Make(O)

  type t = cell M.t

  let map_find x m = try M.find x m with Not_found -> assert false 

(* Creation *)
  let create () = M.empty

  let add m x =
    let rec c = { value=x; rank=0; parent=c; } in
    M.add x c m


(* Union/Find *)
  let rec find_aux c =
    if c == c.parent then c
    else
      let d = find_aux c.parent in
      c.parent <- d ;
      d
      
      
  let find m x =
    let c = map_find x m in
    let d = find_aux c in
    d.value


  let union_aux c1 c2 =
    let d1 = find_aux c1
    and d2 = find_aux c2 in
    if d1.rank > d2.rank then
      d2.parent <- d1
    else if d2.rank > d1.rank then
      d1.parent <- d2
    else if d1 != d2 then begin
      d2.parent <- d1 ;
      d1.rank <- d1.rank+1
    end

  let union m x1 x2 = union_aux (map_find x1 m) (map_find x2 m)
      
(* Extract *)
  module Sol = M

  let as_solution m =
    M.fold
      (fun x c k ->
        if c.parent == c then k else
        M.add x c.parent.value k)
      m M.empty
end
