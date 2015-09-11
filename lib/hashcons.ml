(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Hash tables for hash-consing. Code borrowed to Jean-Christophe Filliatre,
   included by permission. *)

type 'a hash_consed = { 
  hkey : int;
  tag : int;
  node : 'a }

let gentag =
  let r = ref 0 in
  fun () -> incr r; !r

type 'a t = {
  mutable table : 'a hash_consed Weak.t array ;
  mutable totsize : int;             (* sum of the bucket sizes *)
  mutable limit : int;               (* max ratio totsize/table length *)
}

type 'a u = 'a t
(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> key hash_consed
    val iter : (key hash_consed -> unit) -> t -> unit
  end

module Make(H : HashedType) : (S with type key = H.t) = struct

  type key = H.t


  type t = H.t u
(*
  type data = H.t hash_consed
  type t = {
    mutable table : data Weak.t array;
    mutable totsize : int;             (* sum of the bucket sizes *)
    mutable limit : int;               (* max ratio totsize/table length *)
  }
*)
  let emptybucket = Weak.create 0

  let create sz =
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
    {
      table = Array.make sz emptybucket;
      totsize = 0;
      limit = 3;
    }

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- emptybucket
    done;
    t.totsize <- 0;
    t.limit <- 3
  
  let fold f t init =
    let rec fold_bucket i b accu =
      if i >= Weak.length b then accu else
      match Weak.get b i with
      | Some v -> fold_bucket (i+1) b (f v accu)
      | None -> fold_bucket (i+1) b accu
    in
    Array.fold_right (fold_bucket 0) t.table init

  let iter f t =
    let rec iter_bucket i b =
      if i >= Weak.length b then () else
      match Weak.get b i with
      | Some v -> f v; iter_bucket (i+1) b
      | None -> iter_bucket (i+1) b
    in
    Array.iter (iter_bucket 0) t.table

  let count t =
    let rec count_bucket i b accu =
      if i >= Weak.length b then accu else
      count_bucket (i+1) b (accu + (if Weak.check b i then 1 else 0))
    in
    Array.fold_right (count_bucket 0) t.table 0

  let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1)

  let rec resize t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
      fold (fun d () -> add newt d) t ();
      t.table <- newt.table;
      t.limit <- t.limit + 2;
    end

  and add t d =
    let index = d.hkey mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
        let newsz = min (sz + 3) (Sys.max_array_length - 1) in
        if newsz <= sz then 
	  failwith "Hashcons.Make: hash bucket cannot grow more";
        let newbucket = Weak.create newsz in
        Weak.blit bucket 0 newbucket 0 sz;
        Weak.set newbucket i (Some d);
        t.table.(index) <- newbucket;
        t.totsize <- t.totsize + (newsz - sz);
        if t.totsize > t.limit * Array.length t.table then resize t;
      end else begin
        if Weak.check bucket i
        then loop (i+1)
        else Weak.set bucket i (Some d)
      end
    in
    loop 0

  let hashcons t d =
    let hkey = H.hash d in
    let hkey = hkey land max_int in (* >=0 !!! *)
    let sztable = Array.length t.table in
    let index = hkey mod sztable in    
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
	let hnode = { hkey = hkey; tag = gentag (); node = d } in
	add t hnode;
	hnode
      end else begin
        match Weak.get_copy bucket i with
        | Some v when H.equal v.node d -> 
	    begin match Weak.get bucket i with
              | Some v -> v
              | None -> loop (i+1)
            end
        | _ -> loop (i+1)
      end
    in
    loop 0
  
end
