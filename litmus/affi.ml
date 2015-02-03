 (*********************************************************************)
(*                         Diy/Litmus                                *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*  Copyright 2011, the authors and                                  *)
(*  Institut National de Recherche en Informatique et                *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf

type com = Rf | Fr | Ws | Hat

let is_not_rf = function
  | Rf -> false
  | Fr|Ws|Hat -> true

type t =
    int list list      (* Thread grouping *)
      * (int * int) list (* Exclusion *)

let pp (cs,ne) =
  let cs =
    String.concat " "
      (List.map
         (fun c ->
           sprintf "[%s]" (String.concat ", " (List.map (sprintf "%i") c)))
         cs)
  and ne =
    String.concat " "
      (List.map (fun (x,y) -> sprintf "(%i,%i)" x y) ne) in
  sprintf "%s ; %s" cs ne

let dbg fmt = kprintf (fun _ -> ()) fmt

let debug_nodes xss =
  String.concat " "
    (List.map
       (fun xs ->
         (sprintf "[%s]"
            (String.concat "," (List.map (sprintf "%i") xs))))
       xss)

type node =
    { p : int ; edge : com; mutable next : node ; mutable prev : node; }

let rec nil =
  { p = -1 ; edge = Rf ;  next = nil ; prev = nil ; }


(* Build proc cycle *)

let mk_cycle coms =
  let ms =
    Misc.mapi
      (fun p c ->
        { p=p; edge=c; next=nil; prev=nil;}) coms in
  let patch = function
    | [] -> assert false
    | [x] ->
        x.next <- x ; x.prev <- x ; x
    | x::xs ->
        let rec do_rec prev = function
          | [] ->
              prev.next <- x ; x.prev <- prev
          | y::ys ->
              prev.next <- y ; y.prev <- prev ;
              do_rec y ys in
        do_rec x xs ; x in
  patch ms

let find_next p m =
  let rec do_rec n =
    if p n then n
    else if n.next == m then raise Not_found
    else do_rec n.next in
  do_rec m

let find_fr =
  find_next
    (fun n -> match n.edge with
    | Fr -> true
    | _ -> false)

let find_ws =
  find_next
    (fun n -> match n.edge with
    | Ws -> true
    | _ -> false)


let rec to2 xs k = match xs with
| [] -> k
| [x] -> xs::k
| x::y::xs -> [x;y]::to2 xs k

let add xs ys = match xs with
| [] -> ys
| xs -> to2 xs ys


let collect m =
  let m = m.next in
  let rec do_rec n =
    let xs,ys =
      if n.next == m then [],[]
      else do_rec n.next in
    let ep,e = n.prev.edge,n.edge in
    let n = n.p in
    match e with
(* Ws *)
    | Ws|Fr|Hat -> [n],(add xs ys)
(* Rf *)
    | Rf-> n::xs,ys in
  
  let xs,ys = do_rec m in
  add xs ys

let collect_fr m =
  let rec do_rec n =
    let frs =
      if n.next == m then []
      else do_rec n.next in
    let e = n.edge in
    match e with
    | Fr -> (n.p,n.next.p)::frs
    | _  -> frs in 
  do_rec m

let compute cs =
  let c = mk_cycle cs in
  let c =
    try find_fr c
    with Not_found ->
      try find_ws c
      with Not_found -> c in
  let ne = collect_fr c in
  let xs =
    dbg "Start: %i\n" c.p ;
    let xs = collect c in
    dbg "Nodes: %s\n" (debug_nodes xs) ;
    begin match xs with
    | [[x;y]] (* specific case Rf+ Fr/Ws *)
      when List.exists is_not_rf cs -> [ [x];[y] ]
    | [xs]  -> to2 xs []
    | _ -> xs
    end in
  let xs =
    List.sort
      (fun xs ys -> Misc.int_compare (List.length ys) (List.length xs))
      xs in
  xs,ne
