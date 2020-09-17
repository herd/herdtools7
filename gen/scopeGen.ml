(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val debug : bool
  val info : BellModel.info
end

module type S = sig

  val default : int -> BellInfo.scopes

  val gen :
      (string * int * int) list ->
        int -> (BellInfo.scopes -> 'a -> 'a) -> 'a -> 'a

  val all : int ->  (BellInfo.scopes -> 'a -> 'a) -> 'a -> 'a

end

module Make(O:Config) : S = struct

  open BellInfo

  let bad_order () = Warn.fatal "unsuitable or no order in bell file"
  let default n =
    try
      let o = BellModel.get_order BellName.scopes O.info in
      let tops = StringRel.leaves o in
      let top = StringSet.choose tops in
      Tree (top,Misc.interval 0 n,[])
    with
    | Not_found -> bad_order ()

(***********************)
(* Partition generator *)
(***********************)

  let add_elt y yss k res =
    let rec add_rec prev yss res = match yss with
    | [] -> res
    | ys::yss ->
        let res = k (prev ((y::ys)::yss)) res in
        add_rec (fun yss -> prev (ys::yss)) yss res in
    add_rec (fun yss -> yss) yss res

  let part minsz maxsize xs k res =
    let rec do_rec sz yss xs res = match xs with
    | [] -> if sz >= minsz then k yss res else res
    | x::xs ->
        let res =
          if sz+1 > maxsize then res
          else do_rec (sz+1) ([x]::yss) xs res in
        add_elt x yss (fun yss res -> do_rec sz yss xs res) res in
    do_rec 0 [] (List.rev xs) res

(******************)
(* Tree generator *)
(******************)

  open BellInfo

  let gen_leaf sc min max xs k res =
    part min max xs
      (fun yss res ->
        let sts = List.map (fun ys -> Tree (sc,ys,[])) yss in
        k sts res)
      res

  let children sc ts = Tree (sc,[],ts)

  let contract =
    if O.debug then Misc.identity
    else BellInfo.contract

  let rec
      do_gen :
      'a. (string * int * int) list -> int list ->
        (BellInfo.scopes list -> 'a -> 'a) -> 'a -> 'a =
          fun scs xs k res -> match scs with
          | [] -> assert false
          | [sc,min,max] ->
              gen_leaf sc min max xs k res
          | (sc,min,max)::scs ->
              part min max xs
                (fun yss res ->
                  let ysss =
                    List.map (fun ys -> do_gen scs ys Misc.cons []) yss in
                  Misc.fold_cross ysss
                    (fun stss res ->
                      k (List.map (children sc) stss) res)
                    res)
                res

  let gen scs n k res =
    do_gen scs (Misc.interval 0 n)
      (fun ts res -> k (contract (children "" ts)) res)
      res

  let get_scopes () =
    try
      let o = BellModel.get_order BellName.scopes O.info in
      let tops = StringRel.leaves o
      and bots = StringRel.roots o in
      let top = StringSet.choose tops
      and bot = StringSet.choose bots in
      List.rev (StringRel.path bot top o)
   with
    | Not_found -> bad_order ()


  let all n k res =
    let scs = get_scopes () in
    match scs with
    | [] -> bad_order ()
    | top::rem ->
        let scs = List.map (fun sc -> sc,1,n) rem in
        do_gen scs (Misc.interval 0 n)
          (fun ts res ->
            let t = contract (children top ts) in
            k t res)
          res
end

module NoGen = struct
  let fail () = Warn.fatal "no scope information"
  let default _ = fail ()
  let gen _ = fail ()
  let all _ = fail ()
end
