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

open AST

(* Free variables *)
let bound_var = function
  | None -> StringSet.empty
  | Some x ->  StringSet.singleton x

let bound_pat = function
  | Pvar x -> bound_var x
  | Ptuple xs -> StringSet.unions (List.map bound_var xs)

let bound_bds bds =
  StringSet.unions
    (List.map (fun (_,pat,_) -> bound_pat pat) bds)

let rec free = function
  | Konst _ | Tag _ -> StringSet.empty
  | Var (_,x) -> StringSet.singleton x
  | Op1 (_,_,e)
    -> free e
  | Op (_,_,es)|ExplicitSet (_,es) -> frees es
  | App (_,e1,e2) ->
      StringSet.union (free e1) (free e2)
  | Bind (_,bds,e) ->
      let xs = bound_bds bds in
      StringSet.union
        (bindings bds)
        (StringSet.diff (free e) xs)
  | BindRec (_,bds,e) ->
      let xs = bound_bds bds in
      StringSet.diff
        (StringSet.union (free e) (bindings bds))
        xs
  | Fun (_,_,_,_,fs) -> fs
  | Match (_,e,cls,eo) ->
      let e = free e
      and cls = clauses cls
      and eo = match eo with
      | None -> StringSet.empty
      | Some e -> free e in
      StringSet.union (StringSet.union e eo) cls
  | MatchSet (_,e1,e2,cl) ->
      let e1 = free e1
      and e2 = free e2
      and cl = free_cl cl in
      StringSet.unions [e1;e2;cl;]
  | Try (_,e1,e2) ->
      StringSet.union (free e1) (free e2)
  | If (_,cond,ifso,ifnot) ->
      StringSet.union (free_cond cond)
        (StringSet.union (free ifso) (free ifnot))

and remove_pat0 x = match x with
| None -> Misc.identity
| Some x ->  StringSet.remove x

and free_cl = function
  | EltRem (x,xs,e) ->
      remove_pat0 x (remove_pat0 xs (free e))
  | PreEltPost (xs1,x,xs2,e) ->
      remove_pat0 xs1 (remove_pat0 x (remove_pat0 xs2 (free e)))

and free_cond c = match c with
| Eq (e1,e2)
| Subset (e1,e2)
| In (e1,e2)
  -> StringSet.union (free e1) (free e2)
| Variant _ -> StringSet.empty

and frees es = StringSet.unions (List.map free es)

and bindings bds = StringSet.unions (List.map (fun (_,_,e) -> free e) bds)

and clauses bds = StringSet.unions (List.map (fun (_,e) -> free e) bds)

let free_body xs e =
  List.fold_left
    (fun r p0 -> remove_pat0 p0 r)
    (free e)
    xs
