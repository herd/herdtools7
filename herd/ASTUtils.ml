(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open AST

(* Free variables *)
    let rec free = function
      | Konst _ | Tag _ -> StringSet.empty
      | Var (_,x) -> StringSet.singleton x
      | Op1 (_,_,e)
          -> free e
      | Op (_,_,es)|ExplicitSet (_,es) -> frees es
      | App (_,e,es) ->
          StringSet.union (free e) (frees es)
      | Bind (_,bds,e) ->
          let xs =
            StringSet.of_list (List.map fst bds) in
          StringSet.union
            (bindings bds)
            (StringSet.diff (free e) xs)
      | BindRec (_,bds,e) ->
          let xs =
            StringSet.of_list (List.map fst bds) in
          StringSet.diff
            (StringSet.union (free e) (bindings bds))
            xs
      | Fun (_,_,_,_,fs) -> fs
      | Match (_,e,cls,eo) ->
          let e = free e
          and cls = bindings cls
          and eo = match eo with
          | None -> StringSet.empty
          | Some e -> free e in
          StringSet.union (StringSet.union e eo) cls
      | MatchSet (_,e1,e2,(x,xs,e3)) ->
          let e1 = free e1
          and e2 = free e2
          and e3 = free e3 in
          StringSet.union
            (StringSet.union e1 e2)
            (StringSet.remove x (StringSet.remove xs e3))

and frees es = StringSet.unions (List.map free es)

and bindings bds = StringSet.unions (List.map (fun (_,e) -> free e) bds)

let free_body xs e =
  StringSet.diff (free e) (StringSet.of_list xs)
