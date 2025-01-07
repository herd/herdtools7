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

(* Select "all variables" *)
let is_var = function
  | Var _ -> true
  | _ -> false

let as_var = function
  | Var (loc,id) -> Some (loc,id)
  | _ -> None

let as_vars = Misc.opt_list_fold as_var

(* Position *)
let exp2loc = function
  | Konst (loc,_)
  | Tag (loc,_)
  | Var (loc,_)
  | Op1 (loc,_,_)
  | Op (loc,_,_)
  | ExplicitSet (loc,_)
  | App (loc,_,_)
  | Bind (loc,_,_)
  | BindRec (loc,_,_)
  | Fun (loc,_,_,_,_)
  | Match (loc,_,_,_)
  | MatchSet (loc,_,_,_)
  | Try (loc,_,_)
  | If (loc,_,_,_)
    -> loc

let ins2loc = function
  | Let (loc, _)
  | Rec (loc, _, _)
  | InsMatch (loc, _, _, _)
  | UnShow (loc, _)
  | Show (loc, _)
  | ShowAs (loc, _, _)
  | Include (loc, _)
  | Procedure (loc, _, _, _, _)
  | Call (loc, _, _, _)
  | Enum (loc, _, _)
  | Forall (loc, _, _, _)
  | Debug (loc, _)
  | WithFrom (loc, _, _)
  | Events (loc, _, _, _)
  | IfVariant (loc, _, _, _)
  | Test ((loc, _, _, _, _), _)
      -> loc

(* Flatten application of associative operators. *)

let rec flatten = function
| Op (loc,(Union|Inter|Seq as op),es) ->
   (* Associative operators *)
   Op (loc,op,flatten_op op es)
| Op (loc, (Diff|Cartesian|Add|Tuple as op),es) ->
   (* Non-associative operators *)
   Op (loc,op,flattens es)
| Konst _|Tag _|Var _ as e  -> e
| Op1 (loc,ToId,Konst (_,Universe _)) ->
  Var (loc,"id")
| Op1 (loc,op,e) ->
   Op1 (loc,op,flatten e)
| App (loc,e1,e2) ->
   App (loc,flatten e1,flatten e2)
| Bind (loc,bds,e) ->
   Bind (loc,flatten_bds bds,flatten e)
| BindRec (loc,bds,e) ->
   BindRec (loc,flatten_bds bds,flatten e)
| Fun (loc,pat,e,f,xs) ->
   Fun (loc,pat,flatten e,f,xs)
| ExplicitSet (loc,es) ->
   ExplicitSet (loc,flattens es)
| Match (loc,e,cls,eo) ->
   let cls = List.map (fun (x,e) -> x,flatten e)  cls
   and eo =
     match eo with None -> None | Some e -> Some (flatten e) in
   Match (loc,flatten e,cls,eo)
| MatchSet (loc,e1,e2,cl) ->
   let e1 = flatten e1 and e2 = flatten e2
   and cl =
     match cl with
     | EltRem (p1,p2,e) -> EltRem (p1,p2,flatten e)
     | PreEltPost (p1,p2,p3,e) ->PreEltPost (p1,p2,p3,flatten e) in
    MatchSet (loc,e1,e2,cl)
| Try (loc,e1,e2) ->
   Try (loc,flatten e1,flatten e2)
| If (loc,cond,e1,e2) ->
   If (loc,flatten_cond cond,flatten e1,flatten e2)

and flatten_bds bds = List.map (fun (loc,pat,e) -> loc,pat,flatten e) bds

and flatten_cond = function
| Eq (e1,e2) -> Eq (flatten e1,flatten e2)
| Subset (e1,e2) -> Subset (flatten e1,flatten e2)
| In (e1,e2) -> In (flatten e1,flatten e2)
| VariantCond _ as c -> c

and flattens es = List.map flatten es

and flatten_op op0 es =
  let rec do_rec = function
    | [] -> []
    | Op (_,op,args)::es when op=op0 ->
       args @ do_rec es
    | e::es ->
       e::do_rec es in
  flattens es |> do_rec

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
| VariantCond _ -> StringSet.empty

and frees es = StringSet.unions (List.map free es)

and bindings bds = StringSet.unions (List.map (fun (_,_,e) -> free e) bds)

and clauses bds = StringSet.unions (List.map (fun (_,e) -> free e) bds)

let free_body xs e =
  List.fold_left
    (fun r p0 -> remove_pat0 p0 r)
    (free e)
    xs
