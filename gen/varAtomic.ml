(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Edge variations on plain/atomic/reserve *)

module type S = sig
  type edge

  val varatom_one : edge list -> edge list list
  val varatom_es : edge list list -> edge list list
end

module type Fold = sig
  type atom
  val fold : (atom option -> 'a -> 'a) -> 'a -> 'a
end

module Make(E:Edge.S) (F:Fold with type atom = E.atom) : S
with type edge = E.edge = struct


  type edge = E.edge

  open E

(* Notice, var_src si applied second, hence RMW check *)
  let can_set_src e a = match e.E.edge with
  | E.Rmw _ -> E.compare_atomo e.E.a2 a = 0
  | _ -> true

  let var_src e es = match e.E.a1 with
  | None ->
      F.fold
        (fun ao k ->
          if can_set_src e ao then
            ({ e with a1=ao }::es)::k
        else k)
        []
  | Some _ -> [e::es]

  let var_tgt e es = match e.E.a2 with
  | None ->
      F.fold
        (fun ao k -> ({ e with a2=ao }::es)::k)
        []
  | Some _ -> [e::es]

  let var_edge e1 e2 es = match e1.E.a2,e2.E.a1 with
  | None,None ->
      F.fold
        (fun ao k ->
          if can_set_src e2 ao then
            ({ e1 with a2 = ao}::{ e2 with a1 = ao}::es)::k
          else k)
        []
  | _,_ -> [e1::e2::es]

(*
  let var_both e = match e.E.edge with
  | E.Rmw -> begin match e.E.a1,e.E.a2 with
    | None,None -> (* RMW have identical atomic specs for source and targets *)
        F.fold
          (fun ao k -> { e with a1=ao; a2=ao; }::k)
          []
    | _,_ -> [e]
  end
  | _ -> begin match e.E.a1,e.E.a2 with
    | None,None ->
        F.fold
          (fun ao1 k ->
            F.fold
              (fun ao2 k -> { e with a1=ao1; a2=ao2; }::k)
              k)
          []
    | None,Some _ ->
        F.fold
          (fun ao1 k -> { e with a1=ao1;}::k)
          []
    | Some _,None ->
        F.fold
          (fun ao2 k -> { e with a2=ao2;}::k)
          []
    | Some _,Some _ -> [e]
  end
*)

(* Variation of composite relaxation candidate *)
  let as_cons = function
    | e::es -> e,es
    | [] -> assert false

  let rec varatom_inside = function
    | [] -> assert false
    | [e] ->  var_tgt e []
    | e1::(_::_ as ess) ->
        let ess = varatom_inside ess in
        List.fold_right
          (fun es k ->
            let e2,rem = as_cons es in
            var_edge e1 e2 rem@k)
          ess []

  let varatom_ones es k = match es with
  | [] -> k
  | _ ->
      let ess =   varatom_inside es in
      List.fold_right
        (fun es k ->
          let e,es = as_cons es in
          var_src e es@k)
        ess k

  let varatom_one es = varatom_ones es []

  let varatom_es es = List.fold_right varatom_ones es []

end
