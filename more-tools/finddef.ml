(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module Make (O:sig val verbose : bool end) =
struct
  (* Compute distance *)

  let minimum a b c =
    min a (min b c)

  let distance s t =
    let m = String.length s
    and n = String.length t in
    let d = Array.make_matrix (m+1) (n+1) 0 in

    for i = 0 to m do
      d.(i).(0) <- i
    done;
    for j = 0 to n do
      d.(0).(j) <- j
    done;
    for j = 1 to n do
      for i = 1 to m do
        if s.[i-1] = t.[j-1] then
          d.(i).(j) <- d.(i-1).(j-1)
        else
          d.(i).(j) <-
            minimum
              (d.(i-1).(j) + 1)
              (d.(i).(j-1) + 1)
              (d.(i-1).(j-1) + 1)
      done;
    done;
    d.(m).(n)

  (* Set of strings *)
  module StringSet = Set.Make(String)


  let reverse =
    List.fold_left
      (fun k (name,body) ->
         if Subst.is_reverse body then StringSet.add name k else k)
      StringSet.empty
      Dict.relations

  let () =
    if O.verbose then
      eprintf
        "reverse: [%s]\n%!"
        (StringSet.elements reverse |> String.concat "; ")

  let relation tgt e1 e2 =
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (name,body) ->
           let s = Subst.subst body [| e1; e2; |] in
           let d0 = distance tgt s in
           let best =
             if d0 < d then (name,s),d0
             else best in
           let s = Subst.subst body [| e2; e1; |] in
           let d0 = distance tgt s in
           let best =
             let (_,d) = best in
             if d0 < d then (name^"^-1",s),d0
             else best in
           best)
        (("coucou",""),100) Dict.relations in
    if O.verbose && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    let args =
      if StringSet.mem name reverse then [| e2; e1; |]
      else [| e1; e2; |] in
    name,args

  let set tgt e =
    let arg = [| e |] in
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (name,body) ->
           let s = Subst.subst body arg in
           let d0 = distance tgt s in
           let best =
             if d0 < d then (name,s),d0
             else best in
           best)
        (("coucou",""),100) Dict.sets in
    if O.verbose && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    name,arg

  let is_event = Subst.is_event

  let find ws =
    let es = List.filter is_event ws in
    let es = Array.of_list es in
    let tgt = String.concat "" ws in
    match Array.length es with
    | 1 -> set tgt es.(0)
    | 2 -> relation tgt es.(0) es.(1)
    | _ ->
        Warn.fatal "Bad arguments: '%s'" tgt
end
