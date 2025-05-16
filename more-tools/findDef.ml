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

  let ws_to_string ws = String.concat "" ws

  let rec check_prefix ps ws =
    match ps,ws with
    | _,[] -> None
    | [],_::_ -> Some ws
    | p::ps,w::ws ->
      if List.exists (String.equal (Misc.lowercase w)) p  then
        check_prefix ps ws
      else None

  let prefix_neg =
    List.map (fun x -> [x])
      ["it"; "is"; "not"; "the"; "case"; "that"; ]

  let check_neg ws =
    match check_prefix prefix_neg ws with
    | Some ws -> true,ws
    | None    -> false,ws

  let prefix_plus = [["there"]; ["is";"exists"]; ["a"]; ["chain"]; ["of"]; ]
                    
  let check_plus ws =
    match check_prefix prefix_plus ws with
    | Some ws -> true,ws
    | None -> false,ws

  let remove_plus_suffix =
    let rec find_from = function
      | [] -> assert false
      | "from"::e1::"to"::e2::[] when Subst.is_event e1 && Subst.is_event e2 -> []
      | w::ws -> w::find_from ws in
    find_from

  let set_arg is_def =
    if is_def then sprintf "anEffect%s"
    else Misc.identity

  let relation is_def tgt e1 e2 =
    let a1 = set_arg is_def e1
    and a2 = set_arg is_def e2 in
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (name,body) ->
           let a1,a2 =
             if StringSet.mem name reverse then a2,a1 else a1,a2 in
           let s = Subst.subst body [| a1; a2; |]
           and s_rev = Subst.subst body [| a2; a1; |] in
           let d0 = distance tgt s
           and d0_rev= distance tgt s_rev in
           if d0 <= d0_rev then
             if d0 < d then (name,s),d0 else best
           else
             if d0_rev < d then (name ^ "-1",s_rev),d0_rev else best)
        (("coucou",""),100) Dict.relations in
    if O.verbose && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    let args =
      if StringSet.mem name reverse then [| e2; e1; |]
      else [| e1; e2; |] in
    name,args

  let set is_def tgt e =
    let arg = [| set_arg is_def e |] in
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

  let justname tgt =
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (_,s as p) ->
           let d0 = distance tgt s in
           if d0 < d then p,d0
           else best)
        (("coucou",""),100) Dict.names in
    if O.verbose && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    name
    
  let do_find is_def ws =
    let es = List.filter Subst.is_event ws in
    let es = Array.of_list es in
    let is_neg,ws = check_neg ws in
    let is_plus,ws = check_plus ws in
    let name,es =
      if is_plus then
        let ws = remove_plus_suffix ws in
        let tgt = ws_to_string ws in
        justname tgt ^ "+",es
      else
        let tgt =  ws_to_string ws in
        match Array.length es with
        | 1 -> set is_def tgt es.(0)
        | 2 -> relation is_def tgt es.(0) es.(1)
        | _ ->
            Warn.fatal "Bad arguments: '%s'" tgt in
    if is_neg then "~" ^ name,es else name,es

  let remove_def_suffix ws0 =
    let rec find_rec = function
      | []  ->
          Warn.fatal
            "Not a definition: {%s}"
            (String.concat "," ws0)              
      | "if"::("all"|"one")::"of"::_ -> []
      | w::ws -> w::find_rec ws in
    find_rec ws0

  let find ws = do_find false ws
  and find_def ws = do_find true @@ remove_def_suffix ws
    
end
