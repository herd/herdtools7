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

module type Config = sig
  include ParserConfig.Config
  val cat : string
end

module Make (O:Config) =
struct

  (* All top level names defined by module *)

  module RN =
    ReadNames.Make
      (struct
        let includes = O.includes
        let libdir = O.libdir
        let debug = O.verbose > 1
      end)

  let all_names = RN.read  O.cat

  let () =
    if O.verbose > 1 then
      eprintf
        "Names: {%s}\n%!" (StringSet.pp_str "," Misc.identity all_names)

  (* Compute distance *)

  (* Pre-defined names not defined bu model *)


  let from_csname =
    let add_name name map =
      let csname = MiaouNames.to_csname name in
      StringMap.add csname name map in

    let predef_map =
      List.fold_right add_name
        [
          "iico-ctrl";
          "iico-addr";
          "iico-data";
          "iico-order";
          "ext"; "int";
          "BCC"; "po";
        ]
        StringMap.empty in

    let map =
      StringSet.fold
        (fun name map ->
           let csname = MiaouNames.to_csname name in
           StringMap.add csname name map)
        all_names predef_map in

    fun csname ->
      try StringMap.find csname map
      with Not_found ->
        if O.verbose > 0 then Warn.warn_always "name %s not recognized" csname ;
        csname

  (* Distance *)
  let minimum a b c = min a (min b c)

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

  let longuest =
    let rec do_rec best = function
      | [] -> best
      | name::rem ->
          let best =
            if String.length name > String.length best then name
            else best in
          do_rec best rem in
    function
    | [] -> assert false
    | name::rem -> do_rec name rem

  let dict_relations =
    let add_name map (name,def) =
      let old =
        try StringMap.find def map
        with Not_found -> [] in
      StringMap.add def (name::old) map in
    let map = List.fold_left add_name StringMap.empty Dict.relations in
    StringMap.fold
      (fun def names dict ->
         match names with
         | [name] -> (name,def)::dict
         | _ ->
             let name =
               match
                 List.filter
                   (fun name -> StringSet.mem name all_names)
                   names
               with
               | [] -> longuest names
               | [name] -> name
               | names -> longuest names in
             (name,def)::dict)
      map []

  let reverse =
    List.fold_left
      (fun k (name,body) ->
         if Subst.is_reverse body then StringSet.add name k else k)
      StringSet.empty
      dict_relations

  let () =
    if O.verbose > 1 then
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

  let is_reverse n = StringSet.mem n reverse

  let relation is_def tgt e1 e2 =
    let a1 = set_arg is_def e1
    and a2 = set_arg is_def e2 in
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (name,body) ->
           let a1,a2 =
             if is_reverse name then a2,a1 else a1,a2 in
           let s = Subst.subst body [| a1; a2; |]
           and s_rev = Subst.subst body [| a2; a1; |] in
           let d0 = distance tgt s
           and d0_rev= distance tgt s_rev in
           if d0 <= d0_rev then
             if d0 < d then (PreCat.Name name,s),d0 else best
           else
             if d0_rev < d then (PreCat.Inverse name,s_rev),d0_rev else best)
        ((PreCat.Name "coucou",""),100) dict_relations in
    if true || O.verbose > 0 && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    let args =
      let open PreCat in
      match name with
      | Name n -> if is_reverse n then [| e2; e1; |] else [| e1; e2; |]
      | Inverse n -> if is_reverse n then [| e1; e2; |] else [| e2; e1; |]
      | _ -> assert false in
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
    if O.verbose > 0 && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    PreCat.Name name,arg

  let justname tgt =
    let (name,body),d =
      List.fold_left
        (fun (_,d as best) (_,s as p) ->
           let d0 = distance tgt s in
           if d0 < d then p,d0
           else best)
        (("coucou",""),100) Dict.names in
    if O.verbose > 0 && d > 1 then
      eprintf "Approx[%d]: '%s' as '%s'\n%!" d tgt body ;
    name

  (* Only keep first occurences of events *)
  let remove_dup_events =
    let rec do_rec rs = function
      | [] -> List.rev rs (* Reverse to keep order *)
      | e::es ->
          let rs =
            if List.exists (String.equal e) rs then rs
            else e::rs in
          do_rec rs es in
    do_rec []

  let do_find is_def ws =
    let es = List.filter Subst.is_event ws in
    let es = remove_dup_events es in
    let es = Array.of_list es in
    let is_neg,ws = check_neg ws in
    let is_plus,ws = check_plus ws in
    let name,es =
      if is_plus then
        let ws = remove_plus_suffix ws in
        let tgt = ws_to_string ws in
        PreCat.(Plus (Name (justname tgt))),es
      else
        let tgt =  ws_to_string ws in
        match Array.length es with
        | 1 -> set is_def tgt es.(0)
        | 2 ->
            if false then
              eprintf "e0=%s, e1=%s, tgt=%s\n%!" es.(0) es.(1) tgt ;
            relation is_def tgt es.(0) es.(1)
        | _ ->
            Warn.fatal "Bad arguments: '%s'" tgt in
    if is_neg then PreCat.Neg name,es else name,es

  let remove_def_suffix ws0 =
    let rec find_rec = function
      | []  ->
          Warn.fatal
            "Not a definition: {%s}"
            (String.concat "," ws0)
      | "if"::("all"|"one")::"of"::_ -> []
      | w::ws -> w::find_rec ws in
    find_rec ws0

  let from_csname (name,es) = PreCat.map_name from_csname name,es

  let find ws = do_find false ws |> from_csname
  and find_def ws = do_find true @@ remove_def_suffix ws |> from_csname

end
