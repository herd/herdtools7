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

(** Bell model utilities *)

open Printf

module Make
    (O:sig val debug : bool val compat : bool end)
    (A:ArchBase.S)
    (C:sig
      val info : BellModel.info option
      val get_id_and_list : A.instruction -> string * string list
      val set_list : A.instruction -> string list -> A.instruction
      val tr_compat : A.instruction -> A.instruction
    end) =
  struct

(******************)
(* Checks of test *)
(******************)

    exception Error of string

    let error fmt =
      ksprintf (fun msg -> raise (Error msg)) fmt

(* Check regions *)

    let check_regions defs i =
      try
        let regions = match BellModel.get_regions i with
        | None ->
            error "no definition of %s in bell file"  BellName.regions
        | Some r -> r in
        List.iter
          (fun (_,rs) ->
            List.iter
              (fun r ->
                if not (StringSet.mem r regions) then
                  error
                    "region %s from test is not part of bell file regions {%s}"
                    r (StringSet.pp_str "," Misc.identity regions))
              rs)
          defs
      with Error msg ->
        Warn.user_error "region error, %s" msg

(* Check scope tree *)
    open BellInfo

    let scope_of = function
      | Tree (sc,_,_) -> sc

    let add sc1 sc2 order =
      let p = StringRel.path sc1 sc2 order in
      if O.debug then eprintf "PATH %s %s = [%s]\n"
        sc1 sc2 (String.concat "; " p) ;
      match p with
    | [] ->
        error
          "scope tree tag %s does not have %s as an ancestor"
          sc2 sc1
    | [e] ->
        error "scope tree tag %s appears as it own successor" e
    | _::rem ->
        let rec add_rec = function
          | [] -> assert false
          | [e] ->
              assert (String.compare e sc2 = 0) ;
              fun st -> st
          | e::rem ->
              fun st -> Tree (e,[],[add_rec rem st]) in
        add_rec rem

    let check_tag name sc tags =
      if not (Misc.Simple.mem sc tags) then
        error
          "tree tag %s is not part of bell file %s declaration"
          sc name

    let expand_scope scopes order =
      if O.debug then eprintf "ORDER: %s\n" (BellModel.pp_order_dec order) ;

      let rec expand_rec top st =
        let sc = scope_of st in
        if O.debug then eprintf "EXPAND_REC top=%s, sc=%s\n" top sc;
        check_tag BellName.scopes sc scopes ;
        let st = match st with
        | Tree (sc,ps,[]) ->
            expand_leaf sc ps order
        | Tree (sc,ps,sts) ->
            Tree (sc,ps,List.map (expand_rec sc) sts) in
        add top sc order st

      and expand_leaf sc ps order =
        let leaves = StringRel.leaves_from sc order in
        begin match StringSet.as_singleton leaves with
        | None ->
            error
              "ambiguity in scope tree: %s does not leads to one leaf"
              sc
        | Some leaf ->
            if String.compare sc leaf = 0 then (Tree (leaf,ps,[]))
            else
              let add = add sc leaf order in
              let leaves = List.map (fun p -> Tree (leaf,[p],[])) ps in
              Tree (sc,[],List.map add leaves)
        end in

      fun st ->
        let sc = scope_of st in
        if O.debug then eprintf "EXPAND: sc='%s'\n" sc ;
        match StringSet.as_singleton (StringRel.roots order) with
        | None ->
            error "ambiguity in scope tree: no unique root"
        | Some root ->
            if O.debug then eprintf "EXPAND: root=%s\n" root ;
            if String.compare sc root = 0 then
              match st with
              | Tree (_,ps,[]) ->
                  expand_leaf sc ps order
              | Tree (_,ps,sts) ->
                  Tree (sc,ps,List.map (expand_rec sc) sts)
            else match sc with
            | "" ->
                begin match st with
                | Tree (_,ps,[]) ->
                    expand_leaf root ps order
                | Tree (_,ps,sts) ->
                    Tree (root,ps,List.map (expand_rec root) sts)
                end
            | _ ->
                check_tag BellName.scopes sc scopes ;
                Tree (root,[],[expand_rec root st])

    let get_relation name i =
      try BellModel.get_relation name i
      with Not_found ->
        error "no definition of %s in bell file" name


    let get_order name i =
      try BellModel.get_order name i
      with Not_found ->
        error "no definition of order for %s in bell file" name

    let check_scopes st i =
      try
        let scopes = get_relation BellName.scopes i in
        let order = get_order  BellName.scopes i in
        let nst =
          expand_scope scopes (StringRel.inverse order) st in
        if O.debug then
          eprintf
            "Scope tree:\n%s\n==>\n%s\n%!"
            (BellInfo.pp_scopes st)
            (BellInfo.pp_scopes nst) ;
        nst
      with Error msg ->
        Warn.user_error "scope error, %s" msg


    let rec do_check_levels levels t = match levels,t with
    | level::levels,Tree (sc,_ps,ts) ->
        if not (Misc.string_eq sc level) then
          error
            "tag %s found in level tree, where %s should be" sc level ;
        List.iter (do_check_levels levels) ts
    | [],Tree (sc,_,_) ->
        error "tag %s found in level tree as a leaf is not the leaf tag" sc

(* NB: Level check is simpler than scope checking, as levels define
       a total order, given as the order of tags in the definition
       of the 'levels' enum type *)
    let check_levels lvls i =
      try
        let levels = get_relation BellName.levels i in
        do_check_levels (List.rev levels) lvls
      with Error msg ->
        Warn.user_error "level error, %s" msg

    let error_instruction i id bi =
      let eg = BellModel.get_events id bi in
      error "instruction '%s' does not match bell declarations\n%s: %s"
        (A.dump_instruction i) id (BellModel.pp_event_dec eg)

    let check_instruction bi i =
      let i = C.tr_compat i in
      try
        let id,al =
          try C.get_id_and_list i
          with Not_found -> raise Exit in (* If no annotation, no trouble *)
        assert (StringSet.mem id BellName.all_sets) ;
        (* Change empty to default, if defined *)
        let i,al =
          match al with
          | [] ->
              begin try
                let al = BellModel.get_default id bi in
                C.set_list i al,al
              with
                Not_found -> i,al
            end
          | _::_ -> i,al in
        let i,al =
          if O.compat then
            let al =
              List.map
                (fun a -> match a with
                | "rcu_read_lock" -> "rcu-lock"
                | "rcu_read_unlock" -> "rcu-unlock"
                | "sync" -> "sync-rcu"
                | _ -> a) al in
             C.set_list i al,al
          else i,al in
        let ok = BellModel.check_event id al bi in
        if not ok then error_instruction i id bi ;
        i
      with
      | Exit -> i
      | Error msg -> Warn.user_error "annotation error, %s" msg

    let do_check parsed bi =
      (* checking instructions *)
      let prog =
        List.map
        (fun (p,code) ->
          p,
          List.map
            (A.pseudo_map (check_instruction bi))
            code)
        parsed.MiscParser.prog in
      let parsed = { parsed with MiscParser.prog; } in
      let test_bi =
        let test_bi = parsed.MiscParser.extra_data in
        begin match (List.map
          (function
           | MiscParser.BellExtra b -> b
           | _ -> Warn.fatal "Error getting bell information from test")
          test_bi) with
          | [] -> assert false
          | x::_ -> x
        end in
      begin match test_bi.BellInfo.regions with
      | Some r -> check_regions r bi
      | _ -> ()
      end ;
      let st = Misc.app_opt (fun st -> check_scopes st bi)  test_bi.BellInfo.scopes in
      let test_bi = { test_bi with BellInfo.scopes=st;} in
      let () = Misc.check_opt (fun lvls -> check_levels lvls bi)  test_bi.BellInfo.levels in
      { parsed with MiscParser.extra_data = [MiscParser.BellExtra test_bi;] }

    let check = match C.info with
    | None -> Misc.identity
    | Some bi -> fun parsed -> do_check parsed bi
  end
