(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* Tyler Sorensen, University College London, UK.                    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Bell model utilities *)

open Printf

module Make
    (O:sig val debug : bool end)
    (A:Arch.S)
    (C:sig
      val info : BellModel.info option
      val get_id_and_list : A.instruction -> string * string list
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
          (fun (_,r) ->
            if not (StringSet.mem r regions) then
              error
                "region %s from test is not part of bell file regions {%s}"
                r (StringSet.pp_str "," Misc.identity regions))
          defs
      with Error msg ->
        Warn.user_error "region error, %s" msg
          
(* Check scope tree *)
    open BellInfo

    let scope_of = function
      | Leaf (sc,_) | Children (sc,_) -> sc

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
              fun st -> Children (e,[add_rec rem st]) in
        add_rec rem

    let check_tag sc scopes =
      if not (Misc.Simple.mem sc scopes) then
        error
          "scope tree tag %s is not part of bell file %s declaration"
          sc BellName.scopes

    let expand_scope scopes order =
      if O.debug then eprintf "ORDER: %s\n" (BellModel.pp_order_dec order) ;
      let rec expand_rec top st =
        let sc = scope_of st in
        if O.debug then eprintf "EXPAND_REC top=%s, sc=%s\n" top sc;
        check_tag sc scopes ;
        let st = match st with
        | Leaf (sc,ps) ->
            expand_leaf sc ps order
        | Children (sc,sts) ->
            Children (sc,List.map (expand_rec sc) sts) in
        add top sc order st

      and expand_leaf sc ps order =
        let leaves = StringRel.leaves_from sc order in
        begin match StringSet.as_singleton leaves with
        | None ->
            error
              "ambiguity in scope tree: %s does not leads to one leaf"
              sc
        | Some leaf ->            
            if String.compare sc leaf = 0 then (Leaf (leaf,ps))
            else
              let add = add sc leaf order in
              let leaves = List.map (fun p -> Leaf (leaf,[p])) ps in
              Children (sc,List.map add leaves)
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
              | Leaf (_,ps) ->
                  expand_leaf sc ps order
              | Children (_,sts) ->
                  Children (sc,List.map (expand_rec sc) sts)
            else match sc with
            | "" ->
                begin match st with
                | Leaf (_,ps) ->
                    expand_leaf root ps order
                | Children (_,sts) ->
                    Children (root,List.map (expand_rec root) sts)
                end
            | _ ->
                check_tag sc scopes ;
                Children (root,[expand_rec root st])

    let check_scopes st i =
      try
        let scopes =
          try BellModel.get_relation BellName.scopes i
          with Not_found ->
            error "no definition of %s in bell file" BellName.scopes in
        let order = 
          try BellModel.get_order BellName.scopes i
          with Not_found ->
            error "no definition of scope order in bell file" in
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

    let check_instruction bi i =
      try
        let id,al =
          try C.get_id_and_list i
          with Not_found -> raise Exit in (* If no annotation, no trouble *)
        assert (StringSet.mem id BellName.all_mem_sets) ;
        let ok = BellModel.check_event id al bi in
        if not ok then begin
          let eg = BellModel.get_events id bi in
          error "instruction '%s' does not match bell declarations\n%s: %s"
            (A.dump_instruction i) id (BellModel.pp_event_dec eg)
        end
      with
      | Exit -> () 
      | Error msg -> Warn.user_error "annotation error, %s" msg

    let do_check parsed bi = 
      (* checking instructions *)
      List.iter
        (fun (_,code) ->
          List.iter
            (A.pseudo_iter (check_instruction bi))
            code)
        parsed.MiscParser.prog; 
      
      let test_bi = parsed.MiscParser.bell_info in            
      let test_bi = match test_bi with
      | Some b -> b
      | None -> Warn.fatal "Error getting bell information from test" in
      begin match test_bi.BellInfo.regions with 
      | Some r -> check_regions r bi
      | _ -> ()
      end ;
      let st =
        match test_bi.BellInfo.scopes with 
        | Some s -> Some (check_scopes s bi)
        | _ -> None in        
      let test_bi = { test_bi with BellInfo.scopes=st;} in
      { parsed with MiscParser.bell_info = Some test_bi; }

    let check = match C.info with
    | None -> Misc.identity
    | Some bi -> fun parsed -> do_check parsed bi
  end
