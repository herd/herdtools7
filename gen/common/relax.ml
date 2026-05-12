(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open Code

module type S = sig
  type fence
  type dp
  type edge


  (* `relax`, a sequence of edges. *)
  type relax = edge list

  val ac_fence : fence -> sd -> extr -> extr -> relax
  val bc_fence : fence -> sd -> extr -> extr -> relax

  val compare : relax -> relax -> int
  val pp_relax : relax -> string
  val pp_relax_list : relax list -> string
  val edges_of_relax_list : relax list -> edge list

  val com : relax list
  val po : relax list

  (* Parse the `input` to `Ast.t` using the input grammar *)
  val parse_ast : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> (string,string) Ast.t) -> string -> (string,string) Ast.t
  val parse_sequence_ast : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> (string,string) Ast.t) -> string list -> (string,string) Ast.t
  (* Parse the input relaxation (or relaxations sequences), and expand the wildcard
     syntax into primitive edges and annotations *)
  val parse_expand_relaxs_ast :
    ?ppo:((relax -> relax list -> relax list) -> relax list -> relax list)
        -> (string,string) Ast.t -> (string,edge) Ast.t
  val parse_expand_relaxs :
    ?ppo:((relax -> relax list -> relax list) -> relax list -> relax list)
        -> (string,string) Ast.t -> relax list

  (* Remove invalid relax from the list *)
  val remove_invalid_relaxes : relax list -> relax list

(* Sets *)
  module Set : MySet.S with type elt = relax
  val pp_set : out_channel -> Set.t -> unit

  (* All fences present *)
  val all_fences : Set.t -> fence list

  (* All cumulative fences present *)
  val all_cumul_fences : Set.t -> fence list

  (* Presence of cumulativity relaxations in a set *)
  val cumul_in : Set.t -> bool

  (* Remove cumulativity relaxations from set *)
  val remove_cumul : Set.t -> Set.t

  (* Expand cumulativity relaxations in set *)
  val expand_cumul : Set.t -> Set.t

  module SetSet : MySet.S with type elt = Set.t
  val pp_set_set : out_channel -> SetSet.t -> unit

  (* Apply expand cumul to all sets in a set of sets *)
  val expand_cumuls : SetSet.t -> SetSet.t

(* Map *)
  module Map : Map.S with type key = relax

(* From edge cycle to relax cycle *)
  val relaxs_of : Set.t -> edge list -> SetSet.t

(* Sequence (po) relaxations *)
  val compact_sequence : relax -> relax -> Set.t
end

module Make
    (F:Fence.S)
    (E:Edge.S with type fence = F.fence and type dp = F.dp) : S
with type fence = E.fence
and type dp = E.dp
and type edge = E.edge
      = struct
        type fence = E.fence
        type dp = E.dp
        type edge = E.edge

        type relax = edge list

        let edges_of_relax_list = List.flatten

        let compare r1 r2 =
          List.compare E.compare r1 r2

(* Pretty print, macros are filtered and printed specially *)
        let pp_relax r =
          let open E in
          match r with
          | [e] -> E.pp_edge e
          | es ->
              sprintf "[%s]" (String.concat "," (List.map pp_edge es))

        let pp_relax_list lr = String.concat " " (List.map pp_relax lr)

(* Cumulativity macros *)
        let rf = E.plain_edge (E.Communication (Code.Rf,Ext))
        and fenced f sl d1 d2 = E.plain_edge (E.Fenced (f,sl,d1,d2))
        let ac_fence f sl d1 d2 = [rf; fenced f sl d1 d2]
        let bc_fence f sl d1 d2 = [fenced f sl d1 d2; rf]

(***********)
(* Parsing *)
(***********)

(*
  Same idea as for edges: pretty print all relaxations
  so as to build a table of recognized relaxations.
 *)

(*************************************************************)
(* Expansion of irrelevant direction specifications in edges *)
(*************************************************************)
        let er e = [E.plain_edge e]
        let ers es = List.map E.plain_edge es
        let com =
          let open E in
          [
           er (Communication (Rf,Ext));
           er (Communication (Fr,Ext));
           er (Communication (Co,Ext));
           ers [Communication (Fr,Ext) ; Communication (Rf,Ext);];
           ers [Communication (Co,Ext); Communication (Rf,Ext);];
         ]

        let po =
          let open E in
          er (Po (Diff,Irr,Irr))::
          F.fold_all_fences
            (fun f k ->
              er (Fenced (f,Diff,Irr,Irr))::
              (if F.orders f R R && not (F.orders f W R) then
                [ers [Communication (Rf,Int); Fenced (f,Diff,Dir R,Dir R)]]
              else [])@
              (if F.orders f R W && not (F.orders f W W) then
                [ers [Communication (Rf,Int); Fenced (f,Diff,Dir R,Dir W)]]
              else [])@k)
            []


(* Expand relax macros *)
        let er e = [E.plain_edge e]
        let atoms_key = "atoms"

        let atoms_length = String.length atoms_key

        let _esparse_atoms s =
          if
            String.length s >= atoms_length &&
            String.sub s 0 atoms_length = atoms_key
          then
            let suf =
              String.sub s atoms_length (String.length s - atoms_length) in
            try Some (E.parse_edge suf)
            with _ -> None
          else None

        let parse_ast parser_grammar s =
          try
            Lexing.from_string s
            |> LexUtil.parse parser_grammar
          with
          | Parser.Error ->
              Warn.user_error "Bad relax syntax: %s" s

        let parse_sequence_ast parser_grammar segments =
          Ast.Seq (List.map (parse_ast parser_grammar) segments)

        module MacroTable = struct
          (* wildcard syntax, where one name can unfold to multiple choices *)
          let wildcard = Hashtbl.create 3000
          (* legacy syntax *)
          let legacy_syntax = Hashtbl.create 3000

          let add_to bucket name choices =
            Hashtbl.add bucket name choices

          let add_wildcard = add_to wildcard
          let add_legacy_syntax = add_to legacy_syntax

          let find_opt name = match Hashtbl.find_opt wildcard name with
          | Some _ as r -> r
          | None -> Hashtbl.find_opt legacy_syntax name

          let add_default_dp_alias tag dpo sd e = match dpo with
          | None -> ()
          | Some dp ->
            let name = sprintf "%s%s%s" tag (pp_sd sd) (pp_extr e) in
            let choices = E.expand_edges (er (E.Dp (dp,sd,e))) Misc.cons [] in
            add_legacy_syntax name choices

          let add_cumulativity_alias tag e r =
            let choices = E.expand_edges r Misc.cons [] in
            add_legacy_syntax (sprintf "%s%s" tag (E.pp_edge e)) choices

          let abc_fence f sl d1 d2 = [rf; fenced f sl d1 d2; rf]
          let bc_dp dp sl d = [E.plain_edge (E.Dp (dp,sl,d)); rf]

          let add_cumulativity_macros () =
            let add_fence_aliases tag make_relax fe sd d1 d2 k =
              let e = fenced fe sd d1 d2 in
              add_cumulativity_alias tag e (make_relax fe sd d1 d2) ;
              k in
            let k =
              F.fold_cumul_fences
                (fun fe k ->
                  let k =
                    Code.fold_sd E.wildcard
                      (fun sd k ->
                        let k =
                          add_fence_aliases "ABC" abc_fence fe sd Irr Irr k in
                        add_fence_aliases "ABC" abc_fence fe sd (Dir R) (Dir W) k)
                      k in
                  Code.fold_sd_extr E.wildcard
                    (fun sd e k ->
                      let k =
                        add_fence_aliases "AC" ac_fence fe sd Irr e k in
                      let k =
                        add_fence_aliases "AC" ac_fence fe sd (Dir R) e k in
                      let k =
                        add_fence_aliases "BC" bc_fence fe sd e Irr k in
                      add_fence_aliases "BC" bc_fence fe sd e (Dir W) k)
                    k) () in
            F.fold_dpw
              (fun dpw k ->
                Code.fold_sd E.wildcard
                  (fun sd k ->
                    let e = E.plain_edge (E.Dp (dpw,sd,Dir W)) in
                    add_cumulativity_alias "BC" e (bc_dp dpw sd (Dir W)) ;
                    k)
                  k)
              k

          let add_default_dp_macros () =
            fold_sd E.wildcard
              (fun sd () ->
                if E.wildcard then begin
                  add_default_dp_alias "Dp" F.ddr_default sd Irr ;
                  add_default_dp_alias "Ctrl" F.ctrlr_default sd Irr
                end ;
                add_default_dp_alias "Dp" F.ddr_default sd (Dir R) ;
                add_default_dp_alias "Ctrl" F.ctrlr_default sd (Dir R) ;
                add_default_dp_alias "Dp" F.ddw_default sd (Dir W) ;
                add_default_dp_alias "Ctrl" F.ctrlw_default sd (Dir W))
              ()

          let add_strong_fence_macros () =
            fold_sd_extr_extr E.wildcard
              (fun sd e1 e2 () ->
                let name =
                  sprintf "Fence%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2) in
                let edge = E.plain_edge (E.Fenced (F.strong,sd,e1,e2)) in
                add_legacy_syntax name (E.expand_edges [edge] Misc.cons []))
              ()

          let add_ifetch_macros () =
            match E.do_self,E.instr_atom with
            | true,(Some _ as instr_atom) ->
              let rf ie = { E.edge=E.Communication (Code.Rf,ie); a1=None; a2=instr_atom }
              and fr ie = { E.edge=E.Communication (Code.Fr,ie); a1=instr_atom; a2=None } in
              let add_ifetch_alias name edge ie =
                add_legacy_syntax (sprintf "%s%s" name (Code.pp_ie ie)) [[edge ie]] in
              let add_ifetch_alias_macro name edge =
                fold_ie
                  (fun ie k -> [edge ie]::k)
                  []
                |> add_legacy_syntax (sprintf "%s*" name) in
              if E.wildcard then begin
                add_ifetch_alias_macro "Iff" rf ;
                add_ifetch_alias_macro "Irf" rf ;
                add_ifetch_alias_macro "Fif" fr ;
                add_ifetch_alias_macro "Ifr" fr
              end ;
              fold_ie
                (fun ie () ->
                  add_ifetch_alias "Iff" rf ie ;
                  add_ifetch_alias "Irf" rf ie ;
                  add_ifetch_alias "Fif" fr ie ;
                  add_ifetch_alias "Ifr" fr ie)
                ()
            | _ -> ()

          let all_fences sd d1 d2 =
            F.fold_all_fences
              (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)

          let some_fences sd d1 d2 =
            F.fold_some_fences
              (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)

          let app_def_dp o f r = match o with
          | None -> r
          | Some dp -> f dp r

          let someR sd d =
            er (E.Po (sd,Dir R,Dir d))::
            app_def_dp
              (match d with R -> F.ddr_default | W -> F.ddw_default)
              (fun dp k -> er (E.Dp (dp,sd,Dir d))::k)
              (some_fences sd R d [])

          let someW sd d =
            er (E.Po (sd,Dir W,Dir d))::
            (some_fences sd W d [])

          let allR sd d =
            er (E.Po (sd,Dir R,Dir d))::
            (match d with R -> F.fold_dpr | W -> F.fold_dpw)
              (fun dp k -> er (E.Dp (dp,sd,Dir d))::k)
              (all_fences sd R d [])

          let allW sd d =
            er (E.Po (sd,Dir W,Dir d))::
            (all_fences sd W d [])

          let add_predefined_legacy_syntax () =
            List.iter
              (fun (name, choices) -> add_legacy_syntax name choices)
              [
                "allRR", allR Diff R;
                "allRW", allR Diff W;
                "allWR", allW Diff R;
                "allWW", allW Diff W;
                "someRR", someR Diff R;
                "someRW", someR Diff W;
                "someWR", someW Diff R;
                "someWW", someW Diff W;
              ]

          let add_edge_wildcards () =
            E.fold_edge_wildcards
              (fun name choices () -> add_wildcard name choices)
              ()

          let add_edge_legacy_aliases () =
            E.fold_edge_legacy_aliases
              (fun name choices () -> add_legacy_syntax name choices)
              ()

          let add_edge_legacy_wildcards () =
            E.fold_edge_legacy_wildcards
              (fun name choices () -> add_legacy_syntax name choices)
              ()

          let () =
            (* Backward-compatible aliases for A-, B-, and AB-cumulativity candidates. *)
            add_cumulativity_macros () ;
            (* Backward-compatible defaults for dependency aliases. *)
            add_default_dp_macros () ;
            (* Backward-compatible aliases for strong fence edges. *)
            add_strong_fence_macros () ;
            (* Backward-compatible aliases for instruction-fetch edges. *)
            add_ifetch_macros () ;
            (* Legacy edge-level aliases for relax parsing. *)
            add_edge_legacy_aliases () ;
            (* Edge-level wildcards for relax parsing. *)
            if E.wildcard then add_edge_wildcards () ;
            (* Legacy edge-level wildcards for relax parsing. *)
            if E.wildcard then add_edge_legacy_wildcards () ;
            (* Legacy wildcards for pre-defined edge sets. *)
            if E.wildcard then add_predefined_legacy_syntax ()
        end

        let expand_relaxs rs =
          let expand_relax r = E.expand_edges r Misc.cons in
          List.fold_right expand_relax rs []

        let relax_to_sequence relax = match relax with
        | [] -> Warn.fatal "Relax is parsed incorrectly."
        | [edge] -> Ast.One edge
        | edges -> Ast.Seq (List.map (fun edge -> Ast.One edge) edges)

        let relax_list_to_choice relax_list =
          let ast_relax_list =
            List.map relax_to_sequence relax_list in
          match ast_relax_list with
          | [] -> assert false
          | [relax] -> relax
          | relax_list -> Ast.Choice relax_list

        (* Apply an annotation suffix parsed from a macro name such as `PodW*LA`
           to each singleton expansion of that macro. *)
        let add_macro_annotations a1 a2 =
          List.map
            (function
              | [e] -> [{e with E.a1; E.a2}]
              | relax -> relax)

        (* Find the longest prefix that is a macro.  Since the search starts
           from the full string length, this also covers exact macro names. *)
        let rec find_macro_prefix str i =
          if i <= 0 then None
          else
            let prefix = String.sub str 0 i in
            (* Macro table lookup. *)
            match MacroTable.find_opt prefix with
            | Some relax ->
                let suffix = String.sub str i (String.length str - i) in
                if String.length suffix = 0 then Some relax
                else
                  (* If there is a suffix, try to parse it as two annotations. *)
                  begin match E.parse_edge_annotations suffix with
                  | Some (a1,a2) -> Some (add_macro_annotations a1 a2 relax)
                  | None -> find_macro_prefix str (i - 1)
                  end
            | None -> find_macro_prefix str (i - 1)

        let parse_expand_relax ?(ppo=(fun _ k -> k)) str =
          let unfold_ppo () =
            let relaxs = ppo Misc.cons [] in
            match relaxs with
            | [] -> Warn.fatal "Bad relax: PPO"
            | r -> r in
          let parsed_edges = match str with
          (* Directly unfold macro *)
          | "PPO" -> unfold_ppo ()
          | str ->
              (* Macro lookup *)
              begin match find_macro_prefix str (String.length str) with
              | Some relax -> relax
              | None ->
                  (* Parse primitive edge *)
                  try [[E.parse_edge str]]
                  with Misc.UserError _ as err -> raise err
                     | _ -> Warn.fatal "Bad relax: %s" str
              end in
          (* expand the wildcard edges and annotations *)
          expand_relaxs parsed_edges
          |> relax_list_to_choice

          let parse_expand_relaxs_ast ?(ppo=(fun _ k -> k)) ast =
            Ast.bind ast (parse_expand_relax ~ppo)

          let parse_expand_relaxs ?(ppo=(fun _ k -> k)) ast =
            parse_expand_relaxs_ast ~ppo ast
              |> Ast.expand
                   (fun pred _ ->
                     Warn.user_error
                       "predicate %s is only supported by diy7 search." pred)

        (* After wildcard and macro expansion, remove invalid relaxations
           whose adjacent concrete edges cannot appear consecutively.
           Pseudo-edges (annotations and insert edge) are ignored in the check.
           Duplications are removed as well. *)
        let remove_invalid_relaxes relaxes =
          let rec for_all_adjacent_edge participates predicate = function
            | [] | [_] -> true
            | lhs :: rhs :: list ->
                match participates lhs.E.edge, participates rhs.E.edge with
                | true, true ->
                    predicate lhs rhs
                    && for_all_adjacent_edge participates predicate (rhs :: list)
                | true, false ->
                    for_all_adjacent_edge participates predicate (lhs :: list)
                | false, true ->
                    for_all_adjacent_edge participates predicate (rhs :: list)
                | false, false ->
                    for_all_adjacent_edge participates predicate list in
          relaxes
          |> List.filter
               (fun relax ->
                 (* Drop empty alternatives introduced by `?`; they do not
                    describe an actual relaxation. *)
                 relax <> []
                 && for_all_adjacent_edge E.is_non_pseudo E.can_precede relax
                 (* Validate annotations separately so that atom propagation
                    can reject invalid RMW combinations such as
                    [A,Amo.StAdd,L]. *)
                 && for_all_adjacent_edge
                      (fun edge -> not (E.is_insert_store edge))
                      E.can_precede relax)
          |> List.sort_uniq compare

(********)
(* Sets *)
(********)

        module Set =
          MySet.Make
            (struct
              type t = relax
              let compare = compare
            end)


        let pp_set chan t =
          fprintf chan "{" ;
          Set.pp chan ", "
            (fun chan r -> fprintf chan "%s" (pp_relax r))
            t ;
          fprintf chan "}"

        let is_cumul r =
          let open E in
          match r with
          | [{edge=Communication (Rf,Ext); a1=None; a2=None;};
             {edge=Fenced _; a1=None; a2=None;}]
          | [{edge=Fenced _; a1=None; a2=None;};
             {edge=Communication (Rf,Ext); a1=None; a2=None;};]
          | [{edge=Communication (Rf,Ext); a1=None; a2=None;};
             {edge=Fenced _; a1=None; a2=None;};
             {edge=Communication (Rf,Ext); a1=None; a2=None;};]
            -> true
          | _ -> false

        module FenceSet =
          MySet.Make
            (struct
              type t = F.fence
              let compare = F.compare_fence
            end)

        let add_fence r k =
          let open E in
          match r with
          | [{edge=Fenced (f,_,_,_); _}]
          | [{edge=Communication (Rf,Ext); _};{edge=Fenced (f,_,_,_);_}]
          | [{edge=Fenced (f,_,_,_); _};
             {edge=Communication (Rf,Ext); _};]
          | [{edge=Communication (Rf,Ext); _};
             {edge=Fenced (f,_,_,_); _};
             {edge=Communication (Rf,Ext); _};]
            -> FenceSet.add f k
          | _ -> k

        let all_fences rs =
          let fs = Set.fold  add_fence rs FenceSet.empty in
          FenceSet.elements fs

        module RSet = Set

        let add_cumul_fence r k =
          let open E in
          match r with
          | [{edge=Communication (Rf,Ext); _};{edge=Fenced (f,_,_,_); _}]
          | [{edge=Fenced (f,_,_,_); _};
             {edge=Communication (Rf,Ext); _};]
          | [{edge=Communication (Rf,Ext); _};
             {edge=Fenced (f,_,_,_); _};
             {edge=Communication (Rf,Ext); _};]
            -> FenceSet.add f k
          | _ -> k

        let all_cumul_fences rs =
          let fs = Set.fold  add_cumul_fence rs FenceSet.empty in
          FenceSet.elements fs

        let cumul_in rs =  Set.exists is_cumul rs

        let remove_cumul rs = Set.filter (fun r -> not (is_cumul r)) rs

        let expand_cumul rs =
          let er e = [e] in
          let xs =
            Set.fold
              (fun r k ->
                let open E in
                match r with
                | ([{edge=Communication (Rf,Ext); _}; {edge=Fenced _; _};] as rs)
                | ([{edge=Fenced _; _}; {edge=Communication (Rf,Ext); _};] as rs)
                | ([{edge=Communication (Rf,Ext); _}; {edge=Fenced _; _};
                    {edge=Communication (Rf,Ext); _};] as rs)
                  ->
                    RSet.of_list (List.map er rs)::k
                | _ -> RSet.singleton r::k)
              rs [] in
          RSet.unions xs


        module SetSet = MySet.Make(Set)

        let pp_set_set chan ts = SetSet.pp chan " " pp_set ts

        let expand_cumuls rss =
          let xs =
            SetSet.fold
              (fun rs k -> expand_cumul rs::k)
              rss [] in
          SetSet.of_list  xs

(*********)

        module Map =
          Map.Make
            (struct
              type t = relax
              let compare = compare
            end)



(***************************************)
(* From edge cycle back to relaxations *)
(***************************************)

        let shift = function
          | [] -> assert false
          | x::xs -> xs @ [x]

        let rec match_edges ps es = match ps,es with
        | [],_ -> Some ([],es)
        | _::_,[] -> None
        | p::ps,e::es ->
            if p=e then match match_edges ps es with
            | Some (h,rem) -> Some (e::h,rem)
            | None -> None
            else None

        let rec match_head rs es =
          Set.fold
            (fun r k ->
              match match_edges r es with
              | None -> k
              | Some (h,rem) ->
                  List.fold_left
                    (fun k rs -> (h::rs)::k)
                    k (matches rs rem))
            rs []

        and matches rs es = match es with
        | [] -> [[]]
        | _ -> match_head rs es

        let match_set rss = SetSet.of_list (List.map Set.of_list rss)

        let relaxs_of rs es =
          let rec do_rec k es =
            if k <= 0 then []
            else match_set (matches rs es)::do_rec (k-1) (shift es) in
          SetSet.unions (do_rec (List.length es) es)


        let compact_sequence es1 es2 =
          let e1 = Misc.last es1 and e2 = List.hd es2 in
          begin match E.get_ie e1, E.get_ie e2 with
          | Int,Int when E.can_precede e1 e2 ->
              E.compact_sequence es1 es2 e1 e2
              |> Set.of_list
          | _,_ -> Set.empty
          end
      end
