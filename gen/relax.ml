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
  val bc_dp : dp -> sd -> extr -> relax

(* Call function over all reckognized relaxations *)
  val fold_relax : bool -> (relax -> 'a -> 'a) -> 'a -> 'a

  val compare : relax -> relax -> int
  val pp_relax : relax -> string
  val pp_relax_list : relax list -> string
  val edges_of_relax_list : relax list -> edge list

  val com : relax list
  val po : relax list

  (* Parse the `input` to `Ast.t` using the input grammar *)
  val parse_ast : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> string Ast.t) -> string -> string Ast.t
  (* Parse the input relaxation (or relaxations sequences), and expand the wildcard
     syntax into primitive edges and annotations *)
  val parse_sequence_ast : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> string Ast.t) -> string list -> string Ast.t
  val parse_expand_relaxs :
    ?ppo:((relax -> relax list -> relax list) -> relax list -> relax list)
        -> string Ast.t -> relax list

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



(* Cumulativity macros *)
        let rf = E.plain_edge (E.Rf Ext)
        and fenced  f sl d1 d2 = E.plain_edge (E.Fenced (f,sl,d1,d2))
        let ac_fence f sl d1 d2 = [rf; fenced f sl d1 d2]
        let bc_fence f sl d1 d2 = [fenced f sl d1 d2; rf]
        let abc_fence f sl d1 d2 = [rf; fenced f sl d1 d2; rf]
        let bc_dp dp sl d = [E.plain_edge (E.Dp (dp,sl,d)); rf]

(* Pretty print, macros are filtered and printed specially *)
        let internal_pp_relax backward_compatibility r =
          let open E in
          match r with
          | [e] -> E.pp_edge e
          | [{edge=Rf Ext; a1=None;a2=None;};
             {edge=Fenced _;a1=None; a2=None;} as e] when backward_compatibility ->
                 sprintf "AC%s" (pp_edge e)
          | [{edge=Fenced _; a1=None;a2=None;} as e;
             {edge=Rf Ext; a1=None; a2=None;}] when backward_compatibility ->
                   sprintf "BC%s" (pp_edge e)
          | [{edge=Rf Ext; a1=None; a2=None;};
             {edge=Fenced _; a1=None; a2=None;} as e;
             {edge=Rf Ext; a1=None; a2=None;}] when backward_compatibility ->
                   sprintf "ABC%s" (pp_edge e)
          | [{edge=Dp _; a1=None; a2=None;} as e;
             {edge=Rf Ext; a1=None; a2=None;}] when backward_compatibility ->
                   sprintf "BC%s" (pp_edge e)
          | es ->
              sprintf "[%s]" (String.concat "," (List.map pp_edge es))

        let pp_relax = internal_pp_relax false

        let pp_relax_list lr = String.concat " " (List.map pp_relax lr)

(* Fold over all relaxations *)

        let fold_relax wildcard f k =
          let k = E.fold_edges (fun e -> f [e]) k in
          let k =
            F.fold_cumul_fences
              (fun fe k ->
                let k =
                  Code.fold_sd wildcard
                    (fun sd k ->
                      let k = f (abc_fence fe sd Irr Irr) k in
                      f (abc_fence fe sd (Dir R) (Dir W)) k)
                    k in
                Code.fold_sd_extr wildcard
                  (fun sd e k ->
                    let k = f (ac_fence fe sd Irr e) k in
                    let k = f (ac_fence fe sd (Dir R) e) k in
                    let k = f (bc_fence fe sd e Irr) k in
                    f (bc_fence fe sd e (Dir W)) k)
                  k) k in
          let k =
            F.fold_dpw
              (fun dpw k ->
                Code.fold_sd wildcard
                  (fun sd k -> f (bc_dp dpw sd (Dir W)) k)
                  k) k in
          k

        let iter_relax wildcard = Misc.fold_to_iter (fold_relax wildcard)


(***********)
(* Parsing *)
(***********)

(*
  Same idea as for edges: pretty print all relaxations
  so as to build a table of recognized relaxations.
 *)

(* Lexeme table *)
        let t = Hashtbl.create 101

(* Fill up lexeme table *)
        let () =
          iter_relax E.wildcard
            (fun e ->
              let pp = internal_pp_relax true e in
              Hashtbl.add t pp e);
          ()

(*************************************************************)
(* Expansion of irrelevant direction specifications in edges *)
(*************************************************************)
        let expand_relaxs rs =
          let expand_relax r = E.expand_edges r Misc.cons in
          List.fold_right expand_relax rs []

        let er e = [E.plain_edge e]
        let ers es = List.map E.plain_edge es
        let com =
          let open E in
          [
           er (Rf Ext);
           er (Fr Ext);
           er (Ws Ext);
           ers [Fr Ext ; Rf Ext;];
           ers [Ws Ext; Rf Ext;];
         ]

        let po =
          let open E in
          er (Po (Diff,Irr,Irr))::
          F.fold_all_fences
            (fun f k ->
              er (Fenced (f,Diff,Irr,Irr))::
              (if F.orders f R R && not (F.orders f W R) then
                [ers [Rf Int; Fenced (f,Diff,Dir R,Dir R)]]
              else [])@
              (if F.orders f R W && not (F.orders f W W) then
                [ers [Rf Int; Fenced (f,Diff,Dir R,Dir W)]]
              else [])@k)
            []


(* Expand relax macros *)
        let er e = [E.plain_edge e]

        let all_fences sd d1 d2 =
          F.fold_all_fences
            (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)

        let some_fences sd d1 d2 =
          F.fold_some_fences
            (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)

        let relax_list_to_choice relax_list =
          let ast_relax_list =
            List.map (fun relax -> Ast.One relax) relax_list in
          match ast_relax_list with
              | [] -> assert false
              | [relax] -> relax
              | relax_list -> Ast.Choice relax_list

(* Limited variations *)
        let app_def_dp o f r = match o with
        | None -> r
        | Some dp -> f dp r

        let someR sd d =
          er (E.Po (sd,Dir R,Dir d))::
          app_def_dp
            (match d with R -> F.ddr_default | W -> F.ddw_default)
            (fun dp k -> er (E.Dp (dp,sd,Dir d))::k)
            (some_fences sd R d [])
          |> relax_list_to_choice

        let someW sd d =
          er (E.Po (sd,Dir W,Dir d))::
          (some_fences sd W d [])
          |> relax_list_to_choice


(* ALL *)
        let allR sd d =
          er (E.Po (sd,Dir R,Dir d))::
          (match d with R -> F.fold_dpr | W -> F.fold_dpw)
            (fun dp k -> er (E.Dp (dp,sd,Dir d))::k)
            (all_fences sd R d [])
          |> relax_list_to_choice

        let allW sd d =
          er (E.Po (sd,Dir W,Dir d))::
          (all_fences sd W d [])
          |> relax_list_to_choice

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

        (* After wildcard and macro expansion, remove invalid relaxations
           whose adjacent concrete edges cannot appear consecutively.
           Pseudo-edges (annotations and insert edge) are ignored in the check.
           Duplications are removed as well. *)
        let remove_invalid_relaxes relaxes =
          let rec for_all_adjacent_concrete_edge predicate = function
            | [] | [_] -> true
            | lhs :: rhs :: list ->
                match E.is_non_pseudo lhs.E.edge, E.is_non_pseudo rhs.E.edge with
                | true, true ->
                    predicate lhs rhs
                    && for_all_adjacent_concrete_edge predicate (rhs :: list)
                | true, false ->
                    for_all_adjacent_concrete_edge predicate (lhs :: list)
                | false, true ->
                    for_all_adjacent_concrete_edge predicate (rhs :: list)
                | false, false ->
                    for_all_adjacent_concrete_edge predicate list in
          List.filter
            (fun relax ->
              (* Drop empty alternatives introduced by `?`; they do not
                 describe an actual relaxation. *)
              relax <> []
              && for_all_adjacent_concrete_edge E.can_precede relax)
            relaxes
          |> List.sort_uniq compare

        let parse_expand_relax ?(ppo=(fun _ k -> k)) str =
          let unfold_ppo () =
            let relaxs = ppo Misc.cons [] in
            begin match relaxs with
            | [] -> Warn.fatal "Bad relax: PPO"
            | _ -> ()
            end ;
            expand_relaxs relaxs
            |> relax_list_to_choice in
          match str with
          (* Directly unfold macro *)
          | "PPO" -> unfold_ppo ()
          | "allRR" -> allR Diff R
          | "allRW" -> allR Diff W
          | "allWR" -> allW Diff R
          | "allWW" -> allW Diff W
          | "someRR" -> someR Diff R
          | "someRW" -> someR Diff W
          | "someWR" -> someW Diff R
          | "someWW" -> someW Diff W
          | str ->
            let relax = try [E.parse_edge str]
            (* For backward compatibility, also accept the legacy pretty-printed
               names recorded in the special table `t`. *)
            with _ -> try Hashtbl.find t str
            with Not_found -> Warn.fatal "Bad relax: %s" str in
            [relax]
          (* expand the wildcard edges and annotations *)
          |> expand_relaxs
          |> relax_list_to_choice

          let parse_expand_relaxs ?(ppo=(fun _ k -> k)) ast =
            Ast.bind ast (parse_expand_relax ~ppo)
              |> Ast.expand
              |> List.map edges_of_relax_list

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
          | [{edge=Rf Code.Ext; a1=None; a2=None;};
             {edge=Fenced _; a1=None; a2=None;}]
          | [{edge=Fenced _; a1=None; a2=None;};
             {edge=Rf Code.Ext; a1=None; a2=None;};]
          | [{edge=Rf Code.Ext; a1=None; a2=None;};
             {edge=Fenced _; a1=None; a2=None;};
             {edge=Rf Code.Ext; a1=None; a2=None;};]
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
          | [{edge=Rf Code.Ext; _};{edge=Fenced (f,_,_,_);_}]
          | [{edge=Fenced (f,_,_,_); _};
             {edge=Rf Code.Ext; _};]
          | [{edge=Rf Code.Ext; _};
             {edge=Fenced (f,_,_,_); _};
             {edge=Rf Code.Ext; _};]
            -> FenceSet.add f k
          | _ -> k

        let all_fences rs =
          let fs = Set.fold  add_fence rs FenceSet.empty in
          FenceSet.elements fs

        module RSet = Set

        let add_cumul_fence r k =
          let open E in
          match r with
          | [{edge=Rf Code.Ext; _};{edge=Fenced (f,_,_,_); _}]
          | [{edge=Fenced (f,_,_,_); _};
             {edge=Rf Code.Ext; _};]
          | [{edge=Rf Code.Ext; _};
             {edge=Fenced (f,_,_,_); _};
             {edge=Rf Code.Ext; _};]
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
                | ([{edge=Rf Ext; _}; {edge=Fenced _; _};] as rs)
                | ([{edge=Fenced _; _}; {edge=Rf Ext; _};] as rs)
                | ([{edge=Rf Ext; _}; {edge=Fenced _; _};
                    {edge=Rf Ext; _};] as rs)
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
