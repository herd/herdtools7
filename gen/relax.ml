(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code

module type S = sig
  type fence
  type dp
  type edge


  type relax =
(* Sequence of edges (eg Cumulativity) *)
    | ERS of edge list 
    | PPO

  val ac_fence : fence -> sd -> extr -> extr -> relax
  val bc_fence : fence -> sd -> extr -> extr -> relax
  val bc_dp : dp -> sd -> extr -> relax

(* Call function over all reckognized relaxations *)
  val fold_relax : (relax -> 'a -> 'a) -> 'a -> 'a

  val compare : relax -> relax -> int
  val pp_relax : relax -> string
  val pp_relax_list : relax list -> string
  val edges_of : relax -> edge list

(* Replace Irr directions in par expansion to W and R *)
  val expand_relaxs :
      ((relax -> relax list -> relax list) -> relax list -> relax list) ->
        relax list -> relax list
  val expand_relax_seq : relax list -> relax list list
  val com : relax list
  val po : relax list

(* parsing *)      
  val parse_relax : LexUtil.t -> relax
  val parse_relaxs : LexUtil.t list -> relax list
(* parsing, with macro expansion *)
  val expand_relax_macro : LexUtil.t -> relax list
 (* NB use for set of relaxations only *)
  val expand_relax_macros : LexUtil.t list -> relax list

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

        type relax =
          | ERS of edge list
          | PPO

        let edges_of r = match r with
        | ERS es -> es
        | PPO -> assert false


        let rec compare_edges es1 es2 = match es1,es2 with
        | [],[] -> 0
        | [],_::_ -> -1
        | _::_,[] -> 1
        | e1::es1,e2::es2 ->
            begin match E.compare e1 e2 with
            | 0 -> compare_edges es1 es2
            | r -> r
            end

        let compare r1 r2 = match r1,r2 with
        | PPO,PPO -> 0
        | PPO,ERS _ -> -1
        | ERS _,PPO -> 1
        | ERS l1,ERS l2 -> compare_edges l1 l2



(* Cumulativity macros *)
        let rf = E.plain_edge (E.Rf Ext)
        and fenced  f sl d1 d2 = E.plain_edge (E.Fenced (f,sl,d1,d2))
        let ac_fence f sl d1 d2 = ERS [rf; fenced f sl d1 d2]
        let bc_fence f sl d1 d2 = ERS [fenced f sl d1 d2; rf]
        let abc_fence f sl d1 d2 = ERS [rf; fenced f sl d1 d2; rf]
        let bc_dp dp sl d = ERS [E.plain_edge (E.Dp (dp,sl,d)); rf]

(* Pretty print, macros are filtered and printed specially *)
        let pp_relax r = 
          let open E in
          match r with
          | ERS [e] -> E.pp_edge e
          | ERS
              [{edge=Rf Ext; a1=None;a2=None;};
               {edge=Fenced _;a1=None; a2=None;} as e] ->
                 sprintf "AC%s" (pp_edge e)
          | ERS [{edge=Fenced _; a1=None;a2=None;} as e;
                 {edge=Rf Ext; a1=None; a2=None;}] ->
                   sprintf "BC%s" (pp_edge e)
          | ERS [{edge=Rf Ext; a1=None; a2=None;};
                 {edge=Fenced _; a1=None; a2=None;} as e;
                 {edge=Rf Ext; a1=None; a2=None;}] ->
                   sprintf "ABC%s" (pp_edge e)
          | ERS [{edge=Dp _; a1=None; a2=None;} as e;
                 {edge=Rf Ext; a1=None; a2=None;}] ->
                   sprintf "BC%s" (pp_edge e) 
          | ERS es ->
              sprintf "[%s]" (String.concat "," (List.map pp_edge es))
          | PPO -> "PPO"


        let pp_relax_list lr = String.concat " " (List.map pp_relax lr)

(* Fold over all relaxations *)

        let fold_relax f k =
          let k = E.fold_edges (fun e -> f (ERS [e])) k in
          let k = 
            F.fold_cumul_fences
              (fun fe k ->
                let k =
                  Code.fold_sd
                    (fun sd k ->
                      let k = f (abc_fence fe sd Irr Irr) k in
                      f (abc_fence fe sd (Dir R) (Dir W)) k)
                    k in
                Code.fold_sd_extr
                  (fun sd e k ->
                    let k = f (ac_fence fe sd Irr e) k in
                    let k = f (ac_fence fe sd (Dir R) e) k in
                    let k = f (bc_fence fe sd e Irr) k in
                    f (bc_fence fe sd e (Dir W)) k)
                  k) k in
          let k =
            F.fold_dpw
              (fun dpw k ->
                Code.fold_sd
                  (fun sd k -> f (bc_dp dpw sd (Dir W)) k)
                  k) k in
          let k = f PPO k in
          k

        let iter_relax f = fold_relax (fun r () -> f r) ()


(***********)
(* Parsing *)
(***********)

(*
  Same idea as for edges: pretty print all relaxations
  so as to build a table of reckognized relaxations.
 *)

(* Lexeme table *)
        let t = Hashtbl.create 101

(* Fill up lexeme table *)
        let () =
          iter_relax
            (fun e ->
              let pp = pp_relax e in
              Hashtbl.add t pp e);
          ()

        let do_parse_relax s = 
          try ERS [E.parse_edge s] (* Because some edges have special parsing *)
          with _ ->
            try Hashtbl.find t s
            with Not_found ->
              Warn.fatal "Bad relax: %s" s


(*************************************************************)
(* Expansion of irrelevant direction specifications in edges *)
(*************************************************************)
        let rec do_expand_relax ppo r f = match r with
        | ERS es -> E.expand_edges es (fun es -> f (ERS es))
        | PPO  -> ppo (fun r -> do_expand_relax ppo r f)
              

        let expand_relaxs ppo rs =
          let expand_relax r =  do_expand_relax ppo r Misc.cons in
          List.fold_right expand_relax rs []

        let rec cross_cons rs rss = match rs with
        | [] -> []
        | r::rs ->
            List.fold_right (fun rs k -> (r::rs)::k)
              rss
              (cross_cons rs rss)

        let expand_relax_seq rs =

          let rec expn rs = match rs with
          | [] -> [[]]
          | PPO ::_-> Warn.fatal "PPO in expand_relax_seq" 
          | ERS es::rem ->
              let rs =
                E.expand_edges es (fun es k -> ERS es::k) [] in
              let rss = expn rem in
              cross_cons rs rss in

          expn rs


        let er e = ERS [E.plain_edge e]
        let ers es = ERS (List.map E.plain_edge es)
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
            
            
        open LexUtil

        let parse_relax = function
          | One r -> do_parse_relax r
          | Seq [] -> Warn.fatal "Empty relaxation"
          | Seq es -> ERS (List.map E.parse_edge es)

        let parse_relaxs = List.map parse_relax

(* Expand relax macros *)
        let er e = ERS [E.plain_edge e]

        let all_fences sd d1 d2 =
          F.fold_all_fences
            (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)

        let some_fences sd d1 d2 =
          F.fold_some_fences
            (fun f k -> er (E.Fenced (f,sd,Dir d1,Dir d2))::k)
            
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

        let someW sd d =
          er (E.Po (sd,Dir W,Dir d))::
          (some_fences sd W d [])      

            
(* ALL *)
        let allR sd d =
          er (E.Po (sd,Dir R,Dir d))::
          (match d with R -> F.fold_dpr | W -> F.fold_dpw)
            (fun dp k -> er (E.Dp (dp,sd,Dir d))::k)
            (all_fences sd R d [])      

        let allW sd d =
          er (E.Po (sd,Dir W,Dir d))::
          (all_fences sd W d [])      

        let atoms_key = "atoms"

        let atoms_length = String.length atoms_key

        let parse_atoms s =
          if
            String.length s >= atoms_length &&
            String.sub s 0 atoms_length = atoms_key
          then
            let suf =
              String.sub s atoms_length (String.length s - atoms_length) in
            try Some (E.parse_edge suf)
            with _ -> None
          else None

        let expand_relax_macro = function
          | One s ->
              begin match s with
              | "allRR" -> allR Diff R
              | "allRW" -> allR Diff W
              | "allWR" -> allW Diff R
              | "allWW" -> allW Diff W
              | "someRR" -> someR Diff R
              | "someRW" -> someR Diff W
              | "someWR" -> someW Diff R
              | "someWW" -> someW Diff W
              | _ ->
                  match parse_atoms s with
                  | None ->  [do_parse_relax s]
                  | Some r ->
                      let module V = VarAtomic.Make(E)  in
                      let es = V.var_both r in
                      List.map (fun r -> ERS [r]) es
              end
          | Seq [] -> Warn.fatal "Empty relaxation"
          | Seq es -> [ERS (List.map E.parse_edge es)]

        let expand_relax_macros lus =
          let rs = List.map expand_relax_macro lus in
          let rs = List.flatten rs in
          rs

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
          | ERS
              [{edge=Rf Code.Ext; a1=None; a2=None;};
               {edge=Fenced _; a1=None; a2=None;}]
          | ERS
              [{edge=Fenced _; a1=None; a2=None;};
               {edge=Rf Code.Ext; a1=None; a2=None;};]
          | ERS
              [{edge=Rf Code.Ext; a1=None; a2=None;};
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
          | ERS
              [{edge=Fenced (f,_,_,_); _}]
          | ERS
              [{edge=Rf Code.Ext; _};{edge=Fenced (f,_,_,_);_}]
          | ERS
              [{edge=Fenced (f,_,_,_); _};
               {edge=Rf Code.Ext; _};]
          | ERS
              [{edge=Rf Code.Ext; _};
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
          | ERS
              [{edge=Rf Code.Ext; _};{edge=Fenced (f,_,_,_); _}]
          | ERS
              [{edge=Fenced (f,_,_,_); _};
               {edge=Rf Code.Ext; _};]
          | ERS
              [{edge=Rf Code.Ext; _};
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
          let er e = ERS [e] in
          let xs = 
            Set.fold
              (fun r k ->
                let open E in
                match r with
                | ERS 
                    ([{edge=Rf Ext;}; {edge=Fenced _;};] as rs)
                | ERS
                    ([{edge=Fenced _;}; {edge=Rf Ext;};] as rs)
                | ERS
                    ([{edge=Rf Ext;}; {edge=Fenced _;};
                      {edge=Rf Ext;};] as rs)
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
              let ps = edges_of r in
              match match_edges ps es with
              | None -> k
              | Some (h,rem) ->
                  List.fold_left
                    (fun k rs -> (ERS h::rs)::k)
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


        let compact_sequence r1 r2 = match r1,r2 with
        | ERS es1,ERS es2 ->
            let e1 = Misc.last es1 and e2 = List.hd es2 in
            begin match E.get_ie e1, E.get_ie e2 with
            | Int,Int when E.can_precede e1 e2 ->
                let ess = E.compact_sequence es1 es2 e1 e2 in
                let rs = List.map (fun es -> ERS es) ess in
                Set.of_list rs
            | _,_ -> Set.empty
            end
        | _,_ -> assert false
      end
