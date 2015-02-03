(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Edges, ie specifications of an event pair in a model relation  *)

module type S = sig
  open Code

  type fence
  type dp
  type atom

  val pp_atom : atom -> string

  val strong : fence
  val pp_fence : fence -> string

(* edge proper *)
  type tedge =
    | Rf of ie | Fr of ie | Ws of ie
    | Detour of extr     (* source direction *)
    | DetourWs of extr   (* source direction *)
    | Po of sd*extr*extr | Fenced of fence*sd*extr*extr
    | Dp of dp*sd*extr
    | Store (* Insert a store at thread start *)
    | Leave of com (* Leave thread *)
    | Back  of com (* Return to thread *)
(* fancy *)
    | Hat
    | RfStar of ie
    | Rmw

  type edge = { edge: tedge;  a1:atom option; a2: atom option; }
  val plain_edge : tedge -> edge
  
  val fold_atomo : (atom option -> 'a -> 'a) -> 'a -> 'a
  val fold_edges : (edge -> 'a -> 'a) -> 'a -> 'a
  val iter_edges : (edge -> unit) -> unit


  val fold_pp_edges : (string -> 'a -> 'a) -> 'a -> 'a

  val pp_tedge : tedge -> string
  val pp_edge : edge -> string
  val compare_atomo : atom option -> atom option -> int
  val compare : edge -> edge -> int

  val parse_fence : string -> fence
  val parse_edge : string -> edge
  val parse_edges : string -> edge list

  val pp_edges : edge list -> string

(* Raised by get/set function on Store edge *)
  exception IsStore of string

(* Get source and target event direction,
   Returning Irr means that a Read OR a Write is acceptable *)  
  val dir_src : edge -> extr 
  val dir_tgt : edge -> extr

(* Return edge with direction resolved *)
  val set_src : dir ->  edge -> edge
  val set_tgt : dir ->  edge -> edge

(* Does source and target events have the same or different locations? *)
  val loc_sd : edge -> sd

(* Internal (same proc) or external edge (different procs) *)
  val get_ie : edge -> ie
(* More detailed *)
  type full_ie = IE of ie | LeaveBack
  val get_full_ie : edge -> full_ie

(* Can e1 target event direction be the same as e2 source event? *)
  val can_precede : edge -> edge -> bool

(* Expansion of Irr directions *)
  val expand_edges : edge list -> (edge list -> 'a -> 'a) -> 'a -> 'a

(* Resolve Irr directions and unspecified atom *)
  val resolve_edges : edge list -> edge list

(* Possible interpretation of edge sequence as an edge *)
  val compact_sequence : edge list -> edge list -> edge -> edge -> edge list list
(* Utilities *)
  val is_detour : edge -> bool
  val is_store : edge -> bool
  val is_ext : edge -> bool

(* Set *)
  module Set : MySet.S with type elt = edge


end


module Make(F:Fence.S) : S
with type fence = F.fence
and type dp = F.dp 
and type atom = F.atom = struct

  open Code

  type fence = F.fence
  type dp = F.dp
  type atom = F.atom

  let pp_atom = F.pp_atom

  let pp_fence = F.pp_fence

  let strong = F.strong

(* edge proper *)
  type tedge =
    | Rf of ie | Fr of ie | Ws of ie
    | Detour of extr
    | DetourWs of extr
    | Po of sd*extr*extr | Fenced of fence*sd*extr*extr
    | Dp of dp*sd*extr
    | Store
    | Leave of com
    | Back of com
    | Hat
    | RfStar of ie
    | Rmw

 type edge = { edge: tedge;  a1:atom option; a2: atom option; }


  open Printf

  let plain_edge e = { a1=None; a2=None; edge=e; }

  let pp_a = function
    | None -> Code.plain
    | Some a -> F.pp_atom a

  let pp_aa a1 a2 = match a1, a2 with
  | None,None -> ""
  | _,_ -> sprintf "%s%s" (pp_a a1) (pp_a a2)

  let pp_a_bis = function
    | None -> "P"
    | Some a -> F.pp_atom a

  let pp_aa_bis a1 a2 = match a1,a2 with
  | None,None -> ""
  | _,_ -> sprintf "%s%s" (pp_a_bis a1) (pp_a_bis a2)

  let pp_a_ter = function
    | None -> Code.plain
    | Some a as ao ->
        if ao = F.pp_as_a then "A"
        else F.pp_atom a

  let pp_aa_ter a1 a2 = match a1,a2 with
  | None,None -> ""
  | _,_ -> sprintf "%s%s" (pp_a_ter a1) (pp_a_ter a2)

  let pp_a_qua = function
    | None -> "P"
    | Some a as ao ->
        if ao = F.pp_as_a then "A"
        else F.pp_atom a

  let pp_aa_qua a1 a2 = match a1,a2 with
  | None,None -> ""
  | _,_ -> sprintf "%s%s" (pp_a_qua a1) (pp_a_qua a2)

  let pp_tedge = function
    | Rf ie -> sprintf "Rf%s" (pp_ie ie)
    | RfStar ie -> sprintf "Rf*%s" (pp_ie ie)
    | Fr ie -> sprintf "Fr%s" (pp_ie ie)
    | Ws ie -> sprintf "Ws%s" (pp_ie ie)
    | Detour d -> sprintf "Detour%s" (pp_extr d)
    | DetourWs d -> sprintf "Detour%sW" (pp_extr d)
    | Po (sd,e1,e2) -> sprintf "Po%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Fenced (f,sd,e1,e2) -> 
        sprintf "%s%s%s%s" (F.pp_fence f) (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Dp (dp,sd,e) -> sprintf "Dp%s%s%s"
          (F.pp_dp dp) (pp_sd sd) (pp_extr e)
    | Hat -> "Hat"
    | Rmw -> "Rmw"
    | Store -> "Store"
    | Leave c -> sprintf "%sLeave" (pp_com c)
    | Back c -> sprintf "%sBack" (pp_com c)

  let do_pp_edge pp_aa e = pp_tedge e.edge ^ pp_aa e.a1 e.a2

  let pp_edge e = do_pp_edge pp_aa e 
      
  let pp_edge_with_p e = do_pp_edge pp_aa_bis e

  let pp_edge_with_a e = do_pp_edge pp_aa_ter e

  let pp_edge_with_pa e = do_pp_edge pp_aa_qua e

  let compare_atomo a1 a2 = match a1,a2 with
  | None,None -> 0
  | None,Some _ -> -1
  | Some _,None -> 1
  | Some a1,Some a2 -> F.compare_atom a1 a2

  let compare e1 e2 = match compare_atomo e1.a1 e2.a1 with
  | 0 ->
      begin match  compare_atomo e1.a2 e2.a2 with
      | 0 -> Pervasives.compare e1.edge e2.edge
      | r -> r
      end
  | r -> r

  let pp_strong sd e1 e2 =
    sprintf "Fence%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2)

(* Backward compatibility... *)

let pp_dp_default tag sd e = sprintf "%s%s%s" tag (pp_sd sd) (pp_extr e)

  exception IsStore of string

  let do_dir_tgt_com = function
    | CRf -> Dir R
    | CWs|CFr -> Dir W

 and do_dir_src_com = function
   | CRf|CWs -> Dir W
   | CFr -> Dir R

  let do_dir_tgt e = match e with
  | Po(_,_,e)| Fenced(_,_,_,e)|Dp (_,_,e) -> e
  | Rf _| RfStar _ | Hat | Detour _ -> Dir R
  | Ws _|Fr _|Rmw|DetourWs _ -> Dir W
  | Store -> Dir W
  | Leave c|Back c -> do_dir_tgt_com c

  and do_dir_src e = match e with
  | Po(_,e,_)| Fenced(_,_,e,_) | Detour e | DetourWs e -> e
  | Dp _|Fr _|Hat|Rmw -> Dir R
  | Ws _|Rf _|RfStar _ -> Dir W
  | Store -> Dir W
  | Leave c|Back c -> do_dir_src_com c



let fold_tedges f r =
  let r = fold_ie (fun ie -> f (Rf ie)) r in
  let r = fold_ie (fun ie -> f (RfStar ie)) r in
  let r = fold_ie (fun ie -> f (Fr ie)) r in
  let r = fold_ie (fun ie -> f (Ws ie)) r in
  let r = f Rmw r in
  let r = fold_extr (fun e -> f (Detour e)) r in
  let r = fold_extr (fun e -> f (DetourWs e)) r in
  let r = fold_sd_extr_extr (fun sd e1 e2 r -> f (Po (sd,e1,e2)) r) r in
  let r =
    F.fold_all_fences
      (fun fe ->
        fold_sd_extr_extr
          (fun sd e1 e2 -> f (Fenced (fe,sd,e1,e2)))) r in
  let r =
    F.fold_dpr
      (fun dp -> fold_sd (fun sd -> f (Dp (dp,sd,Dir R)))) r in
  let r =
    F.fold_dpw
      (fun dp -> fold_sd (fun sd -> f (Dp (dp,sd,Dir W)))) r in
  let r = f Hat r in
  let r = f Store r in
  let r = fold_com (fun c r -> f (Leave c) r) r in
  let r = fold_com (fun c r -> f (Back c) r) r in
  r

  let fold_atomo f k = f None (F.fold_atom (fun a k -> f  (Some a) k) k)


  let _applies_atom a d = match a with
  | None -> true
  | Some a -> F.applies_atom a d

  let fold_edges f =
   fold_atomo
      (fun a1 ->
        fold_atomo
          (fun a2 ->
            (fold_tedges
               (fun te k ->
                 match te with
                 | Store ->
                     if a1 = None && a2=None then
                       f {a1; a2; edge=te;} k
                     else k
                 | Rmw -> (* identical sources and target atomicity for RMW *)
                     if a1 = a2 then
                       f {a1; a2; edge=te;} k
                     else k
                 | _ ->
                     f {a1; a2; edge=te;} k))))

(* checked later... because rmw accepts all atomicity
                     let d1 = do_dir_src te
                     and d2 = do_dir_tgt te in
                     if applies_atom a1 d1 && applies_atom a2 d2 then
                       f {a1; a2; edge=te;} k
                     else k *)

  let iter_edges f = fold_edges (fun e () -> f e) ()

  let t = Hashtbl.create 101

  let add_lxm lxm e =  Hashtbl.add t lxm e

(* Fill up lexeme table *)
  let () =
    let es = fold_edges (fun e k -> e::k) [] in
    List.iter  (fun e -> add_lxm (pp_edge e) e) es ;
    List.iter
      (fun e -> match e.a1,e.a2 with
      | (None,Some _)
      | (Some _,None) ->
          add_lxm (pp_edge_with_p e) e
      | _,_ -> ()) es ;
    List.iter
      (fun e -> match e.a1,e.a2 with
      | (_,(Some _ as a)) when a = F.pp_as_a ->
          add_lxm (pp_edge_with_a e) e
      | ((Some _ as a),_) when a = F.pp_as_a ->
          add_lxm (pp_edge_with_a e) e
      | _,_ -> ()) es ;
    List.iter
      (fun e -> match e.a1,e.a2 with
      | (None,(Some _ as a))
      | ((Some _ as a),None) when a = F.pp_as_a ->
          add_lxm (pp_edge_with_pa e) e
      | _,_ -> ()) es ;
    fold_sd_extr_extr
      (fun sd e1 e2 () ->
        add_lxm
          (pp_strong sd e1 e2) (plain_edge (Fenced (F.strong,sd,e1,e2)))) () ;
    let fill_opt tag dpo sd e = match dpo with
    | None -> ()
    | Some dp ->
        add_lxm
          (pp_dp_default tag sd e) 
          (plain_edge (Dp (dp,sd,e))) in
    fold_sd
      (fun sd () ->
        fill_opt "Dp" F.ddr_default sd Irr ;
        fill_opt "Dp" F.ddr_default sd (Dir R) ;
        fill_opt "Ctrl" F.ctrlr_default sd (Dir R) ;
        fill_opt "Dp" F.ddw_default sd (Dir W) ;
        fill_opt "Ctrl" F.ctrlw_default sd (Dir W) ;
        ()) () ;
    add_lxm "DetourRR" (plain_edge (Detour (Dir R))) ;
    add_lxm "DetourWR" (plain_edge (Detour (Dir W))) ;
    ()


  let fold_pp_edges f =
    Hashtbl.fold
      (fun s e k ->
        if e.a1=None && e.a2=None then
          f s k
        else k)
      t

  let fences_pp =
    F.fold_all_fences
      (fun f k -> (F.pp_fence f,f)::k)
      []

  let parse_fence s =
    try List.assoc s fences_pp
    with Not_found -> Warn.fatal "%s is not a fence" s

  let parse_edge s = 
    try Hashtbl.find t s
    with Not_found -> Warn.fatal "Bad edge: %s" s

let parse_edges s = List.map parse_edge (LexUtil.just_split s)

let pp_edges es = String.concat " " (List.map pp_edge es)

let dir_tgt e = do_dir_tgt e.edge
and dir_src e = do_dir_src e.edge


let do_set_tgt d e = match e  with
  | Po(sd,src,_) -> Po (sd,src,Dir d)
  | Fenced(f,sd,src,_) -> Fenced(f,sd,src,Dir d)
  | Dp (dp,sd,_) -> Dp (dp,sd,Dir d) 
  | Rf _ |RfStar _ | Hat
  | Ws _|Fr _|Rmw|Detour _|DetourWs _|Store|Leave _|Back _-> e

and do_set_src d e = match e with
  | Po(sd,_,tgt) -> Po(sd,Dir d,tgt)
  | Fenced(f,sd,_,tgt) -> Fenced(f,sd,Dir d,tgt)
  | Detour _ -> Detour (Dir d)
  | DetourWs _ -> DetourWs (Dir d)
  | Fr _|Hat|Dp _
  | Ws _|Rf _|RfStar _|Rmw|Store|Leave _|Back _ -> e

let set_tgt d e = { e with edge = do_set_tgt d e.edge ; }
and set_src d e = { e with edge = do_set_src d e.edge ; }

let loc_sd e = match e.edge with
  | Fr _|Ws _|Rf _|RfStar _|Hat|Rmw|Detour _|DetourWs _ -> Same
  | Po (sd,_,_) | Fenced (_,sd,_,_) | Dp (_,sd,_) -> sd
  | Store -> Same
  | Leave _|Back _ -> Same

  let get_ie e = match e.edge with
  | Po _|Dp _|Fenced _|Rmw|Detour _|DetourWs _ -> Int
  | Rf ie|RfStar ie|Fr ie|Ws ie -> ie
  | Store -> Int
  | Leave _|Back _|Hat -> Ext

  type full_ie = IE of ie | LeaveBack

  let get_full_ie e = match e.edge with
  | Leave _|Back _ -> LeaveBack
  | _ -> IE (get_ie e)


  let can_precede_dirs  x y = match x.edge,y.edge with
  | (Store,Store) -> false
  | (Store,_) -> get_ie y = Int
  | (_,Store) -> true
  | _,_ ->
      begin match dir_tgt x,dir_src y with
      | Irr,Irr -> false
      | (Irr,Dir _) | (Dir _,Irr) -> true
      | Dir d1,Dir d2 -> d1=d2
      end

  let is_ext e = match e.edge with
  | Rf Ext|Fr Ext|Ws Ext
  | Leave _|Back _ -> true
  | _ -> false

  let can_precede_atoms x y = match x.a2,y.a1 with
  | None,_
  | _,None -> true
  | Some a1,Some a2 -> F.compare_atom a1 a2 = 0

  let can_precede x y = can_precede_dirs  x y && can_precede_atoms x y


(*************************************************************)
(* Expansion of irrelevant direction specifications in edges *)
(*************************************************************)

  let expand_dir d f = match d with
  | Dir _ -> f d
  | Irr -> fun k -> f (Dir W) (f (Dir R) k)
        
  let expand_dir2 e1 e2 f = 
    expand_dir e1
      (fun d1 -> expand_dir e2 (fun d2 -> f d1 d2))

  let do_expand_edge e f =
    match e.edge with
    | Rf _ | RfStar _ | Fr _ | Ws _ | Hat |Rmw|Dp _|Store|Leave _|Back _
      -> f e
    | Detour d ->
        expand_dir d (fun d -> f { e with edge=Detour d;})
    | DetourWs d ->
        expand_dir d (fun d -> f { e with edge=DetourWs d;})    
    | Po(sd,e1,e2) ->
        expand_dir2 e1 e2 (fun d1 d2 -> f {e with edge=Po(sd,d1,d2);})
    | Fenced(fe,sd,e1,e2) ->
        expand_dir2 e1 e2 (fun d1 d2 -> f {e with edge=Fenced(fe,sd,d1,d2);})


  let rec do_expand_edges es f suf = match es with
  | [] -> f suf
  | e::es ->
      do_expand_edge e
        (fun e k ->
          try
            let suf = match suf with
            | [] -> [e]
            | f::_ ->
                if can_precede e f then e::suf
                else raise Exit in
            do_expand_edges es f suf k
          with Exit -> k)

  let expand_edges es f = do_expand_edges (List.rev es) f []

(* resolve *)
  let resolve_pair e1 e2 =
    let e1,e2 =
      let d1 = dir_tgt e1 and d2 = dir_src e2 in
      match d1,d2 with
      | Irr,Dir d -> set_tgt d e1,e2
      | Dir d,Irr -> e1,set_src d e2
      | _,_ -> e1,e2 in
    let a1 = e1.a2 and a2 = e2.a1 in
    match a1,a2 with
    | None,Some _ -> { e1 with a2 = a2;},e2
    | Some _,None -> e1, { e2 with a1 = a1}
    | _,_ -> e1,e2

  let merge_dir d1 d2 = match d1,d2 with
  | (Irr,Dir d)|(Dir d,Irr) -> d
  | Dir d1,Dir d2 -> assert (d1=d2) ; d1
  | Irr,Irr -> assert false

  let merge_atom a1 a2 = match a1,a2 with
  | None,Some _ -> a2
  | Some _,None -> a1
  | None,None -> None
  | Some a1,Some a2 -> assert (F.compare_atom a1 a2 = 0) ; Some a1

  let merge_pair e1 e2 =
    let tgt = merge_dir (dir_tgt e1) (dir_tgt e2)
    and src = merge_dir (dir_src e1) (dir_src e2) in
    let e = set_tgt tgt (set_src src e1) in
    { e with a1 = merge_atom e1.a1 e2.a1; a2 = merge_atom e1.a2 e2.a2; }

 
  let resolve_edges es = match es with
  | []|[_] -> es
  | fst::es ->
     let rec do_rec p = function
     | [] -> 
         let p,fst = resolve_pair p fst in
         fst,p,[]
     | e::es ->
         let p,e = resolve_pair p e in
         let fst,q,es = do_rec e es in
         fst,p,q::es in
     let fst1,fst2,es = do_rec fst es in
     let fst = merge_pair fst1 fst2 in
     fst::es
     
(* compact *)
  let seq_sd e1 e2 = match loc_sd e1,loc_sd e2 with
  | Same,Same -> Same
  | _,_ -> Diff

  let fst_dp e1 e2 k = match e1.edge with
  | Dp (d,_,_) ->
      let ds = F.fst_dp d in
      List.fold_right
        (fun d k -> [plain_edge (Dp (d, seq_sd e1 e2,dir_tgt e2))]::k)
        ds k
  | _ -> k

  let sequence_dp e1 e2 k = match e1.edge,e2.edge with
  | Dp (d1,_,_),Dp (d2,_,_) ->
      let ds = F.sequence_dp d1 d2 in
      List.fold_right
        (fun d k -> [plain_edge (Dp (d, seq_sd e1 e2,dir_tgt e2))]::k)
        ds k
  | _,_ -> k

  let rec set_last xs y = match xs with
  | [] -> assert false
  | [_] -> [y]
  | x::xs -> x::set_last xs y

  let set_fst y xs = match xs with
  | _::xs -> y::xs
  | [] -> assert false

  let fst_fence xs ys e1 e2 k = match e1.edge with
  | Fenced (f,_,_,_) ->
      let ne = plain_edge (Fenced (f, seq_sd e1 e2,dir_src e1,dir_tgt e2)) in
      [ne]::set_last xs ne::set_fst ne ys::k      
  | _ -> k


  let snd_fence xs ys e1 e2 k = match e2.edge with
  | Fenced (f,_,_,_) ->
      let ne = plain_edge (Fenced (f, seq_sd e1 e2,dir_src e1,dir_tgt e2)) in
      [ne]::set_last xs ne::set_fst ne ys::k
  | _ -> k

  let po e1 e2 k =
    [plain_edge (Po (seq_sd e1 e2,dir_src e1,dir_tgt e2))]::k

  let com e1 e2 k = match e1.edge,e2.edge with
  | Ws _,Ws _ 
  | Fr _,Ws _ -> [e1]::k
  | Rf _,Fr _ -> [plain_edge (Ws Int)]::k
  | _,_ -> k
        
  let compact_sequence xs ys e1 e2 =
    let k = com e1 e2 [] in
    let k = po e1 e2 k in
    let k = snd_fence xs ys e1 e2 k in
    let k = fst_fence xs ys e1 e2 k in
    let k = fst_dp e1 e2 k in
    let k = sequence_dp e1 e2 k in
    k

  let is_detour e = match e.edge with
  | Detour _|DetourWs _ -> true
  | _ -> false

  let is_store e = match e.edge with
  | Store -> true
  | _ -> false

  module Set =
    MySet.Make
      (struct
        type t = edge
        let compare = compare
      end)


end
