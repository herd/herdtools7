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

(* Edges, ie specifications of an event pair in a model relation  *)

module Config =
  struct
    let variant _ = false
    let naturalsize = TypBase.get_size TypBase.default
    let wildcard = false
  end

let dbg = 0

module type S = sig
  open Code

  type fence
  type dp

  include Atom.AtomType

  val wildcard : bool
  type value = Value.v

  val pp_atom : atom -> string
  val tr_value : atom option -> value -> value
  val overwrite_value : value -> atom option -> value -> value
  val extract_value : value -> atom option -> value
  val set_pteval :
    atom option -> Value.pte -> (unit -> string) -> Value.pte
  val merge_atoms : atom -> atom -> atom option
  val is_ifetch : atom option -> bool
  val atom_to_bank : atom option -> SIMD.atom Code.bank
  val strong : fence
  val pp_fence : fence -> string

(* edge proper *)
  type tedge =
    | Rf of ie | Fr of ie | Ws of ie
    | Po of sd*extr*extr | Fenced of fence*sd*extr*extr
    | Dp of dp*sd*extr
    | Leave of com (* Leave thread *)
    | Back  of com (* Return to thread *)
(* Fake edges *)
    | Id              (* Annotation on access *)
    | Insert of fence (* Insert some code     *)
    | Store           (* Add store at thread code start *)
    | Node of dir     (* Isolated event       *)
(* fancy *)
    | Hat
    | Rmw of RMW.rmw  (* Various sorts of read-modify-write *)

  val is_id : tedge -> bool
  val is_node : tedge -> bool
  val is_insert_store : tedge -> bool
  val is_non_pseudo : tedge -> bool
  val is_dp_addr : tedge -> bool
  val compute_rmw : RMW.rmw -> value -> value -> value
  val valid_rmw : RMW.rmw list -> bool

  type edge = { edge: tedge;  a1:atom option; a2: atom option; }

  val plain_edge : tedge -> edge

  val fold_atomo : (atom option -> 'a -> 'a) -> 'a -> 'a
  val fold_mixed : (atom option -> 'a -> 'a) -> 'a -> 'a
  val fold_atomo_list : atom option list -> (atom option -> 'a -> 'a) -> 'a -> 'a

  val fold_edges : (edge -> 'a -> 'a) -> 'a -> 'a
  val iter_edges : (edge -> unit) -> unit

  val pp_tedge : tedge -> string
  val pp_atom_option : atom option -> string

  val debug_edge : edge -> string
  val pp_edge : edge -> string
  val compare_atomo : atom option -> atom option -> int
  val compare : edge -> edge -> int

  val parse_atom : string -> atom option
  val parse_atoms : string list -> atom option list
  val get_access_atom: atom option -> MachMixed.t option

  val parse_fence : string -> fence
  val parse_edge : string -> edge
  val parse_edges : string -> edge list

  val pp_edges : edge list -> string

(* Get source and target event direction,
   Returning Irr means that a Read OR a Write is acceptable,
   Returning No means that the direction is not applicable (pseudo edge *)
  val dir_src : edge -> extr
  val dir_tgt : edge -> extr
  val safe_dir : edge -> dir option
(* Return edge with direction resolved *)
  val set_src : dir ->  edge -> edge
  val set_tgt : dir ->  edge -> edge

(* Does source and target events have the same or different locations? *)
  val loc_sd : edge -> sd
  val is_diff: edge -> bool

(* Internal (same proc) or external edge (different procs) *)
  val get_ie : edge -> ie
(* More detailed *)
  type full_ie = IE of ie | LeaveBack
  val get_full_ie : edge -> full_ie

(* If source atom implies wide access, size of access as integers *)
  val as_integers : edge -> int option

(* Is source atom a pair access? *)
  val is_pair : edge -> bool

(* Can e1 target event direction be the same as e2 source event? *)
  val can_precede : edge -> edge -> bool

(* Expansion of Irr directions *)
  val expand_edges : edge list -> (edge list -> 'a -> 'a) -> 'a -> 'a

(* Resolve Irr directions and unspecified atom *)
  val resolve_edges : edge list -> edge list

(* Atomic variation over yet unspecified atoms *)
  val varatom : edge list -> (edge list -> 'a -> 'a) -> 'a -> 'a

(* Possible interpretation of edge sequence as an edge *)
  val compact_sequence : edge list -> edge list -> edge -> edge -> edge list list

(* return if an edge switch on any machine feature *)
  val get_machine_feature : edge -> StringSet.t

(* Utilities *)
  val is_ext : edge -> bool
  val is_com : edge -> bool
  val is_fetch : edge -> bool

(* Set/Map *)
  module Set : MySet.S with type elt = edge
  module Map : MyMap.S with type key = edge

(* Show some elements, for documentation *)
  val show : ShowGen.t -> unit
end


module
  Make
    (Cfg:
       sig
         val variant : Variant_gen.t -> bool
         val naturalsize : MachSize.sz
         val wildcard : bool
       end)
    (F:Fence.S)
    (A:Atom.S): S
with
type fence = F.fence
and type dp = F.dp
and type atom = A.atom
and module SIMD = A.SIMD
and module Value = A.Value
and type value = A.Value.v
and module RMW = A.RMW = struct
  let ()  = ignore (Cfg.naturalsize)
  let do_self = Cfg.variant Variant_gen.Self
  let do_mixed = Variant_gen.is_mixed Cfg.variant
  let do_kvm =  Variant_gen.is_kvm Cfg.variant
  let do_disjoint = Cfg.variant Variant_gen.MixedDisjoint
  let do_strict_overlap = Cfg.variant Variant_gen.MixedStrictOverlap
  let wildcard = Cfg.wildcard

  let debug = false
  open Code

  type fence = F.fence
  type dp = F.dp

  type atom = A.atom
  module Value = A.Value
  module SIMD = A.SIMD
  module RMW = A.RMW

  type value = A.Value.v

  let compute_rmw rmw old operand =
    Value.from_int @@ RMW.compute_rmw rmw ~old:(Value.to_int old) ~operand:(Value.to_int operand)

  let valid_rmw = RMW.valid_rmw

  let pp_atom = A.pp_atom
  let tr_value = A.tr_value
  let overwrite_value = A.overwrite_value
  let extract_value = A.extract_value
  let set_pteval ao p = match ao with
  | None -> fun _ -> p
  | Some a -> A.Value.set_pteval a p

  let applies_atom ao d = match ao,d with
  | (None,_)|(_,(Irr|NoDir)) -> true
  | Some a,Dir d -> A.applies_atom a d

  let merge_atoms = A.merge_atoms
  let is_ifetch = A.is_ifetch

  let atom_to_bank = function
    | None -> Ord
    | Some a -> A.atom_to_bank a

  let strong = F.strong
  let pp_fence = F.pp_fence

(* edge proper *)
  type tedge =
    | Rf of ie | Fr of ie | Ws of ie
    | Po of sd*extr*extr | Fenced of fence*sd*extr*extr
    | Dp of dp*sd*extr
    | Leave of com
    | Back of com
    | Id
    | Insert of fence
    | Store
    | Node of dir
    | Hat
    | Rmw of RMW.rmw


  let is_id = function
    | Id -> true
    | Store|Insert _|Hat|Rmw _|Rf _|Fr _|Ws _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Node _ -> false

  let is_insert_store = function
    | Store|Insert _ -> true
    | Id|Hat|Rmw _|Rf _|Fr _|Ws _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Node _ -> false

  let is_node = function
    | Node _ -> true
    | Id|Hat|Rmw _|Rf _|Fr _|Ws _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Insert _
    | Store -> false

  let is_non_pseudo = function
    | Store|Insert _ |Id|Node _-> false
    | Hat|Rmw _|Rf _|Fr _|Ws _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _ -> true

  let is_dp_addr = function
    |Dp (dp, _, _) -> F.is_addr dp
    |_ -> false

  type edge = { edge: tedge;  a1:atom option; a2: atom option; }

  open Printf

  let plain_edge e = { a1=None; a2=None; edge=e; }

  let pp_as_a = A.pp_as_a
  let pp_plain = A.pp_plain

  let pp_atom_option = function
    | None -> pp_plain
    | Some a -> pp_atom a

  let pp_one_or_two pp_a e a1 a2 = match e with
  | Id -> pp_a a1
  | _ -> sprintf "%s%s" (pp_a a1) (pp_a a2)

  let pp_aa e a1 a2 = match a1, a2 with
  | None,None  when not (is_id e) -> ""
  | _,_ -> pp_one_or_two pp_atom_option e a1 a2

  let pp_a_ter = function
    | None -> pp_plain
    | Some a as ao ->
        if ao = pp_as_a then "A"
        else pp_atom a

  let pp_aa_ter e a1 a2 = match a1,a2 with
  | None,None  when not (is_id e) -> ""
  | _,_ -> pp_one_or_two pp_a_ter e a1 a2


  let do_pp_tedge compat = function
    | Rf UnspecCom -> sprintf "Rf"
    | Fr UnspecCom -> sprintf "Fr"
    | Ws UnspecCom -> if compat then sprintf "Ws" else sprintf "Co"
    | Rf ie -> sprintf "Rf%s" (pp_ie ie)
    | Fr ie -> sprintf "Fr%s" (pp_ie ie)
    | Ws ie -> if compat then sprintf "Ws%s" (pp_ie ie) else sprintf "Co%s" (pp_ie ie)
    | Po (UnspecLoc,Irr,Irr) -> "Po"
    | Po (sd,e1,e2) ->
      sprintf "Po%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Fenced (f,sd,e1,e2) ->
      sprintf "%s%s%s%s" (pp_fence f) (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Dp (dp,UnspecLoc,Irr) -> sprintf "Dp%s" (F.pp_dp dp)
    | Dp (dp,sd,e) ->
      sprintf "Dp%s%s%s"(F.pp_dp dp) (pp_sd sd) (pp_extr e)
    | Hat -> "Hat"
    | Rmw rmw-> RMW.pp_rmw compat rmw
    | Leave c -> sprintf "%sLeave" (pp_com c)
    | Back c -> sprintf "%sBack" (pp_com c)
    | Id -> "Id"
    | Insert f -> pp_fence f
    | Store -> "Store"
    | Node W -> "Write"
    | Node R -> "Read"

  let pp_tedge = do_pp_tedge false

  let debug_edge e =
    sprintf
      "{edge=%s, a1=%s, a2=%s}"
      (do_pp_tedge false e.edge) (pp_atom_option e.a1) (pp_atom_option e.a2)

  let do_pp_edge compat pp_atom_functor e =
    let annotation = pp_atom_functor e.edge e.a1 e.a2 in
    let edge = match e.edge with
    | Id -> ""
    | _ -> do_pp_tedge compat e.edge in
    edge ^ annotation

  let pp_edge_with_xx compat e = do_pp_edge compat pp_aa e

  let pp_edge_with_a compat e = do_pp_edge compat pp_aa_ter e

  let pp_edge e = pp_edge_with_xx false e

  let compare_atomo = Option.compare A.compare_atom

  let compare e1 e2 = match compare_atomo e1.a1 e2.a1 with
  | 0 ->
      begin match  compare_atomo e1.a2 e2.a2 with
      | 0 -> compare e1.edge e2.edge
      | r -> r
      end
  | r -> r

  let pp_strong sd e1 e2 =
    sprintf "Fence%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2)

(* Backward compatibility... *)

let pp_dp_default tag sd e = sprintf "%s%s%s" tag (pp_sd sd) (pp_extr e)

  let do_dir_tgt_com = function
    | CRf -> Dir R
    | CWs|CFr -> Dir W

 and do_dir_src_com = function
   | CRf|CWs -> Dir W
   | CFr -> Dir R

  exception NotThat of string

  let not_that e msg = raise (NotThat (sprintf "%s: %s" msg (pp_tedge e)))

  let do_dir_tgt e = match e with
  | Po(_,_,e)| Fenced(_,_,_,e)|Dp (_,_,e) -> e
  | Rf _| Hat -> Dir R
  | Ws _|Fr _|Rmw _  -> Dir W
  | Leave c|Back c -> do_dir_tgt_com c
  | Id -> not_that e "do_dir_tgt"
  | Insert _ -> NoDir
  | Store -> Dir W
  | Node d -> Dir d


  and do_dir_src e = match e with
  | Po(_,e,_)| Fenced(_,_,e,_) -> e
  | Dp _|Fr _|Hat|Rmw _ -> Dir R
  | Ws _|Rf _ -> Dir W
  | Leave c|Back c -> do_dir_src_com c
  | Id -> not_that e "do_dir_src"
  | Insert _ -> NoDir
  | Store -> Dir W
  | Node d -> Dir d

  let do_loc_sd e = match e with
  | Po (sd,_,_) | Fenced (_,sd,_,_) | Dp (_,sd,_) -> sd
  | Insert _|Store|Node _|Fr _|Ws _|Rf _|Hat|Rmw _|Id|Leave _|Back _ -> Same

  let do_is_diff e = Code.is_diff_loc @@ do_loc_sd e

let fold_tedges_compat f r =
  let r = fold_ie wildcard (fun ie -> f (Ws ie)) r in
  let r = RMW.fold_rmw_compat (fun rmw -> f (Rmw rmw)) r
  in r

let fold_tedges f r =
  let r = fold_ie wildcard (fun ie -> f (Rf ie)) r in
  let r = fold_ie wildcard (fun ie -> f (Fr ie)) r in
  let r = fold_ie wildcard (fun ie -> f (Ws ie)) r in
  let r = RMW.fold_rmw wildcard (fun rmw -> f (Rmw rmw)) r in
  let r = fold_sd_extr_extr wildcard (fun sd e1 e2 r -> f (Po (sd,e1,e2)) r) r in
  let r = F.fold_all_fences (fun fe -> f (Insert fe)) r in
  let r = f Store r in
  let r =
    F.fold_all_fences
      (fun fe ->
        fold_sd_extr_extr wildcard
          (fun sd e1 e2 -> f (Fenced (fe,sd,e1,e2)))) r in
  let r =
    F.fold_dpr
      (fun dp -> fold_sd wildcard (fun sd -> f (Dp (dp,sd,Dir R)))) r in
  let r =
    F.fold_dpw
      (fun dp -> fold_sd wildcard (fun sd -> f (Dp (dp,sd,Dir W)))) r in
  let r =
    if wildcard then F.fold_dpw
      (fun dp -> fold_sd wildcard (fun sd -> f (Dp (dp,sd,Irr)))) r
    else r in
  let r = f Id r in
  let r = f (Node R) (f (Node W) r) in
  let r = f Hat r in
  let r = fold_com (fun c r -> f (Leave c) r) r in
  let r = fold_com (fun c r -> f (Back c) r) r in
  r

  let fold_atomo f k = f None (A.fold_atom (fun a k -> f  (Some a) k) k)
  let fold_mixed f k = A.fold_mixed (fun a k -> f  (Some a) k) k
  let fold_atomo_list aos f k = List.fold_right (fun a k -> f a k) aos k

  let overlap_atoms a1 a2 =
    match a1,a2 with
    | (None,_)|(_,None) -> true
    | Some a1,Some a2 -> A.overlap_atoms a1 a2

  let get_access_atom = A.get_access_atom

  let same_access_atoms a1 a2 =
    Misc.opt_eq MachMixed.equal (get_access_atom a1) (get_access_atom a2)

  (* For rmw instruction any accesses is a priori.
     However identical accesses are forced for rmw instructions *)
  let ok_rmw rmw a1 a2 =
    not (RMW.is_one_instruction rmw) || same_access_atoms a1 a2

  let ok_non_rmw e a1 a2 =
    (* `do_is_diff` is safe to call when `e` is not
       wildcard `*`/UnspecLoc location. *)
    Code.is_unspec_loc @@ do_loc_sd e ||
    do_is_diff e || do_disjoint ||
    (overlap_atoms a1 a2 &&
     not (do_strict_overlap && same_access_atoms a1 a2))

  let ok_mixed e a1 a2 =
    match e with
    | Rmw rmw ->
    (* Specific case *)
        ok_rmw rmw a1 a2
    | _ ->
    (* Situation is controlled by variant for other relaxations *)
        ok_non_rmw e a1 a2


  let do_fold_edges fold_tedges f =
   fold_atomo
      (fun a1 ->
        fold_atomo
          (fun a2 ->
            (fold_tedges
               (fun te k ->
                 match te with
                 | Rmw rmw -> (* Allowed source and target atomicity for rmw *)
                     if RMW.applies_atom_rmw rmw a1 a2 then begin
                       let e =  {a1; a2; edge=te;} in
                       f e k
                     end else k
                 | Id -> begin
                     match a1,a2 with
                     | Some x1,Some x2 when
                         A.compare_atom x1 x2=0
                         && not (is_ifetch a1) ->
                         f { a1; a2;edge=te; } k
                     | None,None ->
                         let e =  { a1; a2;edge=te; } in
                         f e k
                     | _,_ -> k
                 end
                 | Insert _|Node _|Store  ->
                     begin match a1,a2 with
                     | None,None ->
                         let e =  { a1; a2;edge=te; } in
                         f e k
                     | _,_ -> k
                     end
                 | _ ->
                     let d1 = do_dir_src te
                     and d2 = do_dir_tgt te in
                     if
                       applies_atom a1 d1 &&
                       applies_atom a2 d2 &&
                       (Misc.is_none (get_access_atom a1) &&
                        Misc.is_none (get_access_atom a2)||
                       ok_non_rmw te a1 a2)
                     then
                       f {a1; a2; edge=te;} k
                     else begin
                       if debug then
                         eprintf "Not %s\n" (debug_edge  {a1; a2; edge=te;}) ;
                       k
                     end ))))

  let fold_edges f = do_fold_edges fold_tedges f

(* checked later... because rmw accepts all atomicity
                     let d1 = do_dir_src te
                     and d2 = do_dir_tgt te in
                     if applies_atom a1 d1 && applies_atom a2 d2 then
                       f {a1; a2; edge=te;} k
                     else k *)

  let dir_tgt e = do_dir_tgt e.edge
  and dir_src e = do_dir_src e.edge
  and safe_dir e =
    try
      begin match do_dir_src e.edge with
      | Dir d -> Some d
      | NoDir|Irr -> None
      end
    with NotThat _ -> None

(***************)
(* Atom lexing *)
(***************)

  let iter_atom = Misc.fold_to_iter fold_atomo

  let ta = Hashtbl.create  37

  let add_lxm_atom lxm a =
    if dbg > 1 then eprintf "ATOM: %s\n" lxm ;
    try
      let old = Hashtbl.find ta lxm in
      assert (compare_atomo old a = 0) ;
    with Not_found ->
      if not (is_ifetch a) then Hashtbl.add ta lxm a

  let () = iter_atom (fun a -> add_lxm_atom (pp_atom_option a) a)

  let parse_atom s =
    try Hashtbl.find ta s
    with Not_found -> Warn.fatal "Bad atom: %s" s

  let parse_atoms xs =
    try
      List.fold_left
        (fun k x ->
          List.fold_left
            (fun k s -> parse_atom s::k)
            k (LexUtil.just_split x))
        [] xs
    with LexUtil.Error msg ->
      Warn.fatal "bad atoms list (%s)" msg

  let get_access_atom = A.get_access_atom


(**********)
(* Lexing *)
(**********)

  let iter_edges = Misc.fold_to_iter fold_edges


  let t = Hashtbl.create 40000

  let add_lxm_edge lxm e =
    if dbg > 1 then eprintf "LXM: %s\n" lxm ;
    try
      let old = Hashtbl.find t lxm in
      if compare old e <> 0 then begin
        Warn.warn_always "ambiguous lexeme: %s" lxm ;
        eprintf "%s\n%s\n" (debug_edge old) (debug_edge e) ;
        assert false
      end

    with Not_found ->
      Hashtbl.add t lxm e

(* Fill lexeme table *)
  let iter_ie = Misc.fold_to_iter (fold_ie wildcard)

  let four_times_iter_edges compat iter_edges =
    iter_edges  (fun e -> add_lxm_edge (pp_edge_with_xx compat e) e) ;
    iter_edges
      (fun e -> match e.a1,e.a2 with
      | (None,(Some _ as a))
      | ((Some _ as a),None) when a = pp_as_a ->
          add_lxm_edge (pp_edge_with_a compat e) e
      | _,_ -> ())

  let () =
   four_times_iter_edges false iter_edges;
   fold_sd_extr_extr wildcard
      (fun sd e1 e2 () ->
        add_lxm_edge
          (pp_strong sd e1 e2) (plain_edge (Fenced (F.strong,sd,e1,e2)))) () ;
    let fill_opt tag dpo sd e = match dpo with
    | None -> ()
    | Some dp ->
        add_lxm_edge
          (pp_dp_default tag sd e)
          (plain_edge (Dp (dp,sd,e))) in
    fold_sd wildcard
      (fun sd () ->
        if wildcard then fill_opt "Dp" F.ddr_default sd Irr ;
        if wildcard then fill_opt "Ctrl" F.ctrlr_default sd Irr ;
        fill_opt "Dp" F.ddr_default sd (Dir R) ;
        fill_opt "Ctrl" F.ctrlr_default sd (Dir R) ;
        fill_opt "Dp" F.ddw_default sd (Dir W) ;
        fill_opt "Ctrl" F.ctrlw_default sd (Dir W) ;
        ()) () ;
    if not (Hashtbl.mem t "R") then add_lxm_edge "R" (plain_edge (Node R)) ;
    if not (Hashtbl.mem t "W") then add_lxm_edge "W" (plain_edge (Node W)) ;
(*Co aka Ws and LxSx aka Rmw*)
   four_times_iter_edges true (Misc.fold_to_iter (do_fold_edges fold_tedges_compat));
(* Backward compatibility *)
    let instr_atom = A.instr_atom in
    if do_self && instr_atom != None then
      iter_ie
        (fun ie ->
           add_lxm_edge (sprintf "Iff%s" (pp_ie ie)) { a1=None; a2=instr_atom; edge=(Rf ie); } ;
           add_lxm_edge (sprintf "Irf%s" (pp_ie ie)) { a1=None; a2=instr_atom; edge=(Rf ie); } ;
           add_lxm_edge (sprintf "Fif%s" (pp_ie ie)) { a1=instr_atom; a2=None; edge=(Fr ie); } ;
           add_lxm_edge (sprintf "Ifr%s" (pp_ie ie)) { a1=instr_atom; a2=None; edge=(Fr ie); });
    ()

  let fold_pp_edges f =
    Hashtbl.fold
      (fun s e k ->
        if e.a1=None && e.a2=None && e.edge <> Id then
          f s k
        else k)
      t

  let fences_pp =
    F.fold_all_fences
      (fun f k -> (pp_fence f,f)::k)
      []

  let parse_fence s =
    try List.assoc s fences_pp
    with Not_found -> Warn.fatal "%s is not a fence" s

  let parse_edge s =
    try Hashtbl.find t s
    with Not_found -> Warn.fatal "Bad edge: %s" s

  let parse_edges s = List.map parse_edge (LexUtil.just_split s)

  let pp_edges es = String.concat " " (List.map pp_edge es)

  let do_set_tgt d e = match e  with
  | Po(sd,src,_) -> Po (sd,src,Dir d)
  | Fenced(f,sd,src,_) -> Fenced(f,sd,src,Dir d)
  | Dp (dp,sd,_) -> Dp (dp,sd,Dir d)
  | Rf _ | Hat
  | Insert _|Store|Id|Node _|Ws _|Fr _|Rmw _|Leave _|Back _-> e

  and do_set_src d e = match e with
  | Po(sd,_,tgt) -> Po(sd,Dir d,tgt)
  | Fenced(f,sd,_,tgt) -> Fenced(f,sd,Dir d,tgt)
  | Fr _|Hat|Dp _
  | Insert _|Store|Id|Node _|Ws _|Rf _|Rmw _|Leave _|Back _ -> e

  let set_tgt d e = { e with edge = do_set_tgt d e.edge ; }
  and set_src d e = { e with edge = do_set_src d e.edge ; }

  let loc_sd e = do_loc_sd e.edge
  and is_diff e = do_is_diff e.edge

  let get_ie e = match e.edge with
  | Id |Po _|Dp _|Fenced _|Rmw _ -> Int
  | Rf ie|Fr ie|Ws ie -> ie
  | Leave _|Back _|Hat -> Ext
  | Insert _|Store|Node _ -> Int

  type full_ie = IE of ie | LeaveBack

  let get_full_ie e = match e.edge with
  | Leave _|Back _ -> LeaveBack
  | _ -> IE (get_ie e)

  let as_integers e = A.as_integers e.a1

  let is_pair e = A.is_pair e.a1

  let can_precede_dirs  x y = match x.edge,y.edge with
  | (Store,Store) -> false
  | (Id,_)|(_,Id)|(Store,_)|(_,Store) -> true
  | (Insert _,Insert _) -> do_kvm || do_self
  | _,_ ->
      begin match dir_tgt x,dir_src y with
      | (Irr,Irr) -> false
      | (Irr,Dir _) | (Dir _,Irr)|(NoDir,_)|(_,NoDir) -> true
      | Dir d1,Dir d2 -> d1=d2
      end

  let is_ext e = match e.edge with
  | Rf Ext|Fr Ext|Ws Ext
  | Leave _|Back _ -> true
  | _ -> false

  let is_com e = match e.edge with
  | Rf _|Fr _|Ws _|Leave _|Back _| Hat -> true
  | _ -> false

  let is_fetch e = match e.edge with
  | Rf _ -> is_ifetch e.a2
  | Fr _ -> is_ifetch e.a1
  | _ -> is_ifetch e.a1 || ( loc_sd e = Same && is_ifetch e.a2)

  let compat_atoms a1 a2 = match merge_atoms a1 a2 with
  | None -> false
  | Some _ -> true

  let can_precede_atoms x y = match x.a2,y.a1 with
  | None,_
  | _,None -> true
  | Some a1,Some a2 -> compat_atoms a1 a2

  let can_precede x y = can_precede_dirs  x y && can_precede_atoms x y

(*************************************************************)
(* Expansion of irrelevant direction specifications in edges *)
(*************************************************************)

  let expand_loc sd f acc = match sd with
  | Same|Diff -> f sd acc
  | UnspecLoc -> f Same (f Diff acc)

  let expand_com com f acc = match com with
  | Int|Ext -> f com acc
  | UnspecCom -> f Int (f Ext acc)

  let expand_dir d f acc = match d with
  | Dir _|NoDir -> f d acc
  | Irr -> f (Dir W) (f (Dir R) acc)

  let expand_dir2 e1 e2 f =
    expand_dir e1
      (fun d1 -> expand_dir e2 (fun d2 -> f d1 d2))

  let expand_dp_dir dp sd f acc = match sd with
  | Dir _|NoDir -> f sd acc
  | Irr ->
    let expand_dir_list = F.expand_dp_dir dp in
    List.fold_left (fun acc sd -> f (Dir sd) acc) acc expand_dir_list

  let do_expand_edge e f acc =
    match e.edge with
    | Insert _|Store|Id|Node _
    | Hat |Leave _|Back _
      -> f e acc
    | Rf com -> expand_com com ( fun new_com -> f {e with edge = Rf(new_com)}) acc
    | Fr com -> expand_com com ( fun new_com -> f {e with edge = Fr(new_com)}) acc
    | Ws com -> expand_com com ( fun new_com -> f {e with edge = Ws(new_com)}) acc
    | Rmw rmw ->
        let expand_rmw_list = A.RMW.expand_rmw rmw in
        List.fold_left ( fun acc new_rmw -> f {e with edge=Rmw(new_rmw);} acc) acc expand_rmw_list
    | Dp (dp,sd,expr) ->
      expand_dp_dir dp expr (fun new_expr ->
        expand_loc sd ( fun new_sd -> f {e with edge=Dp(dp,new_sd,new_expr);})) acc
    | Po(sd,e1,e2) ->
        expand_dir2 e1 e2 (fun d1 d2 ->
          expand_loc sd ( fun new_sd -> f {e with edge=Po(new_sd,d1,d2);})) acc
    | Fenced(fe,sd,e1,e2) ->
        expand_dir2 e1 e2 (fun d1 d2 ->
          expand_loc sd ( fun new_sd -> f {e with edge=Fenced(fe,new_sd,d1,d2);})) acc

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
  let rec find_non_insert_store = function
    | [] -> raise Not_found
    | e::es -> begin match e.edge with
      | Insert _|Store ->
          let bef,ni,aft = find_non_insert_store es in
          e::bef,ni,aft
      | _ ->
          [],e,es
    end

  let set_a1 e a = match e.edge with
  | Node _|Id -> { e with a1=a; a2=a;}
  | _ -> { e with a1=a;}

  let set_a2 e a = match e.edge with
  | Node _|Id  -> { e with a1=a; a2=a;}
  | _ -> { e with a2=a;}

  let merge_id e1 e2 = match e1.edge,e2.edge with
    | Id,Id ->
        begin
          let a1 = e1.a2 and a2 = e2.a1 in
          match a1,a2 with
          | None,None -> Some e1
          | (None,(Some _ as a))|((Some _ as a),None) ->
              Some { e1 with a1=a; a2=a; }
          | Some a1,Some a2 ->
              begin
                match merge_atoms a1 a2 with
                | None -> None (* Merge impossible, will fail later *)
                | Some _ as a ->
                    Some { e1 with a1=a; a2=a; }
              end
        end
    | _ -> None

  let merge_ids =
    let rec do_rec fst = function
      | [] -> fst,[]
      | [lst] ->
          begin
            match merge_id lst fst with
            | None -> fst,[lst]
            | Some e -> e,[]
          end
      | e1::(e2::es as k) ->
          begin
            match merge_id e1 e2 with
            | None ->
                let fst,k = do_rec fst k in
                fst,e1::k
            | Some e -> do_rec fst (e::es)
          end in
    let rec do_fst = function
      | []|[_] as es -> es
      | e1::(e2::es as k) ->
          begin
            match merge_id e1 e2 with
            | None ->
                let fst,k = do_rec e1 k in
                fst::k
            | Some e -> do_fst (e::es)
          end in
    do_fst

(*
 resolve_pair e1 e2, merges the end annotation of e1 with
 the start annotation of e2.
 Warning: resolve_pair cannot fail, instead it must leave
 e1 and e2 as they are...
*)
  let resolve_pair e1 e2 =
    if dbg > 0 then
      eprintf
        "Resolve pair <%s,%s> -> " (debug_edge e1)  (debug_edge e2) ;
    let e1,e2 =
      try
        let d1 = dir_tgt e1 and d2 = dir_src e2 in
        match d1,d2 with
        | Irr,Dir d -> set_tgt d e1,e2
        | Dir d,Irr -> e1,set_src d e2
        | _,_ -> e1,e2
      with NotThat _ -> e1,e2 in
    let a1 = e1.a2 and a2 = e2.a1 in
    let r =
      match a1,a2 with
      | None,None -> e1,e2
      | None,Some a
      | Some a,None when is_ifetch (Some a)-> e1, e2
      | None,Some _ -> set_a2 e1 a2,e2
      | Some _,None -> e1, set_a1 e2 a1
      | Some a1,Some a2 ->
          begin match merge_atoms a1 a2 with
          | None -> e1,e2
          | Some _ as a ->
              set_a2 e1 a,set_a1 e2 a
          end in
    if dbg > 0 then begin
      let e1,e2 = r in
      eprintf "<%s,%s>\n" (debug_edge e1) (debug_edge e2)
    end ;
    r

  (* Function merge_pair merges two versions of the same edge with
     different annotations and direction resolution.
    It cannot fail *)
  let merge_dir d1 d2 = match d1,d2 with
  | (Irr,Dir d)|(Dir d,Irr) -> d
  | Dir d1,Dir d2 -> assert (d1=d2) ; d1
  | (Irr,Irr)|(NoDir,_)|(_,NoDir) -> assert false

  let merge_atomo a1 a2 = match a1,a2 with
  | None,Some _ -> a2
  | Some _,None -> a1
  | None,None -> None
  | Some a1,Some a2 ->
      begin match merge_atoms a1 a2 with
      | None ->
          Warn.fatal
            "Atoms %s and %s *must* be mergeable"
            (pp_atom a1) (pp_atom a2)
      | Some _ as a -> a
      end

  let merge_pair e1 e2 = match e1.edge,e2.edge with
  | (Id,Id) -> e1
  | (Insert _,_)|(_,Insert _) -> assert false
  | _,_ ->
      let tgt = merge_dir (dir_tgt e1) (dir_tgt e2)
      and src = merge_dir (dir_src e1) (dir_src e2) in
      let e = set_tgt tgt (set_src src e1) in
      { e with a1 = merge_atomo e1.a1 e2.a1; a2 = merge_atomo e1.a2 e2.a2; }

  let default_access = Cfg.naturalsize,0

  let replace_plain_atom a = match get_access_atom a with
    | Some _ -> a
    | None -> A.set_access_atom a default_access

  let replace_plain e =
    let a1 = replace_plain_atom e.a1
    and a2 = replace_plain_atom e.a2 in
    { e with a1; a2; }

  let remove_id = List.filter (fun e -> not (is_id e.edge))

  let check_mixed =
    if not do_mixed || do_disjoint then fun _ -> ()
    else
      List.iter
        (fun e ->
          if not (ok_mixed e.edge e.a1 e.a2) then begin
              match e.edge with
              | Rmw _ ->
                  Warn.fatal
                    "Illegal mixed-size Rmw edge: %s"
                    (pp_edge e)
              | _ ->
                  if same_access_atoms e.a1 e.a2 then
                    Warn.fatal
                      "Identical mixed access in %s and `-variant MixedStrictOverlap` mode"
                      (pp_edge e)
                  else
                    Warn.fatal
                      "Non overlapping accesses in %s, allow with `-variant MixedDisjoint`"
                      (pp_edge e)
          end)

  let resolve_edges es0 =
    let es0 = merge_ids es0 in
    match es0 with
  | []|[_] -> es0
  | e::es ->
      let rec do_rec e es = match e.edge with
      | Insert _|Store ->
          let fst,nxt,es = do_recs es in
          fst,e,nxt::es
      | _ ->
          begin try
            let es0,e1,es1 = find_non_insert_store es in
            let e,e1 = resolve_pair e e1 in
            let fst,f,es = do_recs (es0@(e1::es1)) in
            fst,e,f::es
          with Not_found -> try
            let _,e1,_ = find_non_insert_store es0 in
            let e,e1 = resolve_pair e e1 in
            e1,e,es
          with Not_found -> Warn.user_error "No non-insert-store node in cycle"
          end
      and do_recs = function
(* This case is handled by Not_found handler above *)
        | [] -> assert false
        | e::es -> do_rec e es in
      let fst,e,es = do_rec e es in
      let e =
        match e.edge with
        | Insert _ -> e
        | _ ->
            try merge_pair fst e
            with exn ->
              eprintf "Failure <%s,%s>\n" (debug_edge fst) (debug_edge e) ;
              raise exn in
      let es = remove_id (e::es) in
      let es = if do_mixed then List.map replace_plain es else es in
      if dbg > 0 then eprintf "Check Mixed: %s\n" (pp_edges es) ;
      check_mixed es ;
      es

(********************)
(* Atomic variation *)
(********************)

(* Apply atomic variation to nodes with no atomicity (ie a = None)
   This is done after a resolution step (see resolve_edge above),
   with leaves a1 and a2 to None when there us not neighbouring atomic
   specification. One atomic variation has been applied to all the a` fields
   of all edges, we do another step of resolution, so as to set the neighbouring
   a2 *)

  let var_fence e f r = match e.edge with
  | Fenced (fe,sd,ex1,ex2) when F.compare_fence fe F.strong = 0 ->
      F.var_fence
        (fun fe r -> f {e with edge = Fenced (fe,sd,ex1,ex2)} r) r
  | _ -> f e r



  let varatom es f r =
    let rec var_rec ves es r = match es with
    | [] -> f (resolve_edges (List.rev ves)) r
    | e::es ->
        var_fence e
          (fun e r -> match e.a1 with
          | Some _ -> var_rec (e::ves) es r
          | None ->
              begin match dir_src e with
              | Dir d ->
                  A.varatom_dir d
                    (fun a r -> var_rec ({e with a1=a}::ves)  es r)
                    r
              | NoDir ->  var_rec (e::ves) es r
              | Irr ->  assert false (* resolved at this step *)
              end)
          r in
    var_rec [] es r


(* compact *)

  let seq_sd e1 e2 =
    match Code.seq_sd (loc_sd e1) (loc_sd e2) with
    | None -> Warn.user_error "Unexpected UnspecLoc"
    | Some b -> b


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

  let get_machine_feature e =
    StringSet.union (A.get_machine_feature e.a1) (A.get_machine_feature e.a2)

  module Set =
    MySet.Make
      (struct
        type t = edge
        let compare = compare
      end)

  module Map =
    MyMap.Make
      (struct
        type t = edge
        let compare = compare
      end)

  let show =
    let open ShowGen in
    function
      | Edges ->
          let es = fold_pp_edges (fun s k -> s::k) [] in
          let es = List.sort String.compare es in
          List.iter (eprintf " %s") es ;
          eprintf "\n%!"
      | Annotations ->
          let es =
            fold_atomo
              (fun ao k ->
                if is_ifetch ao then k
                else { edge=Id; a1=ao; a2=ao;}::k)
              [] in
          List.iter
            (fun e -> eprintf " %s" (pp_edge e))
            es ;
          eprintf "\n%!"
      | Fences ->
          F.fold_all_fences (fun f () -> eprintf " %s" (pp_fence f)) () ;
          eprintf "\n%!"
end
