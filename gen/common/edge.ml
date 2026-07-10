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
    let variant = Variant_gen.empty
    let naturalsize = TypBase.get_size TypBase.default
    let wildcard = false
    let debug = Debug_gen.none
  end

module type S = sig
  open Code

  type fence
  type dp

  include Atom.AtomType

  val wildcard : bool
  val do_self : bool
  type value = Value.v

  val pp_atom : atom -> string
  val pp_atom_separate : atom -> string list
  val tr_value : atom option -> value -> value
  val overwrite_value : value -> atom option -> value -> value
  val extract_value : value -> atom option -> value
  val set_pteval :
    atom option -> Value.pte -> (unit -> string) -> Value.pte
  val instr_atom : atom option
  val is_ifetch : atom option -> bool
  val atom_to_bank : atom option -> SIMD.atom Code.bank

(* edge proper *)
  type tedge =
    | Communication of com * ie
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
  val is_dp_data : tedge -> bool
  val compute_rmw : RMW.rmw -> value -> value -> value
  val is_valid_rmw : RMW.rmw list -> bool

  type edge = { edge: tedge;  a1:atom option; a2: atom option; }

  val plain_edge : tedge -> edge

  val fold_atomo : (atom option -> 'a -> 'a) -> 'a -> 'a
  val fold_atomo_list : atom option list -> (atom option -> 'a -> 'a) -> 'a -> 'a
  val fold_edge_wildcards : (string -> edge list list -> 'a -> 'a) -> 'a -> 'a
  val fold_edge_legacy_aliases :
    (string -> edge list list -> 'a -> 'a) -> 'a -> 'a
  val fold_edge_legacy_wildcards :
    (string -> edge list list -> 'a -> 'a) -> 'a -> 'a

  val pp_atom_option : atom option -> string

  val pp_edge : edge -> string
  val compare_atomo : atom option -> atom option -> int
  val compare : edge -> edge -> int

  val parse_atoms : string list -> atom option list
  val get_access_atom: atom option -> Mixed.t option

  val equal_edge_atoms : edge -> edge -> bool

  val parse_fence : string -> fence
  val parse_edge_annotations : string -> (atom option * atom option) option
  val parse_edge : string -> edge
  val parse_edges : string -> edge list

  val pp_edges : ?separate:bool -> edge list -> string

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
  val is_co : edge -> bool
  val is_fr : edge -> bool
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
         val variant : Variant_gen.set
         val naturalsize : MachSize.sz
         val wildcard : bool
         val debug : Debug_gen.t
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
  let do_self = Variant_gen.has Variant_gen.Self Cfg.variant
  let do_mixed = Variant_gen.is_mixed Cfg.variant
  let do_kvm =  Variant_gen.is_kvm Cfg.variant
  let do_disjoint = Variant_gen.has Variant_gen.MixedDisjoint Cfg.variant
  let do_strict_overlap = Variant_gen.has Variant_gen.MixedStrictOverlap Cfg.variant
  let wildcard = Cfg.wildcard

  open Code

  type fence = F.fence
  type dp = F.dp

  type atom = A.atom
  module Value = A.Value
  module SIMD = A.SIMD
  module RMW = A.RMW

  type value = A.Value.v

  let pre_parse_string s =
    let parsed_result = Lexing.from_string (String.trim s)
    |> LexUtil.parse Parser.main
    |> Ast.expand (fun _ -> Warn.fatal "predicate should not exist.") in
    match parsed_result with
      | [x] -> x
      | _ ->
        Warn.user_error "only accepts exactly one input cycle."

  let compute_rmw rmw old operand =
    Value.from_int @@ RMW.compute_rmw rmw ~old:(Value.to_int old) ~operand:(Value.to_int operand)

  let is_valid_rmw = RMW.is_valid_rmw

  let pp_atom = A.pp_atom
  let pp_atom_separate = A.pp_atom_separate
  let tr_value = A.tr_value
  let overwrite_value = A.overwrite_value
  let extract_value = A.extract_value
  let set_pteval ao p = match ao with
  | None -> fun _ -> p
  | Some a -> A.Value.set_pteval a p

  let merge_atoms = A.merge_atoms
  let instr_atom = A.instr_atom
  let is_ifetch = A.is_ifetch

  let atom_to_bank = function
    | None -> Ord
    | Some a -> A.atom_to_bank a

(* edge proper *)
  type tedge =
    | Communication of com * ie
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
    | Store|Insert _|Hat|Rmw _|Communication _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Node _ -> false

  let is_insert_store = function
    | Store|Insert _ -> true
    | Id|Hat|Rmw _|Communication _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Node _ -> false

  let is_node = function
    | Node _ -> true
    | Id|Hat|Rmw _|Communication _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _|Insert _
    | Store -> false

  let is_non_pseudo = function
    | Store|Insert _ |Id|Node _-> false
    | Hat|Rmw _|Communication _|Po (_, _, _)
    | Fenced (_, _, _, _)|Dp (_, _, _)|Leave _|Back _ -> true

  let is_dp_addr = function
    |Dp (dp, _, _) -> F.is_addr dp
    |_ -> false

  let is_dp_data = function
    |Dp (dp, _, _) -> F.is_data dp
    |_ -> false

  type edge = { edge: tedge;  a1:atom option; a2: atom option; }

  let can_merge e = not @@ is_insert_store e.edge

  open Printf

  let plain_edge e = { a1=None; a2=None; edge=e; }

  let pp_plain = A.pp_plain

  let pp_atom_option = function
    | None -> pp_plain
    | Some a -> pp_atom a

  let pp_annotations edge a1 a2 = match edge, a1, a2 with
  | Id, _, _ -> pp_atom_option a1
  | _, None, None -> ""
  | _, _, _ -> sprintf "%s%s" (pp_atom_option a1) (pp_atom_option a2)

  let pp_tedge = function
    | Communication (com,ie) -> sprintf "%s%s" (pp_com com) (pp_ie ie)
    | Po (sd,e1,e2) ->
      sprintf "Po%s%s%s" (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Fenced (f,sd,e1,e2) ->
      sprintf "%s%s%s%s" (F.pp_fence f) (pp_sd sd) (pp_extr e1) (pp_extr e2)
    | Dp (dp,sd,e) ->
      sprintf "Dp%s%s%s"(F.pp_dp dp) (pp_sd sd) (pp_extr e)
    | Hat -> "Hat"
    | Rmw rmw-> RMW.pp_rmw false rmw
    | Leave c -> sprintf "%sLeave" (pp_com c)
    | Back c -> sprintf "%sBack" (pp_com c)
    | Id -> "Id"
    | Insert f -> F.pp_fence f
    | Store -> "Store"
    | Node W -> "Write"
    | Node R -> "Read"

  let debug_edge e =
    sprintf
      "{edge=%s, a1=%s, a2=%s}"
      (pp_tedge e.edge) (pp_atom_option e.a1) (pp_atom_option e.a2)

  let pp_edge e =
    let edge = match e.edge with
    | Id -> ""
    | _ -> pp_tedge e.edge in
    let annotation = pp_annotations e.edge e.a1 e.a2 in
    edge ^ annotation

  let compare_atomo = Option.compare A.compare_atom

  let compare e1 e2 = match compare_atomo e1.a1 e2.a1 with
  | 0 ->
      begin match  compare_atomo e1.a2 e2.a2 with
      | 0 -> compare e1.edge e2.edge
      | r -> r
      end
  | r -> r

  let do_dir_tgt_com = function
    | Rf -> Dir R
    | Co|Fr -> Dir W

 and do_dir_src_com = function
   | Rf|Co -> Dir W
   | Fr -> Dir R

  let do_dir_tgt e = match e with
  | Po(_,_,e)| Fenced(_,_,_,e)|Dp (_,_,e) -> e
  | Hat -> Dir R
  | Rmw _  -> Dir W
  | Communication (c, _)
  | Leave c|Back c -> do_dir_tgt_com c
  | Id -> NoDir
  | Insert _ -> NoDir
  | Store -> Dir W
  | Node d -> Dir d


  and do_dir_src e = match e with
  | Po(_,e,_)| Fenced(_,_,e,_) -> e
  | Dp _|Hat|Rmw _ -> Dir R
  | Communication(c, _)
  | Leave c|Back c -> do_dir_src_com c
  | Id -> NoDir
  | Insert _ -> NoDir
  | Store -> Dir W
  | Node d -> Dir d

  let do_loc_sd e = match e with
  | Po (sd,_,_) | Fenced (_,sd,_,_) | Dp (_,sd,_) -> sd
  | Insert _|Store|Node _|Communication _|Hat|Rmw _|Id|Leave _|Back _ -> Same

  let do_is_diff e = Code.is_diff_loc @@ do_loc_sd e

(* Primitive edge lexemes accepted by `parse_edge`.  Wildcard edge names that
   are now macros, such as Dp**, are added to the relax macro table instead of
   this lexer table. *)
let fold_tedges f r =
  r
  (* Communication edges. *)
  |> fold_com
    (fun com r -> fold_ie (fun ie -> f (Communication (com,ie))) r)
  (* Atomic read-modify-write edges. *)
  |> RMW.fold_rmw (fun rmw -> f (Rmw rmw))
  (* Program-order edges. *)
  |> fold_sd_extr_extr (fun sd e1 e2 r -> f (Po (sd,e1,e2)) r)
  (* Fence insertion pseudo-edges. *)
  |> F.fold_all_fences (fun fe -> f (Insert fe))
  (* Store insertion pseudo-edge. *)
  |> f Store
  (* Concrete fence edges. *)
  |> F.fold_all_fences
    (fun fe ->
      fold_sd_extr_extr
        (fun sd e1 e2 -> f (Fenced (fe,sd,e1,e2))))
  (* Concrete dependency edges. *)
  |> F.fold_dp
    (fun dp r ->
      List.fold_right
        (fun d r -> fold_sd (fun sd -> f (Dp (dp,sd,Dir d))) r)
        (F.expand_dp_dir dp) r)
  (* Identity edge for annotation. *)
  |> f Id
  (* Read/write node pseudo-edges. *)
  |> f (Node R)
  |> f (Node W)
  (* Hat edge. *)
  |> f Hat
  (* Scope leave/back pseudo-edges. *)
  |> fold_com (fun c r -> f (Leave c) r)
  |> fold_com (fun c r -> f (Back c) r)

  let fold_atomo f k = f None (A.fold_atom (fun a k -> f  (Some a) k) k)
  let fold_atomo_list aos f k = List.fold_right (fun a k -> f a k) aos k

  let overlap_atoms a1 a2 =
    match a1,a2 with
    | (None,_)|(_,None) -> true
    | Some a1,Some a2 -> A.overlap_atoms a1 a2

  let get_access_atom = A.get_access_atom

  let equal_atomo = Option.equal (fun a1 a2 -> A.compare_atom a1 a2 = 0)

  let equal_tedge lhs rhs = match lhs,rhs with
  | Communication (c1,ie1),Communication (c2,ie2) ->
      Code.equal_com c1 c2 && Code.equal_ie ie1 ie2
  | Po (sd1,e11,e12),Po (sd2,e21,e22) ->
      Code.equal_sd sd1 sd2 && Code.equal_extr e11 e21 &&
      Code.equal_extr e12 e22
  | Fenced (f1,sd1,e11,e12),Fenced (f2,sd2,e21,e22) ->
      F.compare_fence f1 f2 = 0 && Code.equal_sd sd1 sd2 &&
      Code.equal_extr e11 e21 && Code.equal_extr e12 e22
  | Dp (dp1,sd1,e1),Dp (dp2,sd2,e2) ->
      F.equal_dp dp1 dp2 && Code.equal_sd sd1 sd2 && Code.equal_extr e1 e2
  | Leave c1,Leave c2
  | Back c1,Back c2 -> Code.equal_com c1 c2
  | Id,Id
  | Store,Store
  | Hat,Hat -> true
  | Insert f1,Insert f2 -> F.compare_fence f1 f2 = 0
  | Node d1,Node d2 -> Code.equal_extr (Dir d1) (Dir d2)
  | Rmw rmw1,Rmw rmw2 -> RMW.equal_rmw rmw1 rmw2
  | (Communication _|Po _|Fenced _|Dp _|Leave _|Back _|Id
    |Insert _|Store|Node _|Hat|Rmw _),_ -> false

  let equal_edge_atoms lhs rhs =
    equal_tedge lhs.edge rhs.edge &&
    equal_atomo lhs.a1 rhs.a1 && equal_atomo lhs.a2 rhs.a2

  let same_access_atoms a1 a2 =
    Misc.opt_eq Mixed.equal (get_access_atom a1) (get_access_atom a2)

  (* For rmw instruction any accesses is a priori.
     However identical accesses are forced for rmw instructions *)
  let ok_rmw rmw a1 a2 =
    not (RMW.is_one_instruction rmw) || same_access_atoms a1 a2

  let ok_non_rmw e a1 a2 =
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

  let dir_tgt e = do_dir_tgt e.edge
  and dir_src e = do_dir_src e.edge
  and safe_dir e =
    match do_dir_src e.edge with
    | Dir d -> Some d
    | NoDir|Irr -> None

(***************)
(* Atom lexing *)
(***************)

  let iter_atom = Misc.fold_to_iter fold_atomo

  let annotation_lookup_table = Hashtbl.create 37

  let add_lxm_atom lxm a =
    if Cfg.debug.Debug_gen.lexer then eprintf "ADD ANNOTATION: %s\n" lxm ;
    try
      let old = Hashtbl.find annotation_lookup_table lxm in
      assert (compare_atomo old a = 0) ;
    with Not_found ->
      Hashtbl.add annotation_lookup_table lxm a

  let () = iter_atom (fun a -> add_lxm_atom (pp_atom_option a) a)

  let parse_atom s =
    try Hashtbl.find annotation_lookup_table s
    with Not_found -> Warn.fatal "Bad atom: %s" s

  let parse_atoms xs =
    List.map
      ( fun x ->
        pre_parse_string x
        |> List.map parse_atom
      ) xs
    |> List.flatten

  let get_access_atom = A.get_access_atom


(**********)
(* Lexing *)
(**********)

  let edge_lookup_table = Hashtbl.create 40000

  let add_lxm_edge lxm e =
    if Cfg.debug.Debug_gen.lexer then eprintf "ADD EDGE: %s\n" lxm ;
    try
      let old = Hashtbl.find edge_lookup_table lxm in
      if compare old e <> 0 then begin
        Warn.warn_always "ambiguous lexeme: %s" lxm ;
        eprintf "%s\n%s\n" (debug_edge old) (debug_edge e) ;
        assert false
      end

    with Not_found ->
      Hashtbl.add edge_lookup_table lxm e

  let () =
    (* Fill the primitive edge lexeme table. *)
    Misc.fold_to_iter fold_tedges
      (fun te -> add_lxm_edge (pp_tedge te) (plain_edge te));
    add_lxm_edge "R" (plain_edge (Node R)) ;
    add_lxm_edge "W" (plain_edge (Node W)) ;
    ()

  let fold_pp_edges f =
    Hashtbl.fold
      (fun s e k ->
        if e.a1=None && e.a2=None && e.edge <> Id then
          f s k
        else k)
      edge_lookup_table

  let parse_fence s =
    let fences_pp =
      F.fold_all_fences
        (fun f k -> (F.pp_fence f,f)::k)
        [] in
    try List.assoc s fences_pp
    with Not_found -> Warn.fatal "%s is not a fence" s

  (* Explore prefixes from longest to shortest until `func` returns `Some ..`.
     For example, given the input `"PosWRPA"`, it applies
     `func "PosWRPA"`, then `func "PosWRP"`, and so on, until `func`
     returns `Some result`. The helper then returns that result together
     with the untouched suffix, for example `Some (result, "PA")`. *)
  let fold_prefixes func string =
    let rec do_rec i =
      if i <= 0 then None
      else
        let prefix = String.sub string 0 i in
        match func prefix with
        | Some x -> Some (x, (String.sub string i (String.length string - i)))
        | None -> do_rec (i - 1)
    in
    do_rec (String.length string)

  let lookup_edge_prefix string =
    fold_prefixes (fun prefix -> Hashtbl.find_opt edge_lookup_table prefix) string

  let lookup_atom_prefix string =
    fold_prefixes (fun prefix -> Hashtbl.find_opt annotation_lookup_table prefix) string

  let annotation_edge a = { a1=a; a2=a; edge=Id }

  let parse_annotation input =
    match lookup_atom_prefix input with
    | Some (annotation, "") -> Some annotation
    | _ -> None

  let check_invalid_annotation input =
    let annotation =
      let len = String.length input in
      if len > 2 && input.[1] = '.' then String.sub input 2 (len - 2)
      else input in
    let len = String.length annotation in
    if do_mixed && len > 1 then
      let size = match annotation.[0] with
        | 'b' -> Some MachSize.Byte
        | 'h' -> Some MachSize.Short
        | 'w' -> Some MachSize.Word
        | 'q' -> Some MachSize.Quad
        | 's' -> Some MachSize.S128
        | _ -> None in
      match size,int_of_string_opt (String.sub annotation 1 (len - 1)) with
      | Some size,Some offset when offset mod MachSize.nbytes size <> 0 ->
          sprintf
            "Misaligned mixed-size annotation %s: offset %d is not aligned to the %d-byte access size"
            input offset (MachSize.nbytes size)
      | _,_ -> input
    else input

  (* Parse two edge annotations, for example `AL`. *)
  let parse_edge_annotations string =
    match lookup_atom_prefix string with
    | None -> None
    | Some (left_annotation, suffix) ->
        match lookup_atom_prefix suffix with
        | Some (right_annotation, "") ->
            Some (left_annotation, right_annotation)
        | _ -> None

  (* Parse `input`.
     - For an edge-only input such as `PosWW`, return the edge.
     - For an annotation-only input such as `L`, return an annotated `Id` edge.
     - For a composite input such as `PosWWLA`, attach `L` and `A` as
       the edge annotations.
     - Otherwise, fail. *)
  let parse_edge input =
    let parse_annotation_only () =
      match parse_annotation input with
      | Some annotation -> annotation_edge annotation
      | None ->
          let message = check_invalid_annotation input in
          Warn.user_error "Bad edge: %s" message in
    match lookup_edge_prefix input with
    | None -> parse_annotation_only ()
    | Some (edge, "") -> edge
    (* If there is a suffix, it must parse as two annotations. Otherwise,
       try parsing the whole input as an annotation-only edge, for annotation
       names that start with an edge prefix. *)
    | Some (edge, suffix) ->
        begin match parse_edge_annotations suffix with
        | Some (a1, a2) -> {edge with a1; a2}
        | _ -> parse_annotation_only ()
        end

  let parse_edges s =
    pre_parse_string s |> List.map parse_edge

  (* Separating annotations produces a cycle description that can be passed
     back to diyone7, even when several annotations have been merged. *)
  let pp_edges ?(separate=false) es =
    if separate then
      List.concat_map
        (fun e ->
          (* An instruction annotation cannot stand alone, so edges touching
             an instruction access must keep their composite spelling. *)
          if A.is_ifetch e.a1 || A.is_ifetch e.a2 then [pp_edge e]
          else
            let edge = match e.edge with Id -> [] | _ -> [pp_tedge e.edge] in
            let annotations = match e.a2 with
            | None -> []
            | Some atom when A.compare_atom atom A.default_atom = 0 -> []
            | Some atom -> pp_atom_separate atom in
            edge@annotations)
        es
      |> String.concat " "
    else String.concat " " (List.map pp_edge es)

  let do_set_tgt d e = match e  with
  | Po(sd,src,_) -> Po (sd,src,Dir d)
  | Fenced(f,sd,src,_) -> Fenced(f,sd,src,Dir d)
  | Dp (dp,sd,_) -> Dp (dp,sd,Dir d)
  | Communication _ | Hat
  | Insert _|Store|Id|Node _|Rmw _|Leave _|Back _-> e

  and do_set_src d e = match e with
  | Po(sd,_,tgt) -> Po(sd,Dir d,tgt)
  | Fenced(f,sd,_,tgt) -> Fenced(f,sd,Dir d,tgt)
  | Communication _|Hat|Dp _
  | Insert _|Store|Id|Node _|Rmw _|Leave _|Back _ -> e

  let set_tgt d e = { e with edge = do_set_tgt d e.edge ; }
  and set_src d e = { e with edge = do_set_src d e.edge ; }

  let loc_sd e = do_loc_sd e.edge

  let get_ie e = match e.edge with
  | Id |Po _|Dp _|Fenced _|Rmw _ -> Int
  | Communication (_,ie) -> ie
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
  | Communication (_,Ext)
  | Leave _|Back _ -> true
  | _ -> false

  let is_com e = match e.edge with
  | Communication _|Leave _|Back _| Hat -> true
  | _ -> false

  let is_co e = match e.edge with
  | Communication (Co,_) -> true
  | _ -> false

  let is_fr e = match e.edge with
  | Communication (Fr,_) -> true
  | _ -> false

  let is_fetch e = match e.edge with
  | Communication (Rf,_) -> is_ifetch e.a2
  | Communication (Fr,_) -> is_ifetch e.a1
  | _ -> is_ifetch e.a1 || ( loc_sd e = Same && is_ifetch e.a2)

  let set_a1 e a = match e.edge with
  | Node _|Id -> { e with a1=a; a2=a;}
  | _ -> { e with a1=a;}

  let set_a2 e a = match e.edge with
  | Node _|Id  -> { e with a1=a; a2=a;}
  | _ -> { e with a2=a;}

  (* Merges the end annotation and direction of `e1`
     with the start of `e2`. *)
  let merge_pair e1 e2 =
    let update_dir (e1,e2) =
      let d1 = dir_tgt e1 and d2 = dir_src e2 in
      match d1,d2 with
      | Irr,Dir d -> Some(set_tgt d e1,e2)
      | Dir d,Irr -> Some(e1,set_src d e2)
      | _,_ -> None in
    let update_annotation (e1,e2) =
      let a1 = e1.a2 and a2 = e2.a1 in
      match a1,a2 with
      | None,None -> None
      | None,Some a
      | Some a,None when is_ifetch (Some a) -> None
      | None,Some _ -> Some(set_a2 e1 a2,e2)
      | Some _,None -> Some(e1,set_a1 e2 a1)
      | Some a1,Some a2 -> match merge_atoms a1 a2 with
        | None when is_id e1.edge && is_id e2.edge ->
            Warn.fatal "Incompatible annotations %s and %s"
              (pp_atom a1) (pp_atom a2)
        | None -> None
        | Some _ as atom -> Some(set_a2 e1 atom,set_a1 e2 atom) in
    let input = (e1,e2) in
    let r = update_dir input
        |> ( function
          (* Propagate result `f e` if changed *)
          | Some e -> Some(Option.value (update_annotation e) ~default:e)
          | None -> update_annotation input ) in
    if Cfg.debug.Debug_gen.parser then begin
      let i1,i2 = input in
      let r1,r2 = Option.value ~default:input r in
      eprintf "MERGE PAIR <%s,%s> -> <%s,%s>\n"
        (debug_edge i1) (debug_edge i2) (debug_edge r1) (debug_edge r2)
    end ;
    r

  let can_precede_atoms x y = match x.a2,y.a1 with
  | None,_
  | _,None -> true
  | Some a1,Some a2 -> Option.is_some (merge_atoms a1 a2)

  let can_precede_dp_data_read x y = match x.edge with
  | Dp (dp,_,Dir R) when F.is_data dp ->
      begin match y.edge with Rmw _ -> true | _ -> false end
  | _ -> true

  let valid_atoms edge =
    let applies atom dir = match atom,dir with
      | None,_ | _,(NoDir|Irr) -> true
      | Some atom,Dir dir -> A.applies_atom atom dir in
    applies edge.a1 (dir_src edge) && applies edge.a2 (dir_tgt edge) &&
    (* Further filter on annotation on rmw *)
    match edge.edge with
    | Rmw rmw -> A.RMW.applies_atom_rmw rmw edge.a1 edge.a2
    | _ -> true

  let can_precede x y =
    can_precede_dirs x y && can_precede_atoms x y &&
    can_precede_dp_data_read x y &&
    let merged_x,merged_y = Option.value ~default:(x,y) (merge_pair x y) in
    valid_atoms merged_x && valid_atoms merged_y

(*************************************************************)
(* Add architecture-depended edge-related macros to macros table *)
(*************************************************************)
  let add_po_macros f k =
    let pp_po_macro sd d1 d2 = match sd,d1,d2 with
    | None,None,None -> "Po"
    | _,_,_ -> sprintf "Po%s%s%s" (pp_sd_macro sd) (pp_dir_macro d1) (pp_dir_macro d2) in
    fold_sd_extr_extr_macros
      (fun sd d1 d2 choices k ->
        let choices =
          List.map
            (fun (sd,d1,d2) -> [plain_edge (Po (sd,Dir d1,Dir d2))])
            choices in
        f (pp_po_macro sd d1 d2) choices k)
      k

  let add_fence_macros f k =
    let pp_fence_macro fe sd d1 d2 = match sd,d1,d2 with
    | None,None,None -> sprintf "%s***" (F.pp_fence fe)
    | _,_,_ -> sprintf "%s%s%s%s"
        (F.pp_fence fe) (pp_sd_macro sd) (pp_dir_macro d1) (pp_dir_macro d2) in
    F.fold_all_fences
      (fun fe k ->
        fold_sd_extr_extr_macros
          (fun sd d1 d2 choices k ->
            let choices =
              List.map
                (fun (sd,d1,d2) -> [plain_edge (Fenced (fe,sd,Dir d1,Dir d2))])
                choices in
            f (pp_fence_macro fe sd d1 d2) choices k)
          k)
      k

  let add_strong_fence_alias sd d1 d2 f k =
    let name = sprintf "Fence%s%s%s" (pp_sd sd) (pp_dir d1) (pp_dir d2) in
    let edge = plain_edge (Fenced (F.strong,sd,Dir d1,Dir d2)) in
    f name [[edge]] k

  let add_strong_fence_aliases f k =
    fold_sd
      (fun sd k ->
        expand_dir_macro None
          (fun d1 k ->
            expand_dir_macro None
              (fun d2 k -> add_strong_fence_alias sd d1 d2 f k)
              k)
          k)
      k

  let add_strong_fence_wildcards f k =
    fold_sd_extr_extr_macros
      (fun sd d1 d2 choices k ->
        let name =
          sprintf "Fence%s%s%s"
            (pp_sd_macro sd) (pp_dir_macro d1) (pp_dir_macro d2) in
        let choices =
          List.map
            (fun (sd,d1,d2) ->
              [plain_edge (Fenced (F.strong,sd,Dir d1,Dir d2))])
            choices in
        f name choices k)
      k

  (* Add all dependency-related macro names for one dependency kind.  The
     direction wildcard is not always fully unfolded to both R and W: each
     dependency kind restricts the allowed target directions through
     `F.expand_dp_dir`.  `pp_dp_macro` names the macro form being added. *)
  let add_dp_macros pp_dp_macro dp f k =
    (* Get the allowed `dir` expansion of `dp` *)
    let dirs = F.expand_dp_dir dp in
    fold_sd_extr_macros
      (fun sd e choices k ->
        let filter_choices =
          List.fold_left
            (fun k (sd,d) ->
              (* `fold_sd_extr_macros` expands directions generically.  Filter
                 them here, since each dependency kind allows only its own
                 target directions. *)
              if List.mem d dirs then [plain_edge (Dp (dp,sd,Dir d))]::k
              else k)
            [] choices in
        if Misc.nilp filter_choices then k
        else f (pp_dp_macro dp sd e) filter_choices k)
      k

  let add_default_dp_wildcard tag dpo f k = match dpo with
  | None -> k
  | Some dp ->
      add_dp_macros
        (fun _dp sd d -> sprintf "%s%s%s" tag (pp_sd_macro sd) (pp_dir_macro d))
        dp f k

  let add_default_dp_alias tag dpo sd d f k = match dpo with
  | None -> k
  | Some dp ->
      let name = sprintf "%s%s%s" tag (pp_sd sd) (pp_dir d) in
      f name [[plain_edge (Dp (dp,sd,Dir d))]] k

  let fold_edge_legacy_aliases f k =
    let add_ws name ie k = f name [[plain_edge (Communication (Co,ie))]] k in
    k
    (* Add `rmw` aliases *)
    |> RMW.fold_rmw_macros
      (fun name choices k ->
        let choices =
          List.map
            (fun rmws -> List.map (fun rmw -> plain_edge (Rmw rmw)) rmws)
            choices in
        (* One choice means a backward-compatible alias. *)
        match choices with
        | [_] -> f name choices k
        | _ -> k)
    (* Backward-compatible concrete Ws spellings.  The primitive lexer table
       keeps only the preferred Coi/Coe names; Wsi/Wse are relax aliases. *)
    |> add_ws "Wsi" Int
    |> add_ws "Wse" Ext
    (* Old syntax for concrete default dependency aliases, for example
       DpsR, DpdR, CtrlsW, and CtrldW. *)
    |> fold_sd
      (fun sd k ->
        k
        |> add_default_dp_alias "Dp" F.ddr_default sd R f
        |> add_default_dp_alias "Ctrl" F.ctrlr_default sd R f
        |> add_default_dp_alias "Dp" F.ddw_default sd W f
        |> add_default_dp_alias "Ctrl" F.ctrlw_default sd W f)
    (* Old syntax for concrete strong-fence aliases, for example FenceWW. *)
    |> add_strong_fence_aliases f

  (* Edge-level wildcards exported to the relax wildcard table.  These names
     expand to multiple choices, for example Rf -> Rfi/Rfe or
     Pod** -> PodRR/PodRW/PodWR/PodWW. *)
  let fold_edge_wildcards f k =
    let add_com name make_edge k =
      f name (fold_ie (fun com k -> [plain_edge (make_edge com)]::k) []) k in
    k
    (* Communication aliases without an internal/external suffix. *)
    |> add_com "Rf" (fun ie -> Communication (Rf,ie))
    |> add_com "Fr" (fun ie -> Communication (Fr,ie))
    |> add_com "Co" (fun ie -> Communication (Co,ie))

    (* Add `rmw` wildcard *)
    |> RMW.fold_rmw_macros
      (fun name choices k ->
        let choices =
          List.map
            (fun rmws -> List.map (fun rmw -> plain_edge (Rmw rmw)) rmws)
            choices in
        (* More than one choice means a wildcard macro. *)
        match choices with
        | [] | [_] -> k
        | _ -> f name choices k)
    (* Add `Dp` related macro *)
    |> F.fold_dpw
      (fun dp ->
        (* Print the user-facing dependency macro name for the current
           wildcard shape, for example DpAddr, DpAddr*W, or DpAddr**. *)
        let pp_dp_macro dp sd e = match sd,e with
        | None,None -> sprintf "Dp%s" (F.pp_dp dp)
        | _,_ -> sprintf "Dp%s%s%s" (F.pp_dp dp) (pp_sd_macro sd) (pp_dir_macro e) in
        add_dp_macros pp_dp_macro dp f)
    (* Add `Po` related macro *)
    |> add_po_macros f
    (* Add fence-related macro *)
    |> add_fence_macros f

  let fold_edge_legacy_wildcards f k =
    let add_com name make_edge k =
      f name (fold_ie (fun com k -> [plain_edge (make_edge com)]::k) []) k in
    k
    (* Ws is kept as a backward-compatible alias for Co. *)
    |> add_com "Ws" (fun ie -> Communication (Co,ie))
    (* Old default dependency wildcard syntax, for example Dp* and Ctrl*R. *)
    |> add_default_dp_wildcard "Dp" F.ddr_default f
    |> add_default_dp_wildcard "Ctrl" F.ctrlr_default f
    (* Old syntax for strong-fence wildcards, for example Fence***. *)
    |> add_strong_fence_wildcards f

(* resolve *)
  let rec find_next_merge = function
    | [] -> raise Not_found
    | e::es -> (* `Node` or Non-pseudo can be merged *)
      if can_merge e then [],e,es
      else
        let bef,ni,aft = find_next_merge es in
        e::bef,ni,aft

  (* Assume `e` is neither `Store` nor `Insert`.
     Repeatedly merge the next mergeable edge from `es` into `e` until no
     further merge is possible.

     The result is `(continue, merged, remaining)` where:
     - `continue` tells whether such a successor was found,
     - `merged` is the updated `e` (or `None` if merging collapsed it into `Id`), and
     - `remaining` is the unconsumed suffix of `es`. *)
  let rec merge_left (e:edge) (es:edge list) : (bool * edge option * edge list) =
    let is_default e = match e with
      | { edge=Id; a1=None; a2=None; } -> true
      | _ -> false in
    try
      let store_insert,next,rest = find_next_merge es in
      (* throw away a default annotation *)
      if is_default e then true, None, store_insert @ next :: rest
      else if is_default next then merge_left e (store_insert @ rest)
      (* Merge `e` and `next` *)
      else match merge_pair e next with
      | None -> true, Some e, (store_insert @ next :: rest)
      | Some (e, next) ->
        match is_id e.edge, is_id next.edge with
        (* Erase `next` and continue merging *)
        | _,true -> merge_left e (store_insert @ rest)
        (* Indicate erasing `e` *)
        | true,false -> true, None, (store_insert @ next :: rest)
        (* Two non-`Id` *)
        | false,false -> true, Some(e), (store_insert @ next :: rest)
    (* find_next_merge fails, no more node to process *)
    with Not_found -> false, Some e, es

  let default_access = Cfg.naturalsize,0

  let replace_plain_atom a = match get_access_atom a with
    | Some _ -> a
    | None -> A.set_access_atom a default_access

  let replace_plain e =
    let a1 = replace_plain_atom e.a1
    and a2 = replace_plain_atom e.a2 in
    { e with a1; a2; }

  let validate_edges es =
    (* Check mixed size memory access annotation *)
    let check_mixed e =
      if not (not do_mixed || do_disjoint || (ok_mixed e.edge e.a1 e.a2)) then
        Warn.fatal
          ( match e.edge,(same_access_atoms e.a1 e.a2) with
          | Rmw _,_ -> "Illegal mixed-size Rmw edge: %s"
          | _,true ->
              "Identical mixed access in %s and `-variant MixedStrictOverlap` mode"
          | _,false ->
              "Non overlapping accesses in %s, allow with `-variant MixedDisjoint`" )
          (pp_edge e) in
    (* Check `Id` edge are all pseudo annotation *)
    let check_pseudo_id e =
      if is_id e.edge && (is_ifetch e.a1 || is_ifetch e.a2) then
        Warn.fatal "Standalone instruction access annotation is not supported"
      else if is_id e.edge then
        Warn.fatal "Invalid extra annotation %s" (pp_edge e) in
    List.iter (fun e -> check_mixed e; check_pseudo_id e) es;
    (* Match annotations between non-insert edges *)
    let last_non_insert_edge =
      List.rev es
      |> List.find ( fun e -> not @@ is_insert_store e.edge ) in
    List.fold_left (fun prev_annotation e ->
      match is_insert_store e.edge, compare_atomo prev_annotation e.a1 with
      (* carry over the previous result if insert edges *)
      | true,_ -> prev_annotation
      | false,0 -> e.a2
      | false,_ ->
          Warn.fatal "Annotations mismatch between %s %s."
          (pp_atom_option prev_annotation)
          (pp_atom_option e.a1)
    ) last_non_insert_edge.a2 es
    |> ignore


  let resolve_edges es0 =
    (* Find the first mergeable edge and absorb any mergeable successors on
       its right. *)
    let rec find_first es =
      let before,fst,after =
        try find_next_merge es
        with Not_found -> Warn.user_error "No memory access node in cycle" in
      let continuation,fst,after = merge_left fst after in
      match continuation,fst with
      | false, _ ->
        Warn.user_error "Only one relaxation that is not store nor insert."
      | true, None ->
        let before',fst,after = find_first after in
        before @ before',fst,after
      | true, Some(fst) -> before,fst,after in
    (* Merge the rest of the cycle, using `fst` again at the end so the tail
       can still be merged back into the head. *)
    let rec merge_es fst es = match es with
      | [] -> fst, []
      | e::es ->
        if can_merge e then
          let continuation,e,es = merge_left e es in
          match continuation,e with
          | false, None -> assert false
          (* throw away `e` *)
          | true, None -> merge_es fst es
          | true, Some(e) ->
            let fst,es = merge_es fst es in
            fst,e::es
          | false, Some(e) ->
            (* Reach the last relax, will try to merge with `fst` *)
            let e,fst = merge_pair e fst |> Option.value ~default:(e,fst) in
            fst,e::es
        else
          let fst,es = merge_es fst es in fst,e::es in
    let before,fst,es = find_first es0 in
    let fst,es = merge_es fst es in
    let es = before @ fst :: es in
    let es = if do_mixed then List.map replace_plain es else es in
    validate_edges es ;
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
    let rec var_rec ves es k = match es with
    | [] -> f (List.rev ves) k
    | e::es ->
        var_fence e
          (fun e k -> match e.a1 with
          | Some _ -> var_rec (e::ves) es k
          | None ->
              begin match dir_src e with
              | Dir d ->
                  A.varatom_dir d
                    (fun a k -> var_rec ({e with a1=a}::ves)  es k)
                    k
              | NoDir ->  var_rec (e::ves) es k
              | Irr ->  assert false (* resolved at this step *)
              end)
          k in
    var_rec [] es r


(* compact *)

  let seq_sd e1 e2 = Code.seq_sd (loc_sd e1) (loc_sd e2)


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
  | Communication (Co,_),Communication (Co,_)
  | Communication (Fr,_),Communication (Co,_) -> [e1]::k
  | Communication (Rf,_),Communication (Fr,_) ->
      [plain_edge (Communication (Co,Int))]::k
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
          F.fold_all_fences (fun f () -> eprintf " %s" (F.pp_fence f)) () ;
          eprintf "\n%!"
end
