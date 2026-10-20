(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025 Arm Limited and/or its affiliates                         *)
(* <open-source-office@arm.com>                                             *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Sets and relations in normal form, and operations on them. *)

type 'a inter = Inter of 'a list
type 'a seq = Seq of 'a list
type 'a union = Union of 'a list

let get_inter (Inter l : 'a inter) = l
let get_union (Union l : 'a union) : 'a list = l
let get_seq (Seq l : 'a seq) : 'a list = l

let map_union (f : 'a -> 'b) : 'a union -> 'b union =
 fun (Union l) -> Union (List.map f l)

let union : 'a union -> 'a union -> 'a union =
 fun (Union x) (Union y) -> Union (List.append x y)

let union_l : 'a union list -> 'a union =
 fun l -> List.fold_right union l (Union [])

let seq (s1 : 'a seq) (s2 : 'a seq) : 'a seq =
  match (s1, s2) with Seq s1, Seq s2 -> Seq (List.append s1 s2)

let seq_l : 'a seq list -> 'a seq = function
  | [] -> raise (Invalid_argument "Sequence on empty list")
  | x :: xs -> List.fold_left seq x xs

let inter (e1 : 'a inter) (e2 : 'a inter) : 'a inter =
  match (e1, e2) with Inter i1, Inter i2 -> Inter (List.append i1 i2)

(* The sequence of unions is the union of sequences. *)
let seq_of_unions (e1 : 'a seq union) (e2 : 'a seq union) : 'a seq union =
  Union
    (let open Util.List.Infix in
     let* x = get_union e1 in
     let* y = get_union e2 in
     [ seq x y ])

(* The intersection of unions is the union of intersections. *)
let inter_of_unions ~(inter : 'a -> 'a -> 'a) (e1 : 'a union) (e2 : 'a union) :
    'a union =
  Union
    (let open Util.List.Infix in
     let* x = get_union e1 in
     let* y = get_union e2 in
     [ inter x y ])

let set_inter (e1 : 'a inter union) (e2 : 'a inter union) : 'a inter union =
  Union
    (let open Util.List.Infix in
     let* x = get_union e1 in
     let* y = get_union e2 in
     [ inter x y ])

let universe_set = Union [ Inter [] ]

(* Intermediate representation *)

type fence = AArch64Base.barrier
type prim_rel = Fence of fence | Prim of string
type event_kind = Exp | Imp
type event_direction = M | R | W | A | L | Q
type event_field = Plain | TTD | Tag | Instr

type event = {
  kind : event_kind option;
  direction : event_direction;
  field : event_field option;
  is_no_return : bool;
}

let default_event =
  { kind = None; direction = M; field = None; is_no_return = false }

let all_events =
  List.concat_map
    (fun kind ->
      let fields =
        match kind with
        | Exp -> [ Plain; TTD; Tag; Instr ]
        | Imp -> [ TTD; Tag; Instr ]
      in
      List.concat_map
        (fun field ->
          List.map
            (fun direction ->
              {
                kind = Some kind;
                direction;
                field = Some field;
                is_no_return = false;
              })
            [ R; W ])
        fields)
    [ Exp; Imp ]

let event_matches candidate constraint_ =
  (constraint_.kind = None || candidate.kind = constraint_.kind)
  && (constraint_.field = None || candidate.field = constraint_.field)
  &&
  match (candidate.direction, constraint_.direction) with
  | (R | A | Q), (M | R | A | Q) | (W | L), (M | W | L) -> true
  | _ -> false

let compact_event_directions events =
  let rec fold = function
    | [] -> []
    | event :: events ->
        let same, events =
          List.partition
            (fun candidate ->
              candidate.kind = event.kind
              && candidate.field = event.field
              && candidate.is_no_return = event.is_no_return)
            events
        in
        let directions =
          List.map (fun event -> event.direction) (event :: same)
        in
        if List.mem R directions && List.mem W directions then
          { event with direction = M } :: fold events
        else (event :: same) @ fold events
  in
  fold events

let compact_event_fields events =
  let rec fold = function
    | [] -> []
    | event :: events ->
        let same, events =
          List.partition
            (fun candidate ->
              candidate.kind = event.kind
              && candidate.direction = event.direction
              && candidate.is_no_return = event.is_no_return)
            events
        in
        let fields = List.map (fun event -> event.field) (event :: same) in
        if
          List.mem None fields
          || List.for_all
               (fun field -> List.mem (Some field) fields)
               [ Plain; TTD; Tag; Instr ]
        then { event with field = None } :: fold events
        else (event :: same) @ fold events
  in
  fold events

let compact_event_kinds events =
  let rec fold = function
    | [] -> []
    | event :: events ->
        let same, events =
          List.partition
            (fun candidate ->
              candidate.direction = event.direction
              && candidate.field = event.field
              && candidate.is_no_return = event.is_no_return)
            events
        in
        let kinds = List.map (fun event -> event.kind) (event :: same) in
        if
          List.mem None kinds
          || (List.mem (Some Exp) kinds && List.mem (Some Imp) kinds)
        then { event with kind = None } :: fold events
        else (event :: same) @ fold events
  in
  fold events

let rec compact_events events =
  let compacted =
    events |> compact_event_directions |> compact_event_fields
    |> compact_event_kinds
  in
  if compacted = events then events else compact_events compacted

let neg_event event =
  List.filter (fun candidate -> not (event_matches candidate event)) all_events
  |> compact_events

type prim_set =
  | Domain of seq_item seq
  | Range of seq_item seq
  | Rmw of prim_set
  | Event of event
  | Fence of fence option
  | Comp of prim_set
  | Prim of string

and seq_item = Set of prim_set inter | Rel of prim_rel inter
and set_nf = prim_set inter union
and rel_nf = seq_item seq union

let merge_event_sets sets =
  let events, sets =
    List.partition_map
      (function Event event -> Left event | set -> Right set)
      sets
  in
  let fences, sets =
    List.partition_map
      (function Fence fence -> Left fence | set -> Right set)
      sets
  in
  let event =
    List.fold_left
      (fun event next ->
        let direction =
          match (event.direction, next.direction) with
          | M, direction -> direction
          | R, ((A | Q) as direction) | W, (L as direction) -> direction
          | direction, _ -> direction
        in
        {
          kind = (if event.kind = None then next.kind else event.kind);
          direction;
          field = (if event.field = None then next.field else event.field);
          is_no_return = event.is_no_return || next.is_no_return;
        })
      default_event events
  in
  let sets =
    match fences with
    | [] -> sets
    | _ :: _ ->
        let fence = List.find_map (fun fence -> fence) fences in
        Fence fence :: sets
  in
  match events with [] -> sets | _ :: _ -> Event event :: sets

let merge_event_inter (Inter sets) = Inter (merge_event_sets sets)

let is_rmw = function
  | Rel (Inter relations) ->
      List.exists
        (fun (relation : prim_rel) ->
          match relation with Prim ("rmw" | "rmw-noret") -> true | _ -> false)
        relations
  | Set (Inter [ Rmw _ ]) -> true
  | Set _ -> false

let is_amo_or_rmw item =
  is_rmw item
  ||
  match item with
  | Rel (Inter relations) ->
      List.exists
        (fun (relation : prim_rel) ->
          match relation with Prim "amo" -> true | _ -> false)
        relations
  | Set _ -> false

let is_communication = function
  | Rel (Inter relations) ->
      List.exists
        (fun (relation : prim_rel) ->
          match relation with Prim ("rf" | "fr" | "co") -> true | _ -> false)
        relations
  | Set _ -> false

(* Compression *)

let compress_seq (l : seq_item list) : seq_item list =
  l
  |> List.fold_left
       (fun acc x ->
         match (acc, x) with
         | [], Set set -> [ Set (merge_event_inter set) ]
         | [], x -> [ x ]
         | Set s :: acc, Set s' -> Set (merge_event_inter (inter s s')) :: acc
         | Rel r :: acc, Set s -> Set s :: Rel r :: acc
         | acc, Rel (Inter []) -> acc
         | acc, Rel r -> Rel r :: acc)
       []
  |> List.rev

let compress : seq_item seq union -> seq_item seq union =
  let rec compress_set : prim_set -> prim_set = function
    | Domain (Seq r) -> Domain (Seq (compress_seq r))
    | Range (Seq r) -> Range (Seq (compress_seq r))
    | Rmw s -> Rmw (compress_set s)
    | Comp s -> Comp (compress_set s)
    | s -> s
  in
  map_union (fun (Seq s) ->
      let s = compress_seq s in
      let s =
        List.map
          (function
            | Set (Inter set) ->
                Set (merge_event_inter (Inter (List.map compress_set set)))
            | Rel rel -> Rel rel)
          s
      in
      Seq s)

(* Operations on normal forms *)

(* Computes the intersection of two relations in normal form.

   This operation is only well-defined on a subset of expressions.
   In particular, computing the intersection of two relations
   where one of them is a non-trivial sequence. For example:

     r1 & (r2; r3)

   Raises @Failure@ on such cases.
*)
let rel_inter : rel_nf -> rel_nf -> rel_nf =
  let can_intersect (Seq s : seq_item seq) :
      ( prim_set inter,
        prim_set inter * prim_rel inter * prim_set inter )
      Either.t
      option =
    match compress_seq s with
    | [ Set s ] -> Some (Either.Left s)
    | [ Rel r ] -> Some (Either.Right (Inter [], r, Inter []))
    | [ Set s; Rel r ] -> Some (Either.Right (s, r, Inter []))
    | [ Rel r; Set s ] -> Some (Either.Right (Inter [], r, s))
    | [ Set s1; Rel r; Set s2 ] -> Some (Either.Right (s1, r, s2))
    | _ -> None
  in
  let inter (s1 : seq_item seq) (s2 : seq_item seq) : seq_item seq =
    match (can_intersect s1, can_intersect s2) with
    | Some (Left s1), Some (Left s2) -> Seq [ Set (inter s1 s2) ]
    | Some (Right (s1, r, s2)), Some (Right (s1', r', s2')) ->
        Seq [ Set (inter s1 s1'); Rel (inter r r'); Set (inter s2 s2') ]
    | _, _ -> raise (Invalid_argument "Invalid intersection")
  in
  inter_of_unions ~inter

let prim_set : prim_set -> set_nf = fun p -> Union [ Inter [ p ] ]
let prim_rel : prim_rel -> rel_nf = fun p -> Union [ Seq [ Rel (Inter [ p ]) ] ]

(* Computes the fallback complement of a primitive set. Event complements are
   handled over [all_events] by [event_set_comp] below. A generic fence
   complements to generic memory; unsupported primitives remain symbolic so
   that double complements can still reduce: ~~X --> X. *)
let prim_set_comp : prim_set -> prim_set inter union = function
  | Fence None -> prim_set (Event default_event)
  | Comp s -> prim_set s
  | s -> prim_set (Comp s)

let empty_set : set_nf = Union []
let empty_rel : rel_nf = Union []
let domain : rel_nf -> set_nf = map_union (fun s -> Inter [ Domain s ])
let range : rel_nf -> set_nf = map_union (fun s -> Inter [ Range s ])
let fencerel (f : fence) : rel_nf = prim_rel (Fence f)
let rel_union_l : rel_nf list -> rel_nf = union_l
let set_union_l : set_nf list -> set_nf = union_l

let rel_seq_l : rel_nf list -> rel_nf = function
  | [] -> raise (Invalid_argument "Sequence of empty list")
  | x :: xs -> List.fold_left seq_of_unions x xs

let set_inter_l : set_nf list -> set_nf =
 fun l -> List.fold_right set_inter l universe_set

let rel_inter_l : rel_nf list -> rel_nf option =
  let neutral_rel = Union [ Seq [ Rel (Inter []) ] ] in
  fun l ->
    let compute_inter () = List.fold_right rel_inter l neutral_rel in
    try Some (compute_inter ()) with Failure _ -> None

let prim_rel_inter_l (l : prim_rel list) : rel_nf =
  Union [ Seq [ Rel (Inter l) ] ]

let inv : rel_nf -> rel_nf =
 fun (Union l) -> Union (List.map (fun (Seq l) -> Seq (List.rev l)) l)

let to_id : set_nf -> rel_nf =
 fun (Union s) -> Union (List.map (fun x -> Seq [ Set x ]) s)

(* Compute the complement of an intersection of atoms.

   By De Morgan's laws:
   - the complement of a union is the intersection of complements
   - the complement of an intersection is the union of complements
*)
let comp (atom_comp : 'a -> 'a inter union) : 'a inter union -> 'a inter union =
 fun (Union u_l) ->
  set_inter_l
    (List.map (fun (Inter i_l) -> union_l (List.map atom_comp i_l)) u_l)

let event_set_comp = function
  | Union [] -> None
  | Union sets ->
      let rec events = function
        | [] -> Some []
        | Inter set :: sets -> begin
            match (merge_event_sets set, events sets) with
            | [ Event event ], Some events -> Some (event :: events)
            | _ -> None
          end
      in
      Option.map
        (fun negated ->
          all_events
          |> List.filter (fun candidate ->
              List.for_all
                (fun event -> not (event_matches candidate event))
                negated)
          |> compact_events
          |> List.map (fun event -> Inter [ Event event ])
          |> fun sets ->
          Inter [ Fence None ] :: sets |> fun sets -> Union sets)
        (events sets)

let set_comp nf =
  match event_set_comp nf with Some nf -> nf | None -> comp prim_set_comp nf

let set_diff : set_nf -> set_nf -> set_nf =
 fun a b ->
  match b with
  | Union [ Inter [ Prim "NoRet" ] ] ->
      map_union
        (fun (Inter sets) ->
          Inter
            (List.map
               (function
                 | Event event -> Event { event with is_no_return = true }
                 | set -> set)
               sets))
        a
  | _ -> set_inter a (set_comp b)

let find_fence : set_nf -> fence option = function
  | Union [ Inter [ Fence None ] ] -> Some (DSB (SY, FULL))
  | Union [ Inter [ Fence (Some f) ] ] -> Some f
  | _ -> None

(* Parsing string identifiers into IR structures *)

let event_of_name = function
  | "Exp" -> Some { default_event with kind = Some Exp }
  | "Imp" -> Some { default_event with kind = Some Imp }
  | "M" -> Some default_event
  | "R" -> Some { default_event with direction = R }
  | "W" -> Some { default_event with direction = W }
  | "A" -> Some { default_event with direction = A }
  | "L" -> Some { default_event with direction = L }
  | "Q" -> Some { default_event with direction = Q }
  | "TTD" -> Some { default_event with field = Some TTD }
  | "Tag" -> Some { default_event with field = Some Tag }
  | "Instr" -> Some { default_event with field = Some Instr }
  | _ -> None

(*
  For now, we only deal with:

  - explicit effects
  - memory effects
  - fence effects

  Therefore we interpret everything else outside these three classes as
  the empty set.
*)
let parse_set_id (s : string) : set_nf option =
  let try_fences () =
    match s with
    | "DMB.ISH" | "DMB.OSH" | "DMB.SY" | "dmb.sy" | "dmb.full" ->
        Some (prim_set (Fence (Some (AArch64Base.DMB (SY, FULL)))))
    | "DSB.ISH" | "DSB.OSH" | "DSB.SY" | "dsb.sy" | "dsb.full" ->
        Some (prim_set (Fence (Some (AArch64Base.DSB (SY, FULL)))))
    | "DMB.ISHST" | "DMB.OSHST" | "DMB.ST" | "dmb.st" ->
        Some (prim_set (Fence (Some (AArch64Base.DMB (SY, ST)))))
    | "DMB.ISHLD" | "DMB.OSHLD" | "DMB.LD" | "dmb.ld" ->
        Some (prim_set (Fence (Some (AArch64Base.DMB (SY, LD)))))
    | "DSB.ISHST" | "DSB.OSHST" | "DSB.ST" | "dsb.st" ->
        Some (prim_set (Fence (Some (AArch64Base.DSB (SY, ST)))))
    | "DSB.ISHLD" | "DSB.OSHLD" | "DSB.LD" | "dsb.ld" ->
        Some (prim_set (Fence (Some (AArch64Base.DSB (SY, LD)))))
    | "ISB" -> Some (prim_set (Fence (Some AArch64Base.ISB)))
    | _ -> None
  in
  let try_other_prims () =
    match s with
    | ( "Exp" | "Imp" | "M" | "R" | "W" | "A" | "L" | "Q" | "TTD" | "Tag"
      | "Instr" ) as s ->
        Option.map (fun event -> prim_set (Event event)) (event_of_name s)
    | "F" -> Some (prim_set (Fence None))
    | "NoRet" -> Some (prim_set (Prim "NoRet"))
    | "emptyset" | "T" | "NExp" -> Some empty_set
    | _ -> None
  in
  Util.Option.choice_fn [ try_other_prims; try_fences ]

let parse_rel_id (s : string) : rel_nf option =
  let poswr =
    rel_seq_l
      [
        to_id (prim_set (Event { default_event with direction = W }));
        prim_rel_inter_l [ Prim "po"; Prim "loc" ];
        to_id (prim_set (Event { default_event with direction = R }));
      ]
  in
  let pick_basic_dep = poswr in
  match s with
  | "po" | "loc" | "co" | "fr" | "rf" | "ext" | "addr" | "data" | "ctrl" | "rmw"
  | "amo" | "lxsx" | "pick-addr-dep" | "pick-data-dep" | "pick-ctrl-dep"
  | "same-loc" | "sca-class" | "intervening" ->
      Some (prim_rel (Prim s))
  | "pick-basic-dep" -> Some pick_basic_dep
  | "pick-dep" ->
      Some
        (rel_union_l
           [
             pick_basic_dep;
             prim_rel (Prim "pick-addr-dep");
             prim_rel (Prim "pick-data-dep");
             prim_rel (Prim "pick-ctrl-dep");
           ])
  | _ -> None

(* Pretty-printing *)

let pp_inter pp_item fmt (Inter l) =
  let open Format in
  if l = [] then fprintf fmt "_"
  else
    let pp_sep = fun fmt () -> fprintf fmt " & " in
    pp_print_list ~pp_sep pp_item fmt l

let pp_seq_item pp_set pp_rel fmt = function
  | Set l -> Format.fprintf fmt "[%a]" (pp_inter pp_set) l
  | Rel l -> pp_inter pp_rel fmt l

let pp_seq pp_set pp_rel fmt (Seq l) =
  let pp_sep = fun fmt () -> Format.fprintf fmt "; " in
  Format.pp_print_list ~pp_sep (pp_seq_item pp_set pp_rel) fmt l

let pp_union pp_item fmt (Union l) =
  let open Format in
  match l with
  | [] -> fprintf fmt "emptyset"
  | [ x ] -> fprintf fmt "%a" pp_item x
  | x :: xs ->
      fprintf fmt "@[<v 0>";
      fprintf fmt "  %a" pp_item x;
      List.iter (fun y -> fprintf fmt "@,| %a" pp_item y) xs;
      fprintf fmt "@]"

let pp_prim_rel fmt (r : prim_rel) =
  let open Format in
  match r with
  | Prim x -> fprintf fmt "%s" x
  | Fence f -> fprintf fmt "%s" (AArch64Base.pp_barrier f)

let rec pp_prim_set fmt =
  let open Format in
  function
  | Domain r -> fprintf fmt "domain(%a)" (pp_seq pp_prim_set pp_prim_rel) r
  | Range r -> fprintf fmt "range(%a)" (pp_seq pp_prim_set pp_prim_rel) r
  | Rmw s -> fprintf fmt "rmw(%a)" pp_prim_set s
  | Event { kind; direction; field; is_no_return } ->
      fprintf fmt "%s%s%s%s"
        (match kind with None -> "" | Some Exp -> "Exp" | Some Imp -> "Imp")
        (match field with
        | None | Some Plain -> ""
        | Some TTD -> "&TTD"
        | Some Tag -> "&Tag"
        | Some Instr -> "&Instr")
        (match direction with
        | M -> "&M"
        | R -> "&R"
        | W -> "&W"
        | A -> "&A"
        | L -> "&L"
        | Q -> "&Q")
        (if is_no_return then "\\NoRet" else "")
  | Fence None -> fprintf fmt "F"
  | Fence (Some f) -> fprintf fmt "%s" (AArch64Base.pp_barrier_dot f)
  | Prim x -> fprintf fmt "%s" x
  | Comp x -> fprintf fmt "~(%a)" pp_prim_set x

and pp_set_nf fmt (nf : set_nf) = pp_union (pp_inter pp_prim_set) fmt nf

and pp_rel_nf fmt (nf : rel_nf) =
  pp_union (pp_seq pp_prim_set pp_prim_rel) fmt nf

(* Symbol expansion functions *)

let find_partition (p : 'a -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go acc = function
    | [] -> None
    | x :: xs -> (
        match p x with Some y -> Some (y, acc @ xs) | None -> go (x :: acc) xs)
  in
  go [] l

let expand_domain_range (nf : rel_nf) : rel_nf =
  let can_expand (Seq seq : seq_item seq) : bool =
    let rels =
      List.filter_map (function Set _ -> None | Rel r -> Some r) seq
    in
    match rels with
    | [] -> true
    | [ Inter [ Prim ("amo" | "rmw") ] ] -> true
    | _ -> false
  in
  let expand_item : seq_item -> seq_item list = function
    | Set (Inter x) -> (
        match
          find_partition
            (function
              | Domain r when can_expand r ->
                  Some (fun rest -> [ Set (Inter rest) ] @ get_seq r)
              | Range r when can_expand r ->
                  Some (fun rest -> get_seq r @ [ Set (Inter rest) ])
              | _ -> None)
            x
        with
        | Some (k, rest) -> k rest
        | None -> [ Set (Inter x) ])
    | x -> [ x ]
  in
  map_union (fun (Seq seq) -> Seq (List.concat_map expand_item seq)) nf

let expand_trailing_po nf =
  let has_po = function
    | Rel (Inter relations) ->
        List.exists
          (fun (relation : prim_rel) ->
            match relation with Prim "po" -> true | _ -> false)
          relations
    | Set _ -> false
  in
  map_union
    (fun (Seq items) ->
      match List.rev items with
      | relation :: _ when has_po relation ->
          Seq (items @ [ Set (Inter [ Event default_event ]) ])
      | _ -> Seq items)
    nf

let expand_rmw (nf : rel_nf) : rel_nf =
  let rec leads_to_communication = function
    | Set (Inter []) :: items -> leads_to_communication items
    | item :: _ -> is_communication item
    | [] -> false
  in
  let contains_predicate =
    List.exists (function
      | Set (Inter sets) ->
          List.exists (function Domain _ | Range _ -> true | _ -> false) sets
      | Rel _ -> false)
  in
  let rec mark_each item before = function
    | [] -> []
    | (Event _ as atom) :: atoms ->
        let rest = List.rev_append before atoms in
        let write_side =
          List.exists
            (function Event { direction = W | L; _ } -> true | _ -> false)
            (atom :: rest)
        in
        let marker = Set (Inter [ Rmw atom ]) in
        let marked =
          if write_side then Seq [ marker; item ] else Seq [ item; marker ]
        in
        marked :: mark_each item (atom :: before) atoms
    | atom :: atoms -> mark_each item (atom :: before) atoms
  in
  let expand_item = function
    | Set (Inter atoms) as item -> Seq [ item ] :: mark_each item [] atoms
    | Rel _ as item -> [ Seq [ item ] ]
  in
  let rec expand_items first preserve_last previous_is_rmw = function
    | [] -> []
    | item :: items ->
        let boundary_communication =
          (first && leads_to_communication items)
          || (items = [] && preserve_last)
        in
        let adjacent_rmw =
          previous_is_rmw
          || match items with next :: _ -> is_amo_or_rmw next | [] -> false
        in
        (if boundary_communication || adjacent_rmw then [ Seq [ item ] ]
         else expand_item item)
        :: expand_items false preserve_last (is_amo_or_rmw item) items
  in
  nf
  |> map_union (fun (Seq items) ->
      if contains_predicate items then [ Seq items ]
      else
        let preserve_last =
          match List.rev items with
          | _ :: before -> leads_to_communication before
          | [] -> false
        in
        expand_items true preserve_last false items
        |> Util.List.sequence |> List.map seq_l)
  |> fun (Union seqs) -> Union (List.concat seqs)
