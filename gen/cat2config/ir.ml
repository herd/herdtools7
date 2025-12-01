[@@@warning "-40-42"]

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

let union_flat_map (f : 'a -> 'b union) (Union u : 'a union) : 'b union =
  Union (Util.List.concat_map (fun x -> get_union (f x)) u)

let union_l : 'a union list -> 'a union =
 fun l -> List.fold_right union l (Union [])

let seq (s1 : 'a seq) (s2 : 'a seq) : 'a seq =
  match (s1, s2) with Seq s1, Seq s2 -> Seq (List.append s1 s2)

let seq_l : 'a seq list -> 'a seq = function
  | [] -> raise (Invalid_argument "Sequence on empty list")
  | x :: xs -> List.fold_left seq x xs

let seq_flat_map (f : 'a -> 'a seq union) (Seq seq : 'a seq) : 'a seq union =
  seq
  |> List.map (fun s -> get_union (f s))
  |> Util.List.sequence |> List.map seq_l
  |> fun l -> Union l

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

module A = AArch64Arch_gen.Make (AArch64Arch_gen.Config)
module E = Edge.Make (Edge.Config) (A : Fence.S)

type fence = AArch64Base.barrier

(* type edge = { edge : E.tedge; pprinted : string } *)
type prim_edge =
  | Po
  | Co
  | Fr
  | Rf
  | Fence of fence
  | Dp of A.D.dp
  | PickDp of A.D.dp
  | Amo
  | Rmw
  | LxSx

type prim_rel = Loc | Ext | Edge of prim_edge

type prim_set =
  | Domain of seq_item seq
  | Range of seq_item seq
  | Fence of fence option
  | Atom of A.atom_acc
  | Dir of Code.dir
  | M
  | Comp of prim_set

and seq_item = Set of prim_set inter | Rel of prim_rel inter
and set_nf = prim_set inter union
and rel_nf = seq_item seq union

(* Compression *)

let compress_seq (l : seq_item list) : seq_item list =
  l
  |> List.fold_left
       (fun acc x ->
         match (acc, x) with
         | [], x -> [ x ]
         | Set s :: acc, Set s' -> Set (inter s s') :: acc
         | Rel r :: acc, Set s -> Set s :: Rel r :: acc
         | acc, Rel (Inter []) -> acc
         | acc, Rel r -> Rel r :: acc)
       []
  |> List.rev

let compress : seq_item seq union -> seq_item seq union =
  let rec compress_set : prim_set -> prim_set = function
    | Domain (Seq r) -> Domain (Seq (compress_seq r))
    | Range (Seq r) -> Range (Seq (compress_seq r))
    | Comp s -> Comp (compress_set s)
    | s -> s
  in
  map_union (fun (Seq s) ->
      let s = compress_seq s in
      let s =
        List.map
          (function
            | Set (Inter set) -> Set (Inter (List.map compress_set set))
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
let prim_sets : prim_set list -> set_nf = fun p -> Union [ Inter p ]
let prim_rel : prim_rel -> rel_nf = fun p -> Union [ Seq [ Rel (Inter [ p ]) ] ]

let prim_set_comp : prim_set -> prim_set inter union = function
  | Fence None -> prim_set M
  | M -> prim_set (Fence None)
  | Dir R -> union_l [ prim_set (Dir W); prim_set (Fence None) ]
  | Dir W -> union_l [ prim_set (Dir R); prim_set (Fence None) ]
  | Comp s -> prim_set s
  | s -> prim_set (Comp s)

let empty_set : set_nf = Union []
let empty_rel : rel_nf = Union []
let domain : rel_nf -> set_nf = map_union (fun s -> Inter [ Domain s ])
let range : rel_nf -> set_nf = map_union (fun s -> Inter [ Range s ])
let fencerel (f : fence) : rel_nf = prim_rel (Edge (Fence f))
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

let set_comp : set_nf -> set_nf = comp prim_set_comp
let set_diff : set_nf -> set_nf -> set_nf = fun a b -> set_inter a (set_comp b)

let find_fence : set_nf -> fence option = function
  | Union [ Inter [ Fence None ] ] -> Some (DSB (SY, FULL))
  | Union [ Inter [ Fence (Some f) ] ] -> Some f
  | _ -> None

(* Parsing string identifiers into IR structures *)

(*
  For now, we only deal with:

  - explicit effects
  - memory effects
  - fence effects

  Therefore we interpret everything else outside these three classes as
  the empty set.
*)
let parse_set_id (s : string) : set_nf option =
  let try_other_prims () =
    match s with
    | "Exp" -> Some universe_set
    | "NExp" -> Some empty_set
    | "Imp" -> Some empty_set
    | "R" -> Some (prim_set (Dir Code.R))
    | "W" -> Some (prim_set (Dir Code.W))
    | "M" -> Some (prim_set M)
    | "A" -> Some (prim_set (Atom (Acq None)))
    | "L" -> Some (prim_set (Atom (Rel None)))
    | "Q" -> Some (prim_set (Atom (AcqPc None)))
    | "F" -> Some (prim_set (Fence None))
    | "TTD" -> Some empty_set
    | "Instr" -> Some empty_set
    | "emptyset" -> Some empty_set
    | "NoRet" -> Some empty_set
    | "T" -> Some empty_set
    | "dmb.sy" -> Some (prim_set (Fence (Some (AArch64Base.DMB (SY, FULL)))))
    | "dmb.full" -> Some (prim_set (Fence (Some (AArch64Base.DMB (SY, FULL)))))
    | "dmb.st" -> Some (prim_set (Fence (Some (AArch64Base.DMB (SY, ST)))))
    | "dmb.ld" -> Some (prim_set (Fence (Some (AArch64Base.DMB (SY, LD)))))
    | "dsb.sy" -> Some (prim_set (Fence (Some (AArch64Base.DSB (SY, FULL)))))
    | "dsb.full" -> Some (prim_set (Fence (Some (AArch64Base.DSB (SY, FULL)))))
    | "dsb.st" -> Some (prim_set (Fence (Some (AArch64Base.DSB (SY, ST)))))
    | "dsb.ld" -> Some (prim_set (Fence (Some (AArch64Base.DSB (SY, LD)))))
    | _ -> None
  in
  let try_fences () =
    Option.map (fun b -> prim_set (Fence (Some b))) (Diy_utils.parse_barrier s)
  in
  Util.Option.choice [ try_other_prims (); try_fences () ]

let parse_rel_id (s : string) : rel_nf option =
  let poswr =
    rel_seq_l
      [
        to_id (prim_set (Dir W));
        prim_rel_inter_l [ Edge Po; Loc ];
        to_id (prim_set (Dir R));
      ]
  in
  let pick_basic_dep = poswr in
  let pick_addr_dep = prim_rel (Edge (PickDp A.D.ADDR)) in
  let pick_data_dep = prim_rel (Edge (PickDp A.D.DATA)) in
  let pick_ctrl_dep = prim_rel (Edge (PickDp A.D.CTRL)) in
  match s with
  | "po" -> Some (prim_rel (Edge Po))
  | "loc" -> Some (prim_rel Loc)
  | "co" -> Some (prim_rel (Edge Co))
  | "fr" -> Some (prim_rel (Edge Fr))
  | "rf" -> Some (prim_rel (Edge Rf))
  | "ext" -> Some (prim_rel Ext)
  | "addr" -> Some (prim_rel (Edge (Dp ADDR)))
  | "data" -> Some (prim_rel (Edge (Dp DATA)))
  | "ctrl" -> Some (prim_rel (Edge (Dp CTRL)))
  | "rmw" -> Some (prim_rel (Edge Rmw))
  | "amo" -> Some (prim_rel (Edge Amo))
  | "lxsx" -> Some (prim_rel (Edge LxSx))
  | "lrs" -> Some poswr
  | "pick-basic-dep" -> Some pick_basic_dep
  | "pick-addr-dep" -> Some pick_addr_dep
  | "pick-data-dep" -> Some pick_data_dep
  | "pick-ctrl-dep" -> Some pick_ctrl_dep
  | "pick-dep" ->
      Some
        (rel_union_l
           [ pick_basic_dep; pick_addr_dep; pick_data_dep; pick_ctrl_dep ])
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

let pp_prim_edge fmt =
  let open Format in
  function
  | Po -> fprintf fmt "po"
  | Co -> fprintf fmt "co"
  | Fr -> fprintf fmt "fr"
  | Rf -> fprintf fmt "rf"
  | Amo -> fprintf fmt "amo"
  | LxSx -> fprintf fmt "lxsx"
  | Rmw -> fprintf fmt "rmw"
  | Fence f -> fprintf fmt "%s" (AArch64Base.pp_barrier f)
  | Dp dp -> fprintf fmt "%s" (Diy_utils.pp_dp dp)
  | PickDp dp -> fprintf fmt "pick-%s-dep" (Diy_utils.pp_dp dp)

let pp_prim_rel fmt =
  let open Format in
  function
  | Loc -> fprintf fmt "loc"
  | Ext -> fprintf fmt "ext"
  | Edge e -> fprintf fmt "%a" pp_prim_edge e

let rec pp_prim_set fmt =
  let open Format in
  function
  | Domain r -> fprintf fmt "domain(%a)" (pp_seq pp_prim_set pp_prim_rel) r
  | Range r -> fprintf fmt "range(%a)" (pp_seq pp_prim_set pp_prim_rel) r
  | Fence None -> fprintf fmt "F"
  | Fence (Some f) -> fprintf fmt "%s" (Diy_utils.pp_barrier f)
  | Dir dir -> fprintf fmt "%s" (Code.pp_dir dir)
  | M -> fprintf fmt "M"
  | Atom a -> fprintf fmt "%s" (A.pp_atom_acc a)
  | Comp x -> fprintf fmt "~(%a)" pp_prim_set x

and pp_set_nf fmt (nf : set_nf) = pp_union (pp_inter pp_prim_set) fmt nf

and pp_rel_nf fmt (nf : rel_nf) =
  pp_union (pp_seq pp_prim_set pp_prim_rel) fmt nf

let find_partition (p : 'a -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go acc = function
    | [] -> None
    | x :: xs -> (
        match p x with Some y -> Some (y, acc @ xs) | None -> go (x :: acc) xs)
  in
  go [] l

let mem_partition (elem : 'a) (l : 'a list) : 'a list option =
  find_partition (fun x -> if x = elem then Some () else None) l
  |> Option.map snd

let expand_domain_range (nf : rel_nf) : rel_nf =
  let can_expand (Seq seq : seq_item seq) : bool =
    let rels =
      List.filter_map (function Set _ -> None | Rel r -> Some r) seq
    in
    match rels with [] -> true | [ Inter [ Edge Amo ] ] -> true | _ -> false
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
  map_union (fun (Seq seq) -> Seq (Util.List.concat_map expand_item seq)) nf

let expand_acq_rel (nf : rel_nf) : rel_nf =
  let mem = to_id (prim_set M) in
  let amo = prim_rel (Edge Amo) in
  let atom_a = Atom (Acq None) in
  let atom_q = Atom (AcqPc None) in
  let atom_l = Atom (Rel None) in
  let amo_ap = rel_seq_l [ to_id (prim_set atom_a); amo; mem ] in
  let amo_qp = rel_seq_l [ to_id (prim_set atom_q); amo; mem ] in
  let amo_pl = rel_seq_l [ mem; amo; to_id (prim_set atom_l) ] in
  nf
  |> union_flat_map
       (seq_flat_map (function
         | Set (Inter x) ->
             let s = Union [ Seq [ Set (Inter x) ] ] in
             let s =
               match mem_partition atom_a x with
               | Some rest ->
                   union s (to_id (set_inter (prim_sets rest) (domain amo_ap)))
               | None -> s
             in
             let s =
               match mem_partition atom_q x with
               | Some rest ->
                   union s (to_id (set_inter (prim_sets rest) (domain amo_qp)))
               | None -> s
             in
             let s =
               match mem_partition atom_l x with
               | Some rest ->
                   union s (to_id (set_inter (prim_sets rest) (range amo_pl)))
               | None -> s
             in
             s
         | Rel r -> Union [ Seq [ Rel r ] ]))
