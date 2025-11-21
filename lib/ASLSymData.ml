(** Module abstracting ASL bitvector types for use in symbolic interpretation.
*)

let pp_comma_sep fmt () = Format.fprintf fmt ",@ "

(******************************************************************************)
(*                            slice representation                            *)
(******************************************************************************)

module Positions = struct
  type t = int list

  let length = List.length
  let equal = Misc.list_eq Int.equal
  let compare = Misc.list_compare Int.compare

  let merge_continuous =
    let rec loop acc start prev = function
      | [] -> List.rev ((start, prev) :: acc)
      | cur :: tail ->
          if prev - 1 == cur then loop acc start cur tail
          else
            let acc = (start, prev) :: acc in
            loop acc cur cur tail
    in
    function [] -> [] | h :: t -> loop [] h h t

  let pp fmt =
    let open Format in
    let pp_elt fmt (x, y) =
      if Int.equal x y then pp_print_int fmt x else fprintf fmt "%d..%d" x y
    in
    fun t ->
      let merged = merge_continuous t in
      fprintf fmt "@[<hov>[%a]@]"
        (pp_print_list ~pp_sep:pp_comma_sep pp_elt)
        merged

  let disjoint li1 li2 =
    List.for_all (fun i -> List.for_all (fun j -> not (Int.equal i j)) li2) li1

  let shift i = List.map (Int.add i)

  let rec is_included p1 p2 =
    match (p1, p2) with
    | [], _ -> true
    | _, [] -> false
    | x1 :: t1, x2 :: t2 ->
        if Int.equal x1 x2 then is_included t1 t2 else is_included p1 t2

  (* Here ps = positions *)
  let try_compose =
    let exception Doublon in
    let make_map pos =
      let l = List.length pos - 1 in
      List.mapi (fun i p -> (p, l - i)) pos |> IntMap.from_bindings
    in
    let check_doublons pos map =
      let l = List.length pos - 1 in
      List.iteri
        (fun i p -> if IntMap.find p map != l - i then raise Doublon)
        pos
    in
    fun ~ps_map ~ps_dst ->
      let map = make_map ps_map in
      try
        check_doublons ps_map map;
        Some (List.map (fun p -> IntMap.find p map) ps_dst)
      with Doublon | Not_found -> None
end

module PositionsMap = struct
  type 'a t = (Positions.t * 'a) list

  let pp pp_elt fmt =
    let open Format in
    let pp_list_elt fmt (positions, elt) =
      fprintf fmt "@[<2>at %a:@ %a@]" Positions.pp positions pp_elt elt
    in
    fprintf fmt "@[[%a]@]" (pp_print_list ~pp_sep:pp_comma_sep pp_list_elt)

  let exists' t f = List.exists f t
  let is_empty = function [] -> true | _ :: _ -> false
end

(******************************************************************************)
(*                          bitvector representation                          *)
(******************************************************************************)

module BVData = struct
  module BV = Asllib.Bitvector

  type t = {
    length : int;  (** The length of given bitvector *)
    mask : BV.mask;  (** Stores the fixed bits *)
    known_sub_symbolics : sub PositionsMap.t;
        (** Maps part of this bitvector to other bitvectors. *)
  }
  (** Represents an abstraction of a bitvector. *)

  and sub = int * t
  (** Represents another symbolic bitvector, by its associated variable index
      and its known symbolic data. *)

  let rec pp_content fmt t =
    let full_mask = t.length <= 16 in
    let open Format in
    let pp_sub fmt (s, t) = fprintf fmt "@[<2>S%d@ %a@]" s pp_content t in
    let pp_mask fmt t =
      if BV.(is_zeros (mask_specified t.mask)) then
        pp_print_string fmt "no fixed bits"
      else if full_mask then
        fprintf fmt "mask: %s" (BV.mask_to_canonical_string t.mask)
      else
        fprintf fmt "fixed_bits: %s"
          (BV.to_string_hexa (BV.mask_specified t.mask))
    and pp_sub_symbs fmt t =
      if PositionsMap.is_empty t.known_sub_symbolics then
        fprintf fmt "no known sub symbolics"
      else PositionsMap.pp pp_sub fmt t.known_sub_symbolics
    and pp_length fmt t =
      if full_mask then () else fprintf fmt "with length %d,@ " t.length
    in
    fprintf fmt "(%a%a,@ and %a)" pp_length t pp_mask t pp_sub_symbs t

  let pp fmt t = Format.fprintf fmt "@[<2>%a@]" pp_content t
  let to_string t = Format.asprintf "%a" pp t

  let of_mask mask =
    { length = BV.mask_length mask; mask; known_sub_symbolics = [] }

  let of_mask_string s = BV.mask_of_string s |> of_mask
  let of_bitvector bv = BV.mask_of_bitvector bv |> of_mask

  let extract_slice t positions =
    if not (List.for_all (fun p -> 0 <= p && p < t.length) positions) then
      raise (Invalid_argument "ASLSymData.extract_slice");
    let mask = BV.mask_extract_slice t.mask positions
    and length = Positions.length positions
    and known_sub_symbolics =
      let extract_one (p, (s, t')) =
        match Positions.try_compose ~ps_map:positions ~ps_dst:p with
        | None -> None
        | Some p_res -> Some (p_res, (s, t'))
      in
      List.filter_map extract_one t.known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }

  let find_sub_symbolic positions t =
    List.find_opt
      (fun (p, _) -> Positions.equal p positions)
      t.known_sub_symbolics

  let _filter_symbolics_by_positions f =
    List.filter (fun (positions, _) -> f positions)

  let _partition_symbolics_by_positions f =
    List.partition (fun (positions, _) -> f positions)

  let rec write_slice fresh_val ~src ?src_symb ~dst positions =
    if List.compare_length_with positions src.length != 0 then
      raise (Invalid_argument "ASLSymData.write_slice");
    let mask = BV.mask_write_slice dst.mask src.mask positions
    and length = dst.length
    and known_sub_symbolics, eqs =
      let unchanged, edited =
        _partition_symbolics_by_positions
          (Positions.disjoint positions)
          dst.known_sub_symbolics
      in
      let known_sub_symbolics =
        match src_symb with
        | None -> unchanged
        | Some s -> (positions, (s, src)) :: unchanged
      in
      let edit_one (p, (s, t)) =
        match Positions.try_compose ~ps_map:p ~ps_dst:positions with
        | None -> None
        | Some p_res ->
            let new_s = fresh_val () in
            let sub', eqs = write_slice fresh_val ~src ?src_symb ~dst:t p_res
            and eq = (new_s, p_res, s) in
            Some ((p, (new_s, sub')), eq :: eqs)
      in
      List.fold_left
        (fun (edited, eqs) sub ->
          match edit_one sub with
          | Some (sub', eqs') -> (sub' :: edited, eqs' @ eqs)
          | None -> (edited, eqs))
        (known_sub_symbolics, []) edited
    in
    ({ length; mask; known_sub_symbolics }, eqs)

  let equal_opt =
    let exception IsDifferent in
    let rec equal t1 t2 =
      if not (BV.mask_can_be_equal t1.mask t2.mask) then raise IsDifferent;
      let undetermined_positions =
        BV.mask_undetermined_positions2 t1.mask t2.mask
      in
      let restrictied_sub_symolics1 =
        _filter_symbolics_by_positions
          (Positions.is_included undetermined_positions)
          t1.known_sub_symbolics
      and restrictied_sub_symolics2 =
        _filter_symbolics_by_positions
          (Positions.is_included undetermined_positions)
          t2.known_sub_symbolics
      in
      PositionsMap.exists' restrictied_sub_symolics1 @@ fun (p3, (s3, t3)) ->
      PositionsMap.exists' restrictied_sub_symolics2 @@ fun (p4, (s4, t4)) ->
      Positions.equal p3 p4 && (Int.equal s3 s4 || equal t3 t4)
    in
    fun t1 t2 ->
      try if equal t1 t2 then Some true else None
      with IsDifferent -> Some false

  let logand t1 t2 =
    if t1.length != t2.length then
      Warn.fatal "Different length passed to logand.";
    let length = t1.length
    and mask = BV.mask_and t1.mask t2.mask
    and known_sub_symbolics =
      _filter_symbolics_by_positions
        (List.for_all (BV.is_set_at (BV.mask_set t2.mask)))
        t1.known_sub_symbolics
      @ _filter_symbolics_by_positions
          (List.for_all (BV.is_set_at (BV.mask_set t1.mask)))
          t2.known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }

  let logor t1 t2 =
    if t1.length != t2.length then
      Warn.fatal "Different length passed to logor.";
    let length = t1.length
    and mask = BV.mask_or t1.mask t2.mask
    and known_sub_symbolics =
      _filter_symbolics_by_positions
        (List.for_all (BV.is_set_at (BV.mask_unset t2.mask)))
        t1.known_sub_symbolics
      @ _filter_symbolics_by_positions
          (List.for_all (BV.is_set_at (BV.mask_unset t1.mask)))
          t2.known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }

  let lognot t =
    let length = t.length
    and mask = BV.mask_inverse t.mask
    and known_sub_symbolics = [] in
    { length; mask; known_sub_symbolics }

  let merge t1 t2 =
    if t1.length != t2.length then
      Warn.fatal "Different length passed to merge.";
    let length = t1.length
    and mask = BV.mask_intersection t1.mask t2.mask
    and known_sub_symbolics = t1.known_sub_symbolics @ t2.known_sub_symbolics in
    { length; mask; known_sub_symbolics }

  let is_fully_specified t = BV.mask_is_fully_specified t.mask
  let length t = t.length

  let to_fully_determined_opt t =
    if BV.mask_is_fully_specified t.mask then Some (BV.mask_set t.mask)
    else None

  let full_unspecified length =
    let mask = BV.mask_full_unspecified length and known_sub_symbolics = [] in
    { length; mask; known_sub_symbolics }

  let _shift_known_sub_symbolics length =
    List.map @@ fun (positions, x) -> (Positions.shift length positions, x)

  let concat2 ~low ~high =
    let length = low.length + high.length
    and mask = BV.mask_concat [ high.mask; low.mask ]
    and known_sub_symbolics =
      low.known_sub_symbolics
      @ _shift_known_sub_symbolics low.length high.known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }

  let pte_bv_data length =
    let mask =
      BV.build_mask
        ~unset:
          [
            (if length = 128 then 127 else 54 (* XPN *));
            (if length = 128 then 114 else 53 (* UXPN *));
            (if length = 128 then 113 else 50 (* GP *));
            11 (* res0 ? *);
            5 (* NS *);
            4;
            3;
            2 (* MemAttrs *);
            1 (* TTD: Leaf *);
          ]
        ~set:[ 9; 8 (* Shareability: ISH *) ]
        length
    and known_sub_symbolics = [] in
    { length; mask; known_sub_symbolics }
end

(******************************************************************************)
(*                     general ASL literal representation                     *)
(******************************************************************************)

type t = NoData | Bitvector of BVData.t

let equal_opt t1 t2 =
  match (t1, t2) with
  | NoData, _ | _, NoData -> None
  | Bitvector bv_data1, Bitvector bv_data2 -> BVData.equal_opt bv_data1 bv_data2

let logand t1 t2 =
  match (t1, t2) with
  | NoData, _ | _, NoData -> NoData
  | Bitvector bv_data1, Bitvector bv_data2 ->
      Bitvector (BVData.logand bv_data1 bv_data2)

let logor t1 t2 =
  match (t1, t2) with
  | NoData, _ | _, NoData -> NoData
  | Bitvector bv_data1, Bitvector bv_data2 ->
      Bitvector (BVData.logor bv_data1 bv_data2)

let lognot = function
  | NoData -> NoData
  | Bitvector bv_data -> Bitvector (BVData.lognot bv_data)

let merge t1 t2 =
  match (t1, t2) with
  | NoData, t | t, NoData -> t
  | Bitvector bv_data1, Bitvector bv_data2 ->
      Bitvector (BVData.merge bv_data1 bv_data2)

let full_unspecified length = Bitvector (BVData.full_unspecified length)

let get_length = function
  | NoData -> None
  | Bitvector bv_data -> Some (BVData.length bv_data)

let concat2 t1 t2 =
  match (t1, t2) with
  | NoData, _ | _, NoData -> NoData
  | Bitvector bv_data1, Bitvector bv_data2 ->
      Bitvector (BVData.concat2 ~high:bv_data1 ~low:bv_data2)

let concat = function
  | [] -> raise (Invalid_argument "ASLSymData.concat")
  | [ x ] -> x
  | h :: t -> List.fold_left concat2 h t

let default = NoData
let pp = function NoData -> "" | Bitvector bv_data -> BVData.to_string bv_data
let pte_sdata length = Bitvector (BVData.pte_bv_data length)
