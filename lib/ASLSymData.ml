module BVData = struct
  module BV = Asllib.Bitvector

  type slice = int list

  type t = {
    length : int;
    mask : BV.mask;
    known_sub_symbolics : (slice * int * t) list;
  }

  let _positions_disjoint li1 li2 =
    List.for_all (fun i -> List.for_all (fun j -> not (Int.equal i j)) li2) li1

  let _positions_equal = Misc.list_eq Int.equal

  let _shift_known_sub_symbolics length =
    List.map @@ fun (positions, s, t) ->
    (List.map (( + ) length) positions, s, t)

  let extract_slice t positions =
    let mask = BV.mask_extract_slice t.mask positions
    and length = List.length positions
    and known_sub_symbolics =
      (* TODO: filter and shift known sub symbolics *)
      []
    in
    { length; mask; known_sub_symbolics }

  let find_sub_symbolic positions t =
    List.find_opt
      (fun (positions2, _, _) -> _positions_equal positions2 positions)
      t.known_sub_symbolics

  let _filter_symbolics_by_positions f =
    List.filter (fun (positions, _, _) -> f positions)

  let write_slice ~src ?src_symb ~dst positions =
    let mask = BV.mask_write_slice dst.mask src.mask positions
    and length = dst.length
    and known_sub_symbolics =
      let known_sub_symbolics =
        _filter_symbolics_by_positions
          (_positions_disjoint positions)
          dst.known_sub_symbolics
      in
      match src_symb with
      | None -> known_sub_symbolics
      | Some s -> (positions, s, src) :: known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }

  let of_bitvector bv =
    let length = BV.length bv
    and mask = BV.mask_of_bitvector bv
    and known_sub_symbolics = [] in
    { length; mask; known_sub_symbolics }

  let rec equal_opt t1 t2 =
    if not (BV.mask_can_be_equal t1.mask t2.mask) then Some false
    else
      let undetermined_positions =
        BV.mask_undetermined_positions2 t1.mask t2.mask
      in
      let ( let* ) = Option.bind in
      let* _, s3, t3 = find_sub_symbolic undetermined_positions t1 in
      let* _, s4, t4 = find_sub_symbolic undetermined_positions t2 in
      if Int.equal s3 s4 then Some true else equal_opt t3 t4

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

  let pp t = Printf.sprintf "(with length %d)" t.length

  let concat2 t1 t2 =
    let length = t1.length + t2.length
    and mask = BV.mask_concat [ t2.mask; t1.mask ]
    and known_sub_symbolics =
      t2.known_sub_symbolics
      @ _shift_known_sub_symbolics t2.length t1.known_sub_symbolics
    in
    { length; mask; known_sub_symbolics }
end

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
      Bitvector (BVData.concat2 bv_data1 bv_data2)

let concat = function
  | [] -> raise (Invalid_argument "ASLSymData.concat")
  | [ x ] -> x
  | h :: t ->
      List.fold_left concat2 h t

let default = NoData
let pp = function NoData -> "" | Bitvector bv_data -> BVData.pp bv_data
