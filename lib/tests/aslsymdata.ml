open QCheck2
module BV = Asllib.Bitvector
module BVData = ASLSymData.BVData

(******************************************************************************)
(*                                 Generators                                 *)
(******************************************************************************)

(* General generators, size dependent *)
(**************************************)

module Sized = struct
  open Gen

  let bv ~size =
    let bytes = bytes_size (pure ((size / 8) + 1)) in
    map (BV.of_bytes size) bytes

  let mask data =
    let+ specified = bv ~size:(BV.length data) in
    BV.mask_of_bitvector_and_specified ~specified ~data

  let positions ~size =
    assert (size > 0);
    list_size (1 -- size) (int_bound (size - 1))

  let t =
    fix @@ fun t (data, nb_sub) ->
    let known_sub_symbolic data nb_sub =
      let nb_sub = nb_sub - 1 in
      let* size = 1 -- BV.length data in
      let* positions = positions ~size in
      let+ t = t (BV.extract_slice data positions, nb_sub) in
      (positions, (0, t))
    in
    let known_sub_symbolics =
      fix @@ fun known_sub_symbolics (data, nb_sub) ->
      if nb_sub <= 0 || BV.length data <= 0 then pure []
      else
        let* nb_sub_here = 1 -- nb_sub in
        let nb_sub' = nb_sub - nb_sub_here in
        let+ tail = known_sub_symbolics (data, nb_sub')
        and+ known_sub_symbolic = known_sub_symbolic data nb_sub_here in
        known_sub_symbolic :: tail
    in
    let size = BV.length data in
    let+ mask = mask data
    and+ known_sub_symbolics = known_sub_symbolics (data, nb_sub) in
    BVData.{ length = size; mask; known_sub_symbolics }

  let t_and_matching_bv ~size : (BVData.t * BV.t) t =
    let* data = bv ~size in
    let* nb_sub = int_bound (Int.min 100 (2 * size)) in
    let+ t = t (data, nb_sub) in
    (t, data)

  let positions_size ~size_src ~size_dst =
    assert (size_src > 0);
    assert (size_dst > 0);
    list_size (pure size_src) (int_bound (size_dst - 1))
end

(* Generators usable in tests. Non size_dependent *)
(**************************************************)

module NSized = struct
  open Gen

  let t_and_matching_bv =
    let* size = nat in
    Sized.t_and_matching_bv ~size

  let t_and_matching_bv2 =
    let* size = nat in
    pair (Sized.t_and_matching_bv ~size) (Sized.t_and_matching_bv ~size)

  let t_and_matching_bv2_diff_sizes = pair t_and_matching_bv t_and_matching_bv

  let positions_and_t_and_matching_bv =
    let* size = nat in
    let size = size + 1 in
    pair (Sized.positions ~size) (Sized.t_and_matching_bv ~size)

  let positions_and_t_and_matching_bv2 =
    let* size_dst = nat and* size_src = nat in
    let size_dst = size_dst + 1 and size_src = size_src + 1 in
    triple
      (Sized.positions_size ~size_dst ~size_src)
      (Sized.t_and_matching_bv ~size:size_src)
      (Sized.t_and_matching_bv ~size:size_dst)
end

(******************************************************************************)
(*                                  Printers                                  *)
(******************************************************************************)

module Printers = struct
  open Print

  let bv = BV.to_string
  let positions = list int
  let t = BVData.to_string
  let t_and_bv = pair t bv
  let t_and_bv2 = pair t_and_bv t_and_bv
  let positions_and_t_and_bv = pair positions t_and_bv
  let positions_and_t_and_bv2 = triple positions t_and_bv t_and_bv
end

(******************************************************************************)
(*                                 properties                                 *)
(******************************************************************************)

module Properties = struct
  let rec matches bv t =
    Int.equal (BV.length bv) t.BVData.length
    && BV.matches bv t.BVData.mask
    && Fun.flip List.for_all t.BVData.known_sub_symbolics
       @@ fun (positions, (_s, t')) ->
       (* TODO: handle s *)
       matches (BV.extract_slice bv positions) t'

  let matches' (t, bv) = matches bv t

  let logand_is_sound ((t1, bv1), (t2, bv2)) =
    assume (matches bv1 t1);
    assume (matches bv2 t2);
    matches (BV.logand bv1 bv2) (BVData.logand t1 t2)

  let logor_is_sound ((t1, bv1), (t2, bv2)) =
    assume (matches bv1 t1);
    assume (matches bv2 t2);
    matches (BV.logor bv1 bv2) (BVData.logor t1 t2)

  let lognot_is_sound (t, bv) =
    assume (matches bv t);
    matches (BV.lognot bv) (BVData.lognot t)

  let concat_is_sound ((t1, bv1), (t2, bv2)) =
    assume (matches bv1 t1);
    assume (matches bv2 t2);
    matches (BV.concat [ bv2; bv1 ]) (BVData.concat2 ~low:t1 ~high:t2)

  let extract_slices_is_sound (positions, (t, bv)) =
    assume (matches bv t);
    matches (BV.extract_slice bv positions) (BVData.extract_slice t positions)

  let write_slices_is_sound (positions, (t_src, bv_src), (t_dst, bv_dst)) =
    assume (matches bv_src t_src);
    assume (matches bv_dst t_dst);
    let fresh_val () = 0 in
    let res, _eqs =
      BVData.write_slice fresh_val ~src:t_src ~dst:t_dst positions
    in
    (* TODO: check eqs *)
    matches (BV.write_slice bv_dst bv_src positions) res
end

(******************************************************************************)
(*                                   Tests                                    *)
(******************************************************************************)

let testsuite =
  let long_factor = 100 in
  let make = Test.make ~long_factor in
  [
    make ~name:"generator is not buggy" ~print:Printers.t_and_bv ~count:5
      NSized.t_and_matching_bv Properties.matches';
    make ~name:"logand_is_sound" ~print:Printers.t_and_bv2
      NSized.t_and_matching_bv2 Properties.logand_is_sound;
    make ~name:"logor_is_sound" ~print:Printers.t_and_bv2
      NSized.t_and_matching_bv2 Properties.logor_is_sound;
    make ~name:"lognot_is_sound" ~print:Printers.t_and_bv
      NSized.t_and_matching_bv Properties.lognot_is_sound;
    make ~name:"concat_is_sound" ~print:Printers.t_and_bv2
      NSized.t_and_matching_bv2_diff_sizes Properties.concat_is_sound;
    make ~name:"extract_slices_is_sound" ~print:Printers.positions_and_t_and_bv
      NSized.positions_and_t_and_matching_bv Properties.extract_slices_is_sound;
    make ~name:"write_slices_is_sound" ~print:Printers.positions_and_t_and_bv2
      NSized.positions_and_t_and_matching_bv2 Properties.write_slices_is_sound;
  ]

(******************************************************************************)
(*                                Entrypoint                                  *)
(******************************************************************************)

let () = if true then QCheck_runner.run_tests_main testsuite
