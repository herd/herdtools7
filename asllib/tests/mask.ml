module BV = Asllib.Bitvector
open QCheck2

(* Generators *)
let gen_bv ~size =
  let open Gen in
  let bytes = bytes_size (pure ((size / 8) + 1)) in
  map (BV.of_bytes size) bytes

let gen_mask_and_matching_bv ~size =
  let open Gen in
  let* specified = gen_bv ~size in
  let+ data = gen_bv ~size in
  (BV.mask_of_bitvector_and_specified specified data, data)

let gen_positions ~size =
  let open Gen in
  assert (size > 0);
  list_size (1 -- size) (int_bound (size - 1))

let gen_positions_size ~size_src ~size_dst =
  let open Gen in
  assert (size_src > 0);
  assert (size_dst > 0);
  list_size (pure size_src) (int_bound (size_dst - 1))

let gen_bv' =
  let open Gen in
  let* size = nat in
  gen_bv ~size

let gen_bv2' =
  let open Gen in
  let* size = nat in
  pair (gen_bv ~size) (gen_bv ~size)

let gen_mask_and_matching_bv' =
  let open Gen in
  let* size = nat in
  gen_mask_and_matching_bv ~size

let gen_mask_and_matching_bv2' =
  let open Gen in
  let* size = nat in
  pair (gen_mask_and_matching_bv ~size) (gen_mask_and_matching_bv ~size)

let gen_positions_and_mask_and_matching_bv =
  let open Gen in
  let* size = nat in
  let size = size + 1 in
  pair (gen_positions ~size) (gen_mask_and_matching_bv ~size)

let gen_positions_and_mask_and_matching_bv2 =
  let open Gen in
  let* size_dst = nat and* size_src = nat in
  let size_dst = size_dst + 1 and size_src = size_src + 1 in
  triple
    (gen_positions_size ~size_dst ~size_src)
    (gen_mask_and_matching_bv ~size:size_dst)
    (gen_mask_and_matching_bv ~size:size_src)

let gen_list_mask_and_matching_bv = Gen.list gen_mask_and_matching_bv'

(* Printers *)
let print_mask = BV.mask_to_canonical_string
let print_bv = BV.to_string
let print_bv2 = Print.pair print_bv print_bv
let print_mask_and_bv = Print.pair print_mask print_bv
let print_mask_and_bv2 = Print.pair print_mask_and_bv print_mask_and_bv
let print_positions = Print.(list int)
let print_list_mask_and_bv = Print.list print_mask_and_bv

let print_positions_and_mask_and_bv =
  Print.pair print_positions print_mask_and_bv

let print_positions_and_mask_and_bv2 =
  Print.triple print_positions print_mask_and_bv print_mask_and_bv

(* Properties *)
let lognot_is_sound (mask, bv) =
  assume (BV.matches bv mask);
  BV.matches (BV.lognot bv) (BV.mask_inverse mask)

let logand_is_sound ((m1, bv1), (m2, bv2)) =
  assume (BV.matches bv1 m1);
  assume (BV.matches bv2 m2);
  BV.matches (BV.logand bv1 bv2) (BV.mask_and m1 m2)

let logor_is_sound ((m1, bv1), (m2, bv2)) =
  assume (BV.matches bv1 m1);
  assume (BV.matches bv2 m2);
  BV.matches (BV.logor bv1 bv2) (BV.mask_or m1 m2)

let extract_slice_is_sound (positions, (m, bv)) =
  assume (BV.matches bv m);
  BV.matches (BV.extract_slice bv positions) (BV.mask_extract_slice m positions)

let write_slice_is_sound (positions, (m1, bv1), (m2, bv2)) =
  assume (BV.matches bv1 m1);
  assume (BV.matches bv2 m2);
  BV.matches
    (BV.write_slice bv1 bv2 positions)
    (BV.mask_write_slice m1 m2 positions)

let concat_is_sound mask_and_bvs =
  assume (List.for_all (fun (m, bv) -> BV.matches bv m) mask_and_bvs);
  let ms, bvs = List.split mask_and_bvs in
  BV.matches (BV.concat bvs) (BV.mask_concat ms)

let bv_not_is_involutive bv = BV.equal (BV.lognot (BV.lognot bv)) bv

let bitwise_binop_match_zop bvop zop (bv1, bv2) =
  let z1 = BV.to_z_unsigned bv1 and z2 = BV.to_z_unsigned bv2 in
  Z.equal (zop z1 z2) (bvop bv1 bv2 |> BV.to_z_unsigned)

let testsuite =
  [
    Test.make ~name:"lognot_is_sound" ~print:print_mask_and_bv
      gen_mask_and_matching_bv' lognot_is_sound;
    Test.make ~name:"logand_is_sound" ~print:print_mask_and_bv2
      gen_mask_and_matching_bv2' logand_is_sound;
    Test.make ~name:"logor_is_sound" ~print:print_mask_and_bv2
      gen_mask_and_matching_bv2' logor_is_sound;
    Test.make ~name:"extract_slice_is_sound"
      ~print:print_positions_and_mask_and_bv
      gen_positions_and_mask_and_matching_bv extract_slice_is_sound;
    Test.make ~name:"write_slice_is_sound"
      ~print:print_positions_and_mask_and_bv2
      gen_positions_and_mask_and_matching_bv2 write_slice_is_sound;
    Test.make ~name:"concat_is_sound" ~print:print_list_mask_and_bv
      gen_list_mask_and_matching_bv concat_is_sound;
    Test.make ~name:"bv_is_involutive" ~print:print_bv gen_bv'
      bv_not_is_involutive;
    Test.make ~name:"logand_matches_z" ~print:print_bv2 gen_bv2'
      (bitwise_binop_match_zop BV.logand Z.logand);
    Test.make ~name:"logor_matches_z" ~print:print_bv2 gen_bv2'
      (bitwise_binop_match_zop BV.logor Z.logor);
  ]

let () = QCheck_runner.run_tests_main testsuite
