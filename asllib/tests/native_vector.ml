open Asllib
open! Testhelpers.Helpers

module B = Native.DeterministicBackend

let int_value = B.v_of_int

let int_of_value value =
  match B.v_to_z value with
  | Some value -> Z.to_int value
  | None -> failwith "expected an integer value"

let get_int index vector = B.get_index index vector |> int_of_value

let make_vector length =
  List.init length int_value |> B.create_vector

let assert_int expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "expected %d, got %d" expected actual)

let assert_dynamic_error f =
  match f () with
  | exception Error.ASLException _ -> ()
  | exception exn -> raise exn
  | _ -> failwith "expected a dynamic error"

let set_preserves_original () =
  let original = make_vector 513 in
  let updated = B.set_index 256 (int_value 999) original in
  assert_int 256 (get_int 256 original);
  assert_int 999 (get_int 256 updated)

let small_vectors () =
  let empty = make_vector 0 in
  if B.debug_value empty <> "[]" then failwith "empty vector rendering changed";
  let original = make_vector 256 in
  let updated = B.set_index 255 (int_value 999) original in
  assert_int 255 (get_int 255 original);
  assert_int 999 (get_int 255 updated)

let chunk_boundaries () =
  let original = make_vector 513 in
  let updates = [ (0, 1000); (255, 1255); (256, 1256); (512, 1512) ] in
  let updated =
    List.fold_left
      (fun vector (index, value) -> B.set_index index (int_value value) vector)
      original updates
  in
  List.iter
    (fun (index, value) ->
      assert_int index (get_int index original);
      assert_int value (get_int index updated))
    updates;
  assert_int 254 (get_int 254 updated);
  assert_int 257 (get_int 257 updated)

let nested_values_do_not_alias () =
  let inner = make_vector 3 in
  let outer = B.create_vector [ inner; inner ] in
  let changed_inner = B.set_index 1 (int_value 42) inner in
  let changed_outer = B.set_index 0 changed_inner outer in
  assert_int 1 (B.get_index 0 outer |> B.get_index 1 |> int_of_value);
  assert_int 42 (B.get_index 0 changed_outer |> B.get_index 1 |> int_of_value);
  assert_int 1 (B.get_index 1 changed_outer |> B.get_index 1 |> int_of_value)

let printing_preserves_order () =
  let actual = make_vector 4 |> B.debug_value in
  if actual <> "[0, 1, 2, 3]" then
    failwith (Printf.sprintf "unexpected vector rendering: %S" actual)

let bounds_are_checked () =
  let vector = make_vector 3 in
  assert_dynamic_error (fun () -> B.get_index (-1) vector);
  assert_dynamic_error (fun () -> B.get_index 3 vector);
  assert_dynamic_error (fun () -> B.set_index (-1) (int_value 0) vector);
  assert_dynamic_error (fun () -> B.set_index 3 (int_value 0) vector)

let () =
  exec_tests
    [
      ("native vector update preserves original", set_preserves_original);
      ("small native vectors", small_vectors);
      ("native vector chunk boundaries", chunk_boundaries);
      ("nested native vectors do not alias", nested_values_do_not_alias);
      ("native vector printing preserves order", printing_preserves_order);
      ("native vector bounds are checked", bounds_are_checked);
    ]
