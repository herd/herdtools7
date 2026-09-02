open Asllib

module B = Native.DeterministicBackend

let parse_size () =
  if Array.length Sys.argv = 1 then 8192
  else if Array.length Sys.argv = 2 then int_of_string Sys.argv.(1)
  else failwith "usage: native_vector_bench [size]"

let () =
  let size = parse_size () in
  if size <= 0 then invalid_arg "size must be positive";
  let vector =
    ref (List.init size B.v_of_int |> B.create_vector)
  in
  let read_started = Unix.gettimeofday () in
  let initial_sum = ref Z.zero in
  for index = 0 to size - 1 do
    match B.get_index index !vector |> B.v_to_z with
    | Some value -> initial_sum := Z.add !initial_sum value
    | None -> failwith "expected an integer value"
  done;
  let read_elapsed = Unix.gettimeofday () -. read_started in
  let write_started = Unix.gettimeofday () in
  for index = 0 to size - 1 do
    vector := B.set_index index (B.v_of_int (index + 1)) !vector
  done;
  let write_elapsed = Unix.gettimeofday () -. write_started in
  let checksum =
    [ 0; size / 2; size - 1 ]
    |> List.fold_left
         (fun sum index ->
           match B.get_index index !vector |> B.v_to_z with
           | Some value -> Z.add sum value
           | None -> failwith "expected an integer value")
         Z.zero
  in
  Printf.printf
    "size=%d reads=%d read_seconds=%.9f initial_sum=%s writes=%d \
     write_seconds=%.9f checksum=%s\n%!"
    size size read_elapsed (Z.to_string !initial_sum) size write_elapsed
    (Z.to_string checksum)
