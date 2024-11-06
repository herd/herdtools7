[@@@warning "-44-40"]

let ( <-- ) a b = (a, b)

module ISet = Set.Make (Int)
module IMap = Map.Make (Int)

let from_edges =
  let add_one_edge (nodes, succ_map) (v, succs) =
    let nodes = ISet.add v nodes in
    let nodes = List.fold_right ISet.add succs nodes in
    let succ_map =
      IMap.update v
        (function
          | None -> ISet.of_list succs |> Option.some
          | Some set -> List.fold_right ISet.add succs set |> Option.some)
        succ_map
    in
    (nodes, succ_map)
  in
  fun edges ->
    let nodes, succ_map =
      List.fold_left add_one_edge (ISet.empty, IMap.empty) edges
    in
    let succ_map =
      ISet.fold
        (fun v -> IMap.update v (function None -> Some ISet.empty | s -> s))
        nodes succ_map
    in
    (ISet.elements nodes, fun v -> IMap.find v succ_map |> ISet.elements)

(* Compatibility layer around Int. *)
module I = struct
  let hash : int -> int = Hashtbl.hash [@@warning "-32"]

  include Int
end

module TS = Asllib.TopoSort.Make (I)

let print_fold os () =
  Format.(
    printf "@[<h 2>{%a}@ @]"
      (pp_print_list ~pp_sep:pp_print_space pp_print_int)
      os)

let () =
  if false then (
    let nodes, succs = from_edges [ 1 <-- [ 2; 3 ]; 2 <-- [ 3 ] ] in
    Format.printf "@[<hov 2>Graph:@ ";
    TS.fold_strong_connected print_fold nodes succs ();
    Format.printf "@]@.")

let () =
  if false then (
    let nodes, succs =
      from_edges [ 1 <-- [ 2; 3 ]; 2 <-- [ 3 ]; 3 <-- [ 2 ] ]
    in
    Format.printf "@[<hov 2>Graph:@ ";
    TS.fold_strong_connected print_fold nodes succs ();
    Format.printf "@]@.")

let () =
  if false then (
    let nodes, succs = from_edges [ 0 <-- [ 1 ] ] in
    Format.printf "@[<hov 2>Graph:@ ";
    TS.fold_strong_connected print_fold nodes succs ();
    Format.printf "@]@.";
    assert (TS.Properties.order_respected (nodes, succs)))

let graph n =
  let open QCheck in
  let node = int_bound n in
  let singleton v = [ v ]
  and unsingleton = function [ v ] -> v | _ -> assert false in
  let edge = pair node (map ~rev:unsingleton singleton node) in
  list_of_size Gen.(0 -- (n * n)) edge

let test ?short count n =
  let long_factor = match short with Some true -> 1 | _ -> 10 in
  QCheck.Test.make ~count ~long_factor
    ~name:(Printf.sprintf "order respected on graphs of size %d" n) (graph n)
    (fun edges -> TS.Properties.order_respected (from_edges edges))

let testsuite =
  let short = true in
  [
    test ~short 10 2;
    test ~short 10 1;
    test ~short 20 3;
    test ~short 20 4;
    test ~short 100 5;
    test 200 6;
    test 1000 7;
    test 4000 8;
    test 10000 9;
    test 40000 10;
  ]

let () = QCheck_runner.run_tests_main testsuite
