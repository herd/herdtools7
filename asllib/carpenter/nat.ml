(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

open QCheck2.Gen

(** Module adapted from [random_generator.Gen.Nat] *)

let split2 n =
  let+ k = int_bound n in
  (k, n - k)

(** Builds a random subset of size [size] of the interval [start, limit]. *)
let subset ~size start limit =
  let stop = limit + 1 in
  let range = stop - start in
  if not (0 <= size && size <= range) then invalid_arg "Gen.Nat.subset";
  (* The algorithm below is attributed to Floyd, see for example
     https://eyalsch.wordpress.com/2010/04/01/random-sample/
     https://math.stackexchange.com/questions/178690
  *)
  let module ISet = Set.Make (Int) in
  let rec fill set i =
    if i = stop then return set
    else
      let* pos = start -- i in
      let choice = if ISet.mem pos set then i else pos in
      fill (ISet.add choice set) (i + 1)
  in
  let+ set = fill ISet.empty (stop - size) in
  ISet.elements set

let pos_split ~size:k n =
  (* To split n into n{0}+n{1}+..+n{k-1}, we draw distinct "boundaries"
     b{-1}..b{k-1}, with b{-1}=0 and b{k-1} = n
     and the k-1 intermediate boundaries b{0}..b{k-2}
     chosen randomly distinct in [1;n-1].

     Then each n{i} is defined as b{i}-b{i-1}. *)
  let+ b = subset ~size:(k - 1) 1 (n - 1) in
  let b = Array.of_list b in
  List.init k (fun i ->
      if i = 0 then b.(0)
      else if i = k - 1 then n - b.(i - 1)
      else b.(i) - b.(i - 1))

let split ~size:k n =
  if k > n then raise (Invalid_argument "split");
  match k with
  | 0 -> pure []
  | 1 -> pure [ n ]
  | 2 ->
      let+ k, k' = split2 n in
      [ k; k' ]
  | _ ->
      let+ ns = pos_split ~size:k (n + k) in
      List.map (fun v -> v - 1) ns

let split3 n =
  split ~size:3 n >|= function
  | [ n1; n2; n3 ] -> (n1, n2, n3)
  | _ -> assert false

let list_sized_min_no_gen ?(fun_name = "list_sized_min_no_gen") min size =
  if min > size then raise (Invalid_argument fun_name);
  let* length = min -- size in
  split ~size:length size

let list_sized_min ?(fun_name = "list_sized_min") min elt size =
  let* sizes = list_sized_min_no_gen ~fun_name min size in
  List.map elt sizes |> flatten_l

let list_sized elt size = list_sized_min ~fun_name:"list_sized" 0 elt size

let list_sized_non_empty elt size =
  list_sized_min ~fun_name:"list_sized_non_empty" 1 elt size
